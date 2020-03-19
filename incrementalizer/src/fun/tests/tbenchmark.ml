(* open Core_bench *)
open Batteries
open Benchmark
(* open Benchmark2 *)

module OriginalFunAlgorithm = Original.TypeAlgorithm(FunSpecification.FunSpecification)
module IncrementalFunAlgorithm = Incrementalizer.TypeAlgorithm(FunSpecification.FunSpecification)

open FunSpecification.FunSpecification

open Generator
open VarSet

let rec nodecount e = match e with
  | Unit(annot)
  | Bool(_, annot)
  | Int(_, annot)
  | Float(_, annot)
  | Var(_, annot) -> 1
  | Not(e1, annot)
  | Neg(e1, annot)
  | FNeg(e1, annot) -> 1 + nodecount e1
  | IBop(_, e1, e2, annot)
  | FBop(_, e1, e2, annot)
  | Rel(_, e1, e2, annot) -> 1 + nodecount e1 + nodecount e2
  | If(e1, e2, e3, annot) -> 1 + nodecount e1 + nodecount e2 + nodecount e3
  | Let(_, e1, e2, annot) -> 2 + nodecount e1 + nodecount e2 (* curr node + x *)
  | LetRec ({ name = _; args = yts; body = e1 }, e2, annot) -> 2 + (List.length yts) + nodecount e1 + nodecount e2
  | App (e1, es, annot) -> 1 + (List.fold_left (+) 0 (List.map nodecount (e1::es)))
  | Tuple(es, annot) -> 1 + (List.fold_left (+) 0 (List.map nodecount es))
  | LetTuple(xs, e1, e2, annot) -> 1 + List.length xs + nodecount e1 + nodecount e2
  | Array(e1, e2, annot)
  | Get (e1, e2, annot) -> nodecount e1 + nodecount e2
  | Put (e1, e2, e3, annot) -> nodecount e1 + nodecount e2 + nodecount e3

(*
  Very simple cleanup that removes the samples whose rate is not within a sigma interval from avg

  Functions comp_rates and cpu_process taken from Benchmark library.
*)
let comp_rates cpu (name, bm) =
  let rec loop n m s = function
    | [] -> (name, n, m, s)
    | b :: tl ->
        let rate = Int64.to_float b.iters /. cpu b in
        let n' = n + 1 in
        let m' = m +. (rate -. m) /. (float n') in
        let s' = s +. (rate -. m) *. (rate -. m') in
        loop n' m' s' tl in
  match bm with
  | [] -> (name, 0, nan, 0.) (* NaN used for no-data *)
  | b :: tl -> loop 1 (Int64.to_float b.iters /. (cpu b +. 1e-15)) 0. tl

let cpu_process b = b.utime +. b.stime

let cleanup (res : (string * Benchmark.t list) list) : (string * Benchmark.t list) list =
  let cpu = cpu_process in
  let not_outlier name avg sd b = (
    let rate = Int64.to_float b.iters /. cpu b in
    let min, max = avg-.sd, avg+.sd in
      Printf.printf "%s - #%Ld, avg=%f, sd=%f. %f <= %f <= %f? %b\n" name b.iters avg sd min rate max (min <= rate && rate <= max);
      min <= rate && rate <= max
  ) in
  let stats = List.map (comp_rates cpu) res in
    List.map (fun (name, bm) ->
      let (_, iter, avg, remsq) = List.find (fun (n, _, _, _) -> String.equal n name) stats in
      let sd = sqrt (remsq /. float(iter)) in
      let cleaned = List.filter (not_outlier name avg sd) bm in
        Printf.printf "Keeping %d/%d samples.\n" (List.length cleaned) (List.length bm);
        (name, cleaned)
      ) res

let op o a b =
  assert(a.iters = b.iters);
  { wall = o a.wall b.wall;       utime = o a.utime b.utime;
    stime = o a.stime b.stime;    cutime = o a.cutime b.cutime;
    cstime = o a.cstime b.cstime; iters = a.iters }

let scalar_op o a n =
  { wall = o a.wall (Int64.to_float n);       utime = o a.utime (Int64.to_float n) ;
    stime = o a.stime (Int64.to_float n) ;    cutime = o a.cutime (Int64.to_float n) ;
    cstime = o a.cstime (Int64.to_float n); iters = a.iters }

let sub = op (-.)
let add = op (+.)
let smul = scalar_op ( *. )
let sdiv = scalar_op ( /. )
let null_t i =
  { wall = 0.; utime = 0.; stime = 0.; cutime = 0.; cstime = 0.; iters = i }

let scale_to_iter iter a = { (sdiv (smul a iter) a.iters) with iters = iter }

let compute_avg iter bm =
  let len = List.length bm in
  let scaled = List.map (scale_to_iter iter) bm in
  let sum = List.fold_left add (null_t iter) scaled in
    sdiv sum (Int64.of_int len)

let rm_setup final_name tot_name setup_name bm_list =
  let _, tot_bm = List.find (fun (name, _) -> String.equal tot_name name) bm_list in
  let _, setup_bm = List.find (fun (name, _) -> String.equal setup_name name) bm_list in
  let max = List.max (List.map (fun bm -> bm.iters) tot_bm) in
  let rl = List.remove_assoc tot_name (List.remove_assoc setup_name bm_list) in
  let tot_avg, setup_avg = (compute_avg max tot_bm), (compute_avg max setup_bm) in
    (final_name, [sub tot_avg setup_avg])::rl

let gen_list min max next =
  let rec gen_aux curr =
    if curr >= max then [max] else curr :: (gen_aux (next curr))
  in gen_aux min

let rec cartesian a b = match b with
| [] -> []
| be :: bs ->  (List.map (fun ae -> (be, ae)) a) @ (cartesian a bs)

let print_res csv res repeat time fv_c depth inv_depth =
  let res = rm_setup "incr" "setup+incr" "setup" res in
  if csv then
    let stats = List.map (comp_rates cpu_process) res in
    List.iter
      (fun (name, _, avg, _) -> Printf.eprintf "%s, %d, %d, %d, %f\n" name depth fv_c inv_depth avg; flush stderr)
      stats
  else
    (Printf.printf "fv_c=%d; depth=%d; inv_depth=%d\n" fv_c depth inv_depth; (tabulate res))

let throughput_original_vs_inc_mod e gamma_init d r t =
  let nc = nodecount e in
  let full_cache = IncrementalFunAlgorithm.get_empty_cache nc in
  ignore (IncrementalFunAlgorithm.build_cache e gamma_init full_cache);
  let res_bench = throughputN ~repeat:r t [
      ("orig",
        (fun () -> ignore(OriginalFunAlgorithm.typing gamma_init e)), ());
      ("incr-fullcache",  (* Incremental algorithm w. full cache *)
        (fun _ -> ignore (IncrementalFunAlgorithm.typing full_cache gamma_init e)), ());
      ("incr-emptycache", (* Incremental algorithm w. emtpy cache *)
        (fun _ -> ignore (IncrementalFunAlgorithm.typing (IncrementalFunAlgorithm.get_empty_cache nc) gamma_init e)), ());
      ("setup+incr",
        (fun () ->
          let cache = IncrementalFunAlgorithm.get_empty_cache nc in
            ignore(IncrementalFunAlgorithm.build_cache e gamma_init cache);
            simulate_modification cache e d;
            ignore(IncrementalFunAlgorithm.typing cache gamma_init e)), ());
      ("setup",
        (fun () ->
          let cache = IncrementalFunAlgorithm.get_empty_cache nc in
            ignore(IncrementalFunAlgorithm.build_cache e gamma_init cache);
            simulate_modification cache e d;
            ignore()), ());
  ] in
  let res_bench = cleanup res_bench in
    res_bench

let _ =
  if Array.length Sys.argv < 6 then
    Printf.printf "%s repeat time min_depth max_depth csv\n" Sys.argv.(0)
  else
    let repeat, time, min_depth, max_depth, csv = int_of_string Sys.argv.(1), int_of_string Sys.argv.(2), int_of_string Sys.argv.(3), int_of_string Sys.argv.(4), bool_of_string Sys.argv.(5) in
    let depth_list = gen_list min_depth max_depth (fun n -> n+2) in  (* Seems that big AST have ~20k nodes, cfr. [Erdweg et al.] *)
    let fv_c_list =  1 :: (gen_list 4 (BatInt.pow 2 (max_depth-1)) (fun n -> 2*n)) in (* Saturating the leaves w all different variables: 2^(depth-1) *)
    let inv_depth_list =  [1; 2; 3] @ gen_list 4 max_depth (fun n -> n + 2) in (* Invalidating a tree of 2^(depth - inv_depth) - 1 nodes, i.e. ~2^(-inv_depth) % *)
    let tpl_cmp (a_d, (a_fvc, a_id)) (b_d, (b_fvc, b_id)) = if (a_id = b_id && a_fvc=b_fvc && a_d = b_d) then 0 else -1 in
    let param_list = List.sort_uniq tpl_cmp (cartesian (cartesian inv_depth_list fv_c_list) depth_list) in
    let param_list = List.filter (fun (depth, (fv_c, inv_depth)) -> fv_c <= (BatInt.pow 2 (depth-1)) && inv_depth < depth) param_list in
    let len = List.length param_list in
    if csv then
      (Printf.eprintf "name, depth, fvc, inv_depth, avg, sd\n"; flush stderr)
    else ();
    List.iteri (fun i (depth, (fv_c, inv_depth)) -> (
      Printf.printf "============= [%d/%d] -- depth=%d; fv_c=%d; inv_depth=%d =============\n" (i+1) len depth fv_c inv_depth;
      flush stdout;
      let e = Generator.gen_ibop_ids_ast depth "+" fv_c in
      let initial_gamma_list e = (List.map (fun id -> (id, TInt)) (VarSet.elements (compute_fv e))) in
      let gamma_init = (FunContext.add_list (initial_gamma_list e) (FunContext.get_empty_context ()) ) in
        print_res csv (throughput_original_vs_inc_mod e gamma_init inv_depth repeat time) repeat time fv_c depth inv_depth;
        Printf.printf "============= [%d/%d] DONE =============\n" (i+1) len;
        flush stdout;
      )
    ) param_list
