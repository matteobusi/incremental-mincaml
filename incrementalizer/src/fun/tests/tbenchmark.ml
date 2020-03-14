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
*)
(* let cleanup (res : (string * Benchmark.t list) list) : (string * Benchmark.t list) list =
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
      ) res *)

(* let bench_original_vs_inc e gamma_init nc r t =
  (* These are just to avoid multiple recomputations *)
  let full_cache = IncrementalFunAlgorithm.get_empty_cache nc in
  ignore (IncrementalFunAlgorithm.build_cache e gamma_init full_cache);
  let res = throughputN ~repeat:r t
  [
    ("orig", (* Original algorithm *)
      (fun _ -> ignore (OriginalFunAlgorithm.typing gamma_init e)),
      (fun () -> ()));
    ("inc",  (* Incremental algorithm w. full cache *)
      (fun _ -> ignore (IncrementalFunAlgorithm.typing full_cache gamma_init e)),
      (fun () -> ()));
    ("einc", (* Incremental algorithm w. emtpy cache *)
      (fun cache -> ignore (IncrementalFunAlgorithm.typing (IncrementalFunAlgorithm.get_empty_cache nc) gamma_init e)),
      (fun () -> ()))
  ] in
  cleanup res

let bench_original_vs_inc_mod e gamma_init nc d r t =
  let empty_cache = IncrementalFunAlgorithm.get_empty_cache nc in
  let inv_cache = IncrementalFunAlgorithm.get_empty_cache nc in
  ignore (IncrementalFunAlgorithm.build_cache e gamma_init inv_cache);
  simulate_modification inv_cache e d;
  let res = throughputN ~repeat:r t
    [
      ("orig",
        (fun _ -> ignore (OriginalFunAlgorithm.typing gamma_init e)),
        (fun () -> empty_cache)
      );
      ("inc",
        (fun inv_cache -> ignore (IncrementalFunAlgorithm.typing inv_cache gamma_init e)),
        (fun () -> simulate_modification inv_cache e d; inv_cache)
        (* ok because the cache after incr. typing is the same as the initial full cache - just in the setting of these experiments ! *)
      )
    ] in
    cleanup res *)

let latency_inc e gamma_init nc d =
  let empty_cache = IncrementalFunAlgorithm.get_empty_cache nc in
  let inv_cache = IncrementalFunAlgorithm.get_empty_cache nc in
  ignore (IncrementalFunAlgorithm.build_cache e gamma_init inv_cache);
  (* simulate_modification inv_cache e d; *)
  let wc = Cache.copy inv_cache in
    latencyN 1000L [
      ("simmod", (fun () -> IncrementalFunAlgorithm.typing (simulate_modification inv_cache e d; inv_cache) gamma_init e), ());
      ("double_cache", (fun () -> IncrementalFunAlgorithm.typing inv_cache gamma_init e ~wcache:wc), ());
    ]

let gen_list min max next =
  let rec gen_aux curr =
    if curr >= max then [max] else curr :: (gen_aux (next curr))
  in gen_aux min

let rec cartesian a b = match b with
| [] -> []
| be :: bs ->  (List.map (fun ae -> (be, ae)) a) @ (cartesian a bs)

let print_res ?(inv_depth=(-1)) csv results repeat time transf fv_c depth =
  if csv then
    List.iter (
      fun (name, reslist) ->
      List.iter (
        fun res ->
        let rate = (Int64.to_float res.iters) /. (res.utime +. res.stime) in
          Printf.eprintf "%s, %d, %d, %s, %d, %d, %d, %f\n" name repeat time transf fv_c depth inv_depth rate) reslist; flush stderr
      ) results
  else
    (Printf.printf "transf=%s; fv_c=%d; depth=%d; inv_depth=%d\n" transf fv_c depth inv_depth; (results |> tabulate))

let _ =
  let e = Generator.gen_ibop_ids_ast 16 "+" 32768 in
  let initial_gamma_list e = (List.map (fun id -> (id, TInt)) (VarSet.elements (compute_fv e))) in
  let gamma_init = (FunContext.add_list (initial_gamma_list e) (FunContext.get_empty_context ()) ) in
  let nc = nodecount e in
  let res = latency_inc e gamma_init nc 1 in
    print_newline (); tabulate res

(* let _ =
  if Array.length Sys.argv < 6 then
    Printf.printf "%s repeat time min_depth max_depth csv\n" Sys.argv.(0)
  else
    let repeat, time, min_depth, max_depth, csv = int_of_string Sys.argv.(1), int_of_string Sys.argv.(2), int_of_string Sys.argv.(3), int_of_string Sys.argv.(4), bool_of_string Sys.argv.(5) in
    let depth_list = gen_list min_depth max_depth (fun n -> n+2) in  (* Seems that big AST trees has ~20k nodes, [Erdweg et al., §6] in Paper *)
    let fv_c_list =  1 :: (gen_list 4 (BatInt.pow 2 (max_depth-1)) (fun n -> 2*n)) in (* Saturating the leaves w all different variables: 2^(depth-1) *)
    let inv_depth_list =  [1; 2; 3] @ gen_list 4 max_depth (fun n -> n + 2) in (* Invalidating a tree of 2^(depth - inv_depth) - 1 nodes, i.e. ~2^(-inv_depth) % *)
    let tpl_cmp (a_d, (a_fvc, a_id)) (b_d, (b_fvc, b_id)) = if (a_id = b_id && a_fvc=b_fvc && a_d = b_d) then 0 else -1 in
    let param_list = List.sort_uniq tpl_cmp (cartesian (cartesian inv_depth_list fv_c_list) depth_list) in
    let param_list = List.filter (fun (depth, (fv_c, inv_depth)) -> fv_c <= (BatInt.pow 2 (depth-1)) && inv_depth < depth) param_list in
    let len = List.length param_list in
    if csv then
      (Printf.eprintf "name, repeat, time, transf, fvc, depth, inv_depth, rate\n"; flush stderr)
    else ();
    List.iteri (fun i (depth, (fv_c, inv_depth)) -> (
      Printf.printf "[%d/%d] --- depth=%d; fv_c=%d; inv_depth=%d;\n" (i+1) len depth fv_c inv_depth;
      flush stdout;
      let e = Generator.gen_ibop_ids_ast depth "+" fv_c in
      let initial_gamma_list e = (List.map (fun id -> (id, TInt)) (VarSet.elements (compute_fv e))) in
      let gamma_init = (FunContext.add_list (initial_gamma_list e) (FunContext.get_empty_context ()) ) in
      let nc = nodecount e in
      (* Original typing algorithm vs. Incremental w full cache & no modifications vs. Incremental w empty cache *)
      Printf.printf "transf=id...";
      flush stdout;
      print_res csv (bench_original_vs_inc e gamma_init nc repeat time) repeat time "id" fv_c depth;
      Printf.printf "done\n";
      flush stdout;

      (* (Simulated) code generic modification: full re-typing vs. incremental w full cache *)
      Printf.printf "transf=mod...";
      flush stdout;
      print_res csv (bench_original_vs_inc_mod e gamma_init nc inv_depth repeat time) repeat time "mod" fv_c depth ~inv_depth:inv_depth;
      Printf.printf "done\n";
      flush stdout;
      )
    ) param_list *)
