(* open Core_bench *)
open Batteries
open Benchmark2

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

(* let bench_original_vs_inc e repeat time =
  (* Fill up the initial gamma with needed identifiers *)
  let initial_gamma_list e = (List.map (fun id -> (id, TInt)) (VarSet.elements (compute_fv e))) in
  let gamma_init = (FunContext.add_list (initial_gamma_list e) (FunContext.get_empty_context ()) ) in
  (* These are just to avoid multiple recomputations *)
  let nc = nodecount e in
  let full_cache = IncrementalFunAlgorithm.get_empty_cache nc in
  ignore (IncrementalFunAlgorithm.build_cache e gamma_init full_cache);
  let bench_res = throughputN ~style:Nil ~repeat:repeat time [
    ("orig", (fun () -> ignore (OriginalFunAlgorithm.typing gamma_init e)), ());
    ("inc", (fun () -> ignore (IncrementalFunAlgorithm.typing full_cache gamma_init e)), ());
    ("einc", (fun () -> ignore (
      let copy_cache = IncrementalFunAlgorithm.get_empty_cache nc in (* FIXME: Maybe we can avoid this part by creating them all beforehand *)
        IncrementalFunAlgorithm.typing copy_cache gamma_init e)), ())
  ] in
  remove_setup_time bench_res [("orig", fun ()->()); ("inc", fun ()->()); ("einc", fun () -> (let copy_cache = IncrementalFunAlgorithm.get_empty_cache nc in ignore(copy_cache)))] *)

(*
  Very simple cleanup that removes the samples whose rate is not within a sigma interval from avg
*)
let cleanup (res : (string * Benchmark2.t list) list) : (string * Benchmark2.t list) list =
  let cpu = cpu_process in
  let not_outlier name avg stddev b = (
    let rate = Int64.to_float b.iters /. cpu b in
    let min, max = avg-.(stddev), avg+.(stddev) in
      Printf.printf "%s - #%Ld, avg=%f, dev=%f. %f <= %f <= %f? %b" name b.iters avg stddev min rate max (min <= rate && rate <= max);
      min <= rate && rate <= max
  ) in
  let stats = List.map (comp_rates cpu) res in
    List.map (fun (name, bm) ->
      let (_, iter, avg, stddev) = List.find (fun (n, _, _, _) -> String.equal n name) stats in
      let cleaned = List.filter (not_outlier name avg stddev) bm in
        Printf.printf "Keeping %d/%d samples." (List.length cleaned) (List.length bm);
        (name, cleaned)
      ) res

let bench_original_vs_inc_mod e gamma_init nc d r t =
  let empty_cache = IncrementalFunAlgorithm.get_empty_cache nc in
  let full_cache = IncrementalFunAlgorithm.get_empty_cache nc in
  ignore (IncrementalFunAlgorithm.build_cache e gamma_init full_cache);
  let res = throughputN ~repeat:10 1
    [
      ("orig",
        (fun _ -> ignore (OriginalFunAlgorithm.typing gamma_init e)),
        (fun () -> empty_cache)
      );
      ("inc",
        (fun full_cache -> ignore (IncrementalFunAlgorithm.typing full_cache gamma_init e)),
        (fun () ->
          let full_cache = IncrementalFunAlgorithm.get_empty_cache nc in
            ignore (IncrementalFunAlgorithm.build_cache e gamma_init full_cache);
            simulate_modification full_cache e d;
            full_cache
        )
      )
    ] in
    cleanup res

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
          Printf.printf "%s, %d, %d, %s, %d, %d, %d, %f\n" name repeat time transf fv_c depth inv_depth rate) reslist; flush stdout
      ) results
  else
    (Printf.printf "transf=%s; fv_c=%d; depth=%d; inv_depth=%d\n" transf fv_c depth inv_depth; (results |> tabulate))

let _ =
  let e = Generator.gen_ibop_ids_ast 12 "+" 2048 in
  let initial_gamma_list e = (List.map (fun id -> (id, TInt)) (VarSet.elements (compute_fv e))) in
  let gamma_init = (FunContext.add_list (initial_gamma_list e) (FunContext.get_empty_context ()) ) in
  let nc = nodecount e in
    print_newline (); tabulate (bench_original_vs_inc_mod e gamma_init nc 4 10 1)
(* let _ =
  if Array.length Sys.argv < 6 then
    Printf.printf "%s repeat time min_depth max_depth csv\n" Sys.argv.(0)
  else
    let repeat, time, min_depth, max_depth, csv = int_of_string Sys.argv.(1), int_of_string Sys.argv.(2), int_of_string Sys.argv.(3), int_of_string Sys.argv.(4), bool_of_string Sys.argv.(5) in
    let depth_list = gen_list min_depth max_depth (fun n -> n+2) in  (* Seems that big AST trees has ~20k nodes, [Erdweg et al., §6] in Paper *)
    let fv_c_list =  [(BatInt.pow 2 (max_depth-1))] in (* 1 :: (gen_list 4 (BatInt.pow 2 (max_depth-1)) (fun n -> 2*n)) in Saturating the leaves w all different variables: 2^(depth-1) *)
    let inv_depth_list =  [1; 2; 3] @ gen_list 4 max_depth (fun n -> n + 2) in (* Invalidating a tree of 2^(depth - inv_depth) - 1 nodes, i.e. ~2^(-inv_depth) % *)
    let tpl_cmp (a_d, (a_fvc, a_id)) (b_d, (b_fvc, b_id)) = if (a_id = b_id && a_fvc=b_fvc && a_d = b_d) then 0 else -1 in
    let param_list = List.sort_uniq tpl_cmp (cartesian (cartesian inv_depth_list fv_c_list) depth_list) in
    let param_list = List.filter (fun (depth, (fv_c, inv_depth)) -> fv_c <= (BatInt.pow 2 (depth-1)) && inv_depth < depth) param_list in
    let len = List.length param_list in
        (* List.iter (fun (depth, (fv_c, inv_depth)) -> Printf.printf "(depth=%d, fv_c=%d, inv_depth=%d)\n" depth fv_c inv_depth) param_list; *)
    if csv then
      Printf.printf "name, repeat, time, transf, fvc, depth, inv_depth, rate\n"
    else ();
    List.iteri (fun i (depth, (fv_c, inv_depth)) -> (
      Printf.eprintf "[%d/%d] --- depth=%d; fv_c=%d; inv_depth=%d;\n" (i+1) len depth fv_c inv_depth;
      let e = Generator.gen_ibop_ids_ast depth "+" fv_c in
      (* Original typing algorithm vs. Incremental w full cache & no mofications vs. Incremental w empty cache *)
      Printf.eprintf "transf=id...";
      flush stderr;
      print_res csv (bench_original_vs_inc e repeat time) repeat time "id" fv_c depth;
      Printf.eprintf "done\n";
      flush stderr;

      (* (Simulated) code generic modification: full re-typing vs. incremental w full cache *)
      Printf.eprintf "transf=mod...";
      flush stderr;
      print_res csv (bench_original_vs_inc_mod e inv_depth repeat time) repeat time "mod" fv_c depth ~inv_depth:inv_depth;
      Printf.eprintf "done\n";
      flush stderr;
      )
    ) param_list *)
