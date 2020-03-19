(* open Core *)
open Core_bench.Std
open Batteries
(* open Benchmark *)
(* open Benchmark2 *)
open Core_bench.Simplified_benchmark

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

let throughput_original_vs_inc e gamma_init inv_depth_list =
  let nc = nodecount e in
  let measures = Bench.measure
    ~run_config:(Core_bench.Std.Bench.Run_config.create ~verbosity:Core_bench.Verbosity.Quiet ())
    [
    Bench.Test.create ~name:"orig"
        (fun () -> OriginalFunAlgorithm.typing gamma_init e);
      Bench.Test.create ~name:"incr-fullcache"
        (
          let cache = IncrementalFunAlgorithm.get_empty_cache nc in
            ignore (IncrementalFunAlgorithm.build_cache e gamma_init cache);
            fun () -> IncrementalFunAlgorithm.typing cache gamma_init e
        );
      Bench.Test.create ~name:"incr-emptycache"
        (fun () -> IncrementalFunAlgorithm.typing (IncrementalFunAlgorithm.get_empty_cache nc) gamma_init e);
      Bench.Test.create_indexed
      ~name:"bc+inv+incr"
      ~args:inv_depth_list
        (fun d ->
          Core.Staged.stage (fun () ->
          let cache = IncrementalFunAlgorithm.get_empty_cache nc in
            ignore (IncrementalFunAlgorithm.build_cache e gamma_init cache);
            simulate_modification cache e d;
            IncrementalFunAlgorithm.typing cache gamma_init e
        ));
      Bench.Test.create_indexed
      ~name:"bc+inv"
      ~args:inv_depth_list
        (fun d ->
          Core.Staged.stage(fun () ->
          let cache = IncrementalFunAlgorithm.get_empty_cache nc in
            ignore (IncrementalFunAlgorithm.build_cache e gamma_init cache);
            simulate_modification cache e d;
            cache
        ));
    ] in
  let results = List.map Bench.analyze measures in
  let results = List.filter_map
    (fun (a : Core_bench.Analysis_result.t Core.Or_error.t) -> match a with
      | Error err -> Printf.eprintf "Error %s\n%!" (Core.Error.to_string_hum err); None
      | Ok r -> Some r) results in
  let sexp_res = Core_bench.Simplified_benchmark.to_sexp results in
  (* This fixes the extra open/close parentheses in the output of to_string *)
  let rec printer sexp = match sexp with
        | Core.Sexp.Atom s -> Printf.printf "%s\n" s
        | Core.Sexp.List tl -> (List.iter (fun t -> Printf.printf "%s\n" (Core.Sexp.to_string_mach t)) tl) in
  printer sexp_res

let gen_list min max next =
  let rec gen_aux curr =
    if curr >= max then [max] else curr :: (gen_aux (next curr))
  in gen_aux min

let rec cartesian a b = match b with
| [] -> []
| be :: bs -> (List.map (fun ae -> (be, ae)) a) @ (cartesian a bs)

let _ =
  if Array.length Sys.argv < 6 then
    Printf.printf "%s quota min_depth max_depth\n" Sys.argv.(0)
  else
    let quota, min_depth, max_depth = int_of_string Sys.argv.(1), int_of_string Sys.argv.(2), int_of_string Sys.argv.(3) in
    let depth_list = gen_list min_depth max_depth (fun n -> n+2) in  (* Seems that big AST have ~20k nodes, cfr. [Erdweg et al.] *)
    let len = List.length depth_list in
    List.iteri (fun i depth -> (
      let fv_c = BatInt.pow 2 (depth-1) in
      let inv_depth_list = [1; 2; 3] @ gen_list 4 depth (fun n -> n + 2) in
      Printf.eprintf "============= BEGIN: [%d/%d] -- depth=%d; fv_c=%d =============\n" (i+1) len depth fv_c;
      flush stderr;
      let e = Generator.gen_ibop_ids_ast depth "+" fv_c in
      let initial_gamma_list e = (List.map (fun id -> (id, TInt)) (VarSet.elements (compute_fv e))) in
      let gamma_init = (FunContext.add_list (initial_gamma_list e) (FunContext.get_empty_context ()) ) in
      throughput_original_vs_inc e gamma_init inv_depth_list;
      Printf.eprintf "============= END: [%d/%d] -- depth=%d; fv_c=%d =============\n" (i+1) len depth fv_c;
      flush stderr))
    depth_list

(*  (* Must use either Std.Hashtbl or Batteries WeakHashtbl for caches! *)
  let print_histogram c =
    let hist = IncrementalFunAlgorithm.Cache.stats c in
      Printf.printf "num: %d, max len: %d\n" hist.num_buckets hist.max_bucket_length;
      Array.iteri (fun k freq ->
        Printf.printf "of len %d [" k;
        for i=0 to freq-1 do
          Printf.printf "|"
        done;
        Printf.printf "] - %d buckets\n" freq;
      ) hist.bucket_histogram

  let nc = nodecount e in
  let cache = IncrementalFunAlgorithm.get_empty_cache nc in
    ignore (IncrementalFunAlgorithm.build_cache e gamma_init cache);
    print_histogram cache;
    ignore(IncrementalFunAlgorithm.typing cache gamma_init e);
    print_newline ();
    print_histogram cache;
    flush stdout;
    print_newline ();
*)
