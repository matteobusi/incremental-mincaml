open Core
open Core_bench
(* open Generator *)

open FunSpecification.FunSpecification
open VarSet

open Stest
open Sbenchmark

module OriginalFunAlgorithm = Original.TypeAlgorithm(FunSpecification.FunSpecification)
module IncrementalFunAlgorithm = Incrementalizer.TypeAlgorithm(FunSpecification.FunSpecification)


(* Names of the experiments *)
let orig_n = "orig" (* Original typing algorithm *)
let einc_n = "einc" (* Incremental typing algorithm w. empty initial cache *)
let finc_n = "finc" (* Incremental typing algorithm w. full initial cache *)
let inc_n = "inc" (* Just the incremental typing algorithm *)

module MinimalResult = struct
    type t =
    {
      name : string;
      fvc : int;
      invalidation_parameter : int;
      nodecount : int;
      diffsz : int;
      rate : float;
      threshold : int
    }
end

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
  | Let(_, e1, e2, annot) -> 1 + nodecount e1 + nodecount e2 (* curr node + x *)
  | LetRec ({ name = _; args = yts; body = e1 }, e2, annot) -> 1 + nodecount e1 + nodecount e2
  | App (e1, es, annot) -> 1 + (List.fold_left ~f:(+) ~init:0 (List.map ~f:nodecount (e1::es)))
  | Tuple(es, annot) -> 1 + (List.fold_left ~f:(+) ~init:0 (List.map ~f:nodecount es))
  | LetTuple(xs, e1, e2, annot) -> 1 + nodecount e1 + nodecount e2
  | Array(e1, e2, annot)
  | Get (e1, e2, annot) -> 1 + nodecount e1 + nodecount e2
  | Put (e1, e2, e3, annot) -> 1 + nodecount e1 + nodecount e2 + nodecount e3


let extract_minimal (results : Core_bench.Simplified_benchmark.Results.t) nodecount diffsz fvc threshold=
  let invalidation_parameter_of_name name =
    match String.split ~on:':' name with
    | [n] -> -1
    | [n; par] -> int_of_string par
    | _ -> Printf.eprintf "Name: '%s'\n" name; failwith "Error: does the experiment name contains more than one ':'?" in
  (* First look for incr_setup_name and setup_name and merge them *)
  let rate_of_time t = 1000000000./.t in
  let project (r : Core_bench.Simplified_benchmark.Result.t) =
    MinimalResult.({
      name = r.full_benchmark_name;
      fvc = fvc;
      invalidation_parameter = invalidation_parameter_of_name r.full_benchmark_name;
      nodecount = nodecount;
      diffsz = diffsz;
      rate = rate_of_time r.time_per_run_nanos;
      threshold = threshold
    }) in
  (List.map ~f:project results)


(* Adapted from Core_bench *)
let measure ?(run_config=Run_config.create ()) tests =
  let basic_tests = Stest.expand tests in
  Sbenchmark.measure_all run_config basic_tests


let throughput_original_vs_inc quota verbosity ?(threshold=Int.max_value) (incremental_typing_fun :  ?threshold:Core.Int.t ->
         (FunSpecification.FunSpecification.context Core.ref *
          FunSpecification.FunSpecification.res)
         IncrementalFunAlgorithm.Cache.t ->
         FunSpecification.FunSpecification.context ->
         ('a IncrementalFunAlgorithm.Cache.key_ * VarSet.t)
         FunSpecification.FunSpecification.term ->
         FunSpecification.FunSpecification.res) cache_invalidator invalidator_param fvc gamma_init e =
    let nc = Generator.nodecount e in
    (* Just compute the cache once: *)
    let experiment_cache = IncrementalFunAlgorithm.get_empty_cache () in
      ignore (IncrementalFunAlgorithm.build_cache e gamma_init experiment_cache);
      let oldsz = IncrementalFunAlgorithm.Cache.length experiment_cache in
      cache_invalidator experiment_cache e invalidator_param;
      let diffsz = oldsz - IncrementalFunAlgorithm.Cache.length experiment_cache in
      let measures = measure
        ~run_config:(Core_bench.Run_config.create ~quota:quota ~verbosity:verbosity ())
        [
          Stest.create
            ~name:(orig_n ^ ":" ^ (string_of_int invalidator_param))
            (fun () -> IncrementalFunAlgorithm.Cache.copy experiment_cache)
            (fun _ -> OriginalFunAlgorithm.typing gamma_init e);
          (* Bench.Test.create ~name:finc_n
            (let cache = IncrementalFunAlgorithm.get_empty_cache nc in
                ignore (IncrementalFunAlgorithm.build_cache e gamma_init cache);
                fun () -> IncrementalFunAlgorithm.typing cache gamma_init e
            );
          Bench.Test.create ~name:einc_n
            (fun () -> IncrementalFunAlgorithm.typing (IncrementalFunAlgorithm.get_empty_cache nc) gamma_init e); *)
          Stest.create ~name:(inc_n ^ ":" ^ (string_of_int invalidator_param))
          (fun () -> IncrementalFunAlgorithm.Cache.copy experiment_cache)
          (fun cache ->
            (* assert(oldsz - IncrementalFunAlgorithm.Cache.length cache = diffsz);
            Printf.eprintf "diff=%d\n" diffsz; flush stderr; *)
            incremental_typing_fun ~threshold:threshold cache gamma_init e);
        ] in
      let results = List.map ~f:(fun m -> Analysis.analyze m Analysis_config.default) measures in
      let results = List.filter_map
        ~f:(fun (a : Core_bench.Analysis_result.t Core.Or_error.t) -> match a with
          | Error err -> Printf.eprintf "(dexperiments.ml:59) Warning: test omitted since %s.\n" (Core.Error.to_string_hum err); None
          | Ok r -> Some r) results in
      (* Ugly hack, should use extract but it is not exposed by the interface! *)
      let results = Core_bench.Simplified_benchmark.Results.t_of_sexp (Core_bench.Simplified_benchmark.to_sexp results) in
        extract_minimal results nc diffsz fvc threshold


let print_csv (mr_list : MinimalResult.t list) =
  List.iter
  ~f:(fun (mr : MinimalResult.t) -> Printf.printf "%s, %d, %d, %d, %d, %d, %f\n" mr.name mr.fvc mr.invalidation_parameter mr.nodecount mr.diffsz mr.threshold mr.rate; flush stdout) mr_list

(* FIXME: duplicate from main.ml; Is there a common place to put this one into? *)
let rec annotate_fv e =
  match e with
    | Unit(annot) -> Unit((annot, VarSet.empty))
    | Bool(v, annot) -> Bool(v, (annot, VarSet.empty))
    | Int(v, annot) -> Int(v, (annot, VarSet.empty))
    | Float(v, annot) -> Float(v, (annot, VarSet.empty))
    | Var(x, annot) -> Var (x, (annot, VarSet.singleton x))
    | Not(e1, annot) -> let ae1 = annotate_fv e1 in Not (ae1, (annot, snd (term_getannot ae1)))
    | Neg(e1, annot) -> let ae1 = annotate_fv e1 in Neg (ae1, (annot, snd (term_getannot ae1)))
    | FNeg(e1, annot) -> let ae1 = annotate_fv e1 in FNeg (ae1, (annot, snd (term_getannot ae1)))
    | IBop(op, e1, e2, annot) ->
      let ae1, ae2 = annotate_fv e1, annotate_fv e2 in
        IBop (op, ae1, ae2, (annot, VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2))))
    | FBop(op, e1, e2, annot) ->
      let ae1, ae2 = annotate_fv e1, annotate_fv e2 in
        FBop (op, ae1, ae2, (annot, VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2))))
    | Rel(op, e1, e2, annot) ->
      let ae1, ae2 = annotate_fv e1, annotate_fv e2 in
        Rel (op, ae1, ae2, (annot, VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2))))
    | If(e1, e2, e3, annot) ->
      let ae1, ae2, ae3 = annotate_fv e1, annotate_fv e2, annotate_fv e3 in
        If (ae1, ae2, ae3,
          (annot,
            VarSet.union (snd (term_getannot ae3)) (VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2)))))
    | Let(x, e1, e2, annot) ->
      let ae1, ae2 = annotate_fv e1, annotate_fv e2 in
        Let (x, ae1, ae2,
          (annot, VarSet.remove (VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2))) x))
    | LetRec ({ name = (fn, ft); args = yts; body = e1 }, e2, annot) ->
      let ae1, ae2 = annotate_fv e1, annotate_fv e2 in
        let lrfv = List.fold_left (fn::(List.map yts ~f:fst)) ~init:(VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2))) ~f:VarSet.remove in
          LetRec ({ name = (fn, ft); args = yts; body = ae1 }, ae2, (annot, lrfv))
    | App (e1, es, annot) ->
      let ae1, aes = annotate_fv e1, List.map es ~f:annotate_fv in
        App (ae1, aes, (annot, (List.fold_left aes ~init:VarSet.empty ~f:(fun afv ae -> VarSet.union afv (snd (term_getannot ae))))))
    | Tuple(es, annot) ->
      let aes = List.map es ~f:annotate_fv in
        Tuple(aes, (annot, (List.fold_left aes ~init:VarSet.empty ~f:(fun afv ae -> VarSet.union afv (snd (term_getannot ae))))))
    | LetTuple(xs, e1, e2, annot) ->
      let ae1, ae2 = annotate_fv e1, annotate_fv e2 in
        let lrfv = List.fold_left xs ~init:(VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2))) ~f:VarSet.remove in
          LetTuple(xs, ae1, ae2, (annot, lrfv))
    | Array(e1, e2, annot) ->
      let ae1, ae2 = annotate_fv e1, annotate_fv e2 in
        Array(ae1, ae2, (annot, (VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2)))))
    | Get (e1, e2, annot) ->
      let ae1, ae2 = annotate_fv e1, annotate_fv e2 in
        Get (ae1, ae2, (annot, (VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2)))))
    | Put (e1, e2, e3, annot) ->
      let ae1, ae2, ae3 = annotate_fv e1, annotate_fv e2, annotate_fv e3 in
        Put (ae1, ae2, ae3, (annot, (VarSet.union (snd (term_getannot ae3)) (VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2))))))

