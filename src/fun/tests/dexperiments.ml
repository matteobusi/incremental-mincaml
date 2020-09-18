(* open Core *)
open Core_bench.Std
open Core_bench.Simplified_benchmark
open Core_bench.Simplified_benchmark.Result
open Core_bench.Simplified_benchmark.Results
open Core

open FunSpecification.FunSpecification
open VarSet
open Generator

module OriginalFunAlgorithm = Original.TypeAlgorithm(FunSpecification.FunSpecification)
module IncrementalFunAlgorithm = Incrementalizer.TypeAlgorithm(FunSpecification.FunSpecification)


(* Names of the experiments *)
let orig_n = "orig" (* Original typing algorithm *)
let einc_n = "einc" (* Incremental typing algorithm w. empty initial cache *)
let finc_n = "finc" (* Incremental typing algorithm w. full initial cache *)
(* let setupinc_n = "setup+inc" (* Incremental typing algorithm w. its setup *)
let setup_n = "setup" Just the setup of the above *)
let inc_n = "inc" (* Just the incremental typing algorithm *)

let theo_diff num_fact xi_invalidated =
  (if xi_invalidated = 1 then (2 + 4*(num_fact - 2)) else ((xi_invalidated - 1) + 4*(num_fact - xi_invalidated))) + 5

let throughput_original_vs_inc times verbosity num_fact e gamma_init xi_invalidated =
  let cache = IncrementalFunAlgorithm.get_empty_cache () in
    ignore (IncrementalFunAlgorithm.build_cache e gamma_init cache);
    ignore (Generator.simulate_fullchange cache e xi_invalidated);
  (* FIXME: In Core_bench the maximum number of samples seems to be equal to max_samples=3000, cfr. l.29 of Benchmark.ml *)
    let copies = Array.init times (fun _ -> IncrementalFunAlgorithm.Cache.copy cache) in
    let c_counter = ref 0 in
    let quota = Core_bench.Std.Bench.Quota.of_string ((string_of_int times) ^ "x") in
    let measures = Bench.measure
      ~run_config:(Core_bench.Std.Bench.Run_config.create ~quota:quota ~verbosity:verbosity ())
      [
        Bench.Test.create ~name:(orig_n ^ ":" ^ (string_of_int xi_invalidated))
          (fun () ->
            OriginalFunAlgorithm.typing gamma_init e
          );
        (* Bench.Test.create ~name:finc_n
          (let cache = IncrementalFunAlgorithm.get_empty_cache nc in
              ignore (IncrementalFunAlgorithm.build_cache e gamma_init cache);
              fun () -> IncrementalFunAlgorithm.typing cache gamma_init e
          );
        Bench.Test.create ~name:einc_n
          (fun () -> IncrementalFunAlgorithm.typing (IncrementalFunAlgorithm.get_empty_cache nc) gamma_init e); *)
        Bench.Test.create ~name:(inc_n ^ ":" ^ (string_of_int xi_invalidated))
        (
          fun () ->
            incr c_counter;
            (* assert (IncrementalFunAlgorithm.Cache.length copies.(!c_counter - 1) = new_sz); *)
            IncrementalFunAlgorithm.typing copies.(!c_counter - 1) gamma_init e
        );
      ] in
    let results = List.map ~f:Bench.analyze measures in
    let results = List.filter_map
      ~f:(fun (a : Core_bench.Analysis_result.t Core.Or_error.t) -> match a with
        | Error err -> Printf.eprintf "(dexperiments.ml:59) Warning: test omitted since %s.\n" (Core.Error.to_string_hum err); None
        | Ok r -> Some r) results in
    (* Ugly hack, should use extract but it is not exposed by the interface! *)
    Core_bench.Simplified_benchmark.Results.t_of_sexp (Core_bench.Simplified_benchmark.to_sexp results)

module MinimalResult = struct
    type t =
    {
      name : string;
      num_fact : int;
      xi_invalidated : int;
      diffsz : int;
      rate : float
    }
end

let split_name name =
  match String.split ~on:':' name with
  | [n] -> (n, -1)
  | [n; inv_depth] -> (n, Int.of_string inv_depth)
  | _ -> Printf.eprintf "Name: '%s'\n" name; failwith "Error: does the experiment name contains more than one ':'?"

let extract_minimal (results : Core_bench.Simplified_benchmark.Results.t) num_fact xi_invalidated =
  (* First look for incr_setup_name and setup_name and merge them *)
  let rate_of_time t = 1000000000./.t in
  let project r =
    MinimalResult.({
      name = r.full_benchmark_name;
      num_fact = num_fact;
      xi_invalidated = xi_invalidated;
      diffsz = theo_diff num_fact xi_invalidated;
      rate = rate_of_time r.time_per_run_nanos
    }) in
  (List.map ~f:project results)

let print_csv (mr_list : MinimalResult.t list) =
  List.iter
  ~f:(fun (mr : MinimalResult.t) -> Printf.printf "%s, %d, %d, %d, %f\n" mr.name mr.num_fact mr.xi_invalidated mr.diffsz mr.rate; flush stdout) mr_list

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

(* let test =
  QCheck.Test.make ~count:100000 ~name:"theo_vs_real"
    QCheck.(pair (int_range 1 10000) (int_range 1 10000))
    (fun (num_fact, s) ->
      (
        assume (num_fact <> 0 && s <> 0);
        assume (num_fact >= s);
        let raw_e = Generator.fact_unroll num_fact in
        let e = annotate_fv (OriginalFunAlgorithm.term_map (fun e -> compute_hash e) raw_e) in
        let nc = Generator.nodecount e in
        let gamma_init = FunContext.get_empty_context () in
        let cache = IncrementalFunAlgorithm.get_empty_cache nc in
          ignore (IncrementalFunAlgorithm.build_cache e gamma_init cache);
          let th_sz = (if s = 1 then (2 + 4*(num_fact - 2)) else ((s - 1) + 4*(num_fact - s))) + 5 in
          let pre_sz = IncrementalFunAlgorithm.Cache.length cache in
          Generator.simulate_fullchange cache e s;
          let post_sz = IncrementalFunAlgorithm.Cache.length cache in
            (th_sz = (pre_sz - post_sz))
      )
    )

let _ = QCheck_runner.run_tests_main [test] *)

(* let _ =
  let num_fact, n_intervals = int_of_string Sys.argv.(1), int_of_string Sys.argv.(2) in
  let raw_e = Generator.fact_unroll num_fact in
  let e = annotate_fv (OriginalFunAlgorithm.term_map (fun e -> compute_hash e) raw_e) in
  let nc = Generator.nodecount e in
    let interval_list = Generator.gen_list 1 num_fact (fun s -> s + num_fact/(n_intervals - 1)) in
      List.iteri (fun j s -> (
        Printf.eprintf "num_fact = %d - [%d/%d] x_%d ..." num_fact (j+1) (List.length interval_list) s;
        flush stderr;
        let gamma_init = FunContext.get_empty_context () in
        let cache = IncrementalFunAlgorithm.get_empty_cache nc in
          ignore (IncrementalFunAlgorithm.build_cache e gamma_init cache);
          let th_sz = (if s = 1 then (2 + 4*(num_fact - s - 1)) else ((s - 1) + 4*(num_fact - s))) + 5 in
          let pre_sz = IncrementalFunAlgorithm.Cache.length cache in
          let k = Generator.simulate_fullchange cache e s in
          let post_sz = IncrementalFunAlgorithm.Cache.length cache in
            Printf.eprintf "\ninv_sz = %d; diff_sz = %d; th_sz = %d\n" k (pre_sz - post_sz) th_sz
          (* IncrementalFunAlgorithm.typing cache gamma_init e *)
      )) interval_list *)



let _ =
   if Array.length Sys.argv < 5 then
    Printf.eprintf "Usage: %s times min max step\n" Sys.argv.(0)
  else
    let times, min, max, n_intervals =
      int_of_string Sys.argv.(1),
      int_of_string Sys.argv.(2),
      int_of_string Sys.argv.(3),
      int_of_string Sys.argv.(4) in
    let n_list = Generator.gen_list min max (fun c -> c * 2) in
    let annotated_fact e = annotate_fv (OriginalFunAlgorithm.term_map (fun e -> compute_hash e) e) in
    let prog_list = List.map ~f:(fun n -> (n,  annotated_fact (Generator.fact_unroll n))) n_list in
    let len = List.length prog_list in
    Printf.printf "name, num_fact, xi_invalidated, diffsz, rate\n"; flush stdout;
    List.iteri ~f:(fun i (n, e) -> (
      let interval_list = Generator.gen_list 1 n (fun s -> s + int_of_float (0.5 +. float_of_int (n-1)/. float_of_int (n_intervals - 1))) in
      Printf.eprintf "[%d/%d] n=%d ...\n" (i+1) len n;
      flush stderr;
        List.iteri ~f:(fun j s -> (
          Printf.eprintf "\t[%d/%d] x_%d ..." (j+1) (List.length interval_list) s;
          flush stderr;
          let gamma_init = FunContext.get_empty_context () in
          let simplified_results = throughput_original_vs_inc times Core_bench.Verbosity.Quiet n e gamma_init s in
            print_csv (extract_minimal simplified_results n s);
            Printf.eprintf "done!\n";
            flush stderr;
        )) interval_list
        )
      )
    prog_list
