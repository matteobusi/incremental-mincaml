(* open Core *)
open Core_bench.Std
open Core_bench.Simplified_benchmark
open Core_bench.Simplified_benchmark.Result
open Core_bench.Simplified_benchmark.Results
open Batteries

module OriginalFunAlgorithm = Original.TypeAlgorithm(FunSpecification.FunSpecification)
module IncrementalFunAlgorithm = Incrementalizer.TypeAlgorithm(FunSpecification.FunSpecification)

open FunSpecification.FunSpecification

open Generator
open VarSet

(* Names of the experiments *)
let orig_n = "orig" (* Original typing algorithm *)
let einc_n = "einc" (* Incremental typing algorithm w. empty initial cache *)
let finc_n = "finc" (* Incremental typing algorithm w. full initial cache *)
let inc_n = "inc" (* Just the incremental typing algorithm *)

let throughput_original_vs_inc times verbosity e gamma_init inv_depth =
  let nc = Generator.nodecount e in
  let quota = Core_bench.Std.Bench.Quota.of_string (string_of_int times ^ "x") in
    let copies = Array.init times (fun _ ->
      let cache = IncrementalFunAlgorithm.Cache.copy (IncrementalFunAlgorithm.get_empty_cache nc) in
        ignore (IncrementalFunAlgorithm.build_cache e gamma_init cache);
        Generator.simulate_modification cache e inv_depth;
        cache
      ) in
    let c_counter = ref 0 in
    let measures = Bench.measure
      ~run_config:(Core_bench.Std.Bench.Run_config.create ~quota:quota ~verbosity:verbosity ())
      [
        Bench.Test.create ~name:(orig_n ^ ":" ^ string_of_int inv_depth)
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
        Bench.Test.create ~name:(inc_n ^ ":" ^ string_of_int inv_depth)
          (fun () ->
            incr c_counter;
            IncrementalFunAlgorithm.typing copies.(!c_counter - 1) gamma_init e
          );
      ] in
    let results = List.map Bench.analyze measures in
    let results = List.filter_map
      (fun (a : Core_bench.Analysis_result.t Core.Or_error.t) -> match a with
        | Error err -> Printf.printf "Error %s\n%!" (Core.Error.to_string_hum err); None
        | Ok r -> Some r) results in
    (* Ugly hack, should use extract but it is not exposed by the interface! *)
    Core_bench.Simplified_benchmark.Results.t_of_sexp (Core_bench.Simplified_benchmark.to_sexp results)

module MinimalResult = struct
    type t =
    {
      name : string;
      depth : int;
      fvc : int;
      inv_depth : int;
      rate : float
    }
end

let split_name name =
  match String.split_on_char ':' name with
  | [n] -> (n, -1)
  | [n; inv_depth] -> (n, Int.of_string inv_depth)
  | _ -> Printf.eprintf "Name: '%s'\n" name; failwith "Error: does the experiment name contains more than one ':'?"

let extract_minimal (results : Core_bench.Simplified_benchmark.Results.t) depth fvc =
  (* First look for incr_setup_name and setup_name and merge them *)
  let rate_of_time t = 1000000000./.t in
  let project r =
    MinimalResult.({
      name = r.full_benchmark_name;
      depth = depth;
      fvc = fvc;
      inv_depth = snd (split_name r.full_benchmark_name);
      rate = rate_of_time r.time_per_run_nanos
    }) in
  List.map project results

let print_csv (mr_list : MinimalResult.t list) =
  List.iter
  (fun (mr : MinimalResult.t) -> Printf.printf "%s, %d, %d, %d, %f\n" mr.name mr.depth mr.fvc mr.inv_depth mr.rate; flush stdout) mr_list

let _ =
   if Array.length Sys.argv < 4 then
    Printf.eprintf "%s times min_depth max_depth\n" Sys.argv.(0)
  else
    let times, min_depth, max_depth =
      int_of_string Sys.argv.(1), int_of_string Sys.argv.(2), int_of_string Sys.argv.(3) in
    let depth_list = Generator.gen_list min_depth max_depth (fun n -> n+2) in  (* Seems that big AST have ~20k nodes, cfr. [Erdweg et al.] *)
    let len = List.length depth_list in
    Printf.printf "name, depth, fvc, inv_depth, rate\n"; flush stdout;
    List.iteri (fun i depth -> (
      let fv_c_list = 1 :: Generator.gen_list (BatInt.pow 2 7) (BatInt.pow 2 (depth-1)) (fun n -> n*2) in
      let inv_depth_list = [1; 2; 3] @ Generator.gen_list 4 (depth - 1) (fun n -> n + 2) in
      Printf.eprintf "[%d/%d] depth=%d ...\n" (i+1) len depth;
      flush stderr;
        List.iteri (fun j fv_c -> (
          Printf.eprintf "\t[%d/%d] fv_c=%d ...\n" (j+1) (List.length fv_c_list) fv_c;
          flush stderr;
          List.iteri (fun k inv_depth ->
            Printf.eprintf "\t\t[%d/%d] inv_depth=%d ... " (k+1) (List.length inv_depth_list) inv_depth;
            flush stderr;
            let e = Generator.gen_ibop_ids_ast depth "+" fv_c in
            let initial_gamma_list e = (List.map (fun id -> (id, TInt)) (VarSet.elements (compute_fv e))) in
            let gamma_init = (FunContext.add_list (initial_gamma_list e) (FunContext.get_empty_context ()) ) in
            let simplified_results = throughput_original_vs_inc times Core_bench.Verbosity.Quiet e gamma_init inv_depth in
              print_csv (extract_minimal simplified_results depth fv_c);
              Printf.eprintf "done!\n";
              flush stderr;
          ) inv_depth_list
        )) fv_c_list
        )
      )
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

  let nc = Generator.nodecount e in
  let cache = IncrementalFunAlgorithm.get_empty_cache nc in
    ignore (IncrementalFunAlgorithm.build_cache e gamma_init cache);
    print_histogram cache;
    ignore(IncrementalFunAlgorithm.typing cache gamma_init e);
    print_newline ();
    print_histogram cache;
    flush stdout;
    print_newline ();
*)
