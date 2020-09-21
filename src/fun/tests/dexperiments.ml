open Core
open Core_bench
open Experiments
open FunSpecification.FunSpecification
open VarSet

let _ =
   if Array.length Sys.argv < 5 then
    Printf.eprintf "Usage: %s quota min max step\n" Sys.argv.(0)
  else
    let quota, min, max, n_intervals =
      Quota.of_string Sys.argv.(1),
      int_of_string Sys.argv.(2),
      int_of_string Sys.argv.(3),
      int_of_string Sys.argv.(4) in
    let n_list = Generator.gen_list (Generator.pow 2 min) (Generator.pow 2 max) (fun n -> n*2) in
    let annotated_fact e = annotate_fv (OriginalFunAlgorithm.term_map (fun e -> compute_hash e) e) in
    let prog_list = List.map ~f:(fun n -> (n,  annotated_fact (Generator.fact_unroll n))) n_list in
    let len = List.length prog_list in
    Printf.printf "name, fvc, invalidation_parameter, nodecount, diffsz, rate\n"; flush stdout;
    List.iteri ~f:(fun i (n, e) -> (
      let interval_list = Generator.gen_list 1 n (fun s -> s + int_of_float (0.5 +. float_of_int (n-1)/. float_of_int (n_intervals - 1))) in
      Printf.eprintf "[%d/%d] n=%d ...\n" (i+1) len n;
      flush stderr;
      List.iteri ~f:(fun j xi_invalidated -> (
        Printf.eprintf "\t[%d/%d] xi_invalidated=%d ... " (j+1) (List.length interval_list) xi_invalidated;
        flush stderr;
        let gamma_init = FunContext.get_empty_context () in
        let simplified_results = Experiments.throughput_original_vs_inc quota Core_bench.Verbosity.Quiet Generator.simulate_fullchange xi_invalidated 0 gamma_init e in
          Experiments.print_csv simplified_results;
          Printf.eprintf "done!\n";
          flush stderr;
          )
        ) interval_list
      )
    )
    prog_list
