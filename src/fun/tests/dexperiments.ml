open Core
open Core_bench
open Experiments
open FunSpecification.FunSpecification
open VarSet

module IncrementalFunAlgorithm = Incrementalizer.TypeAlgorithm(FunSpecification.FunSpecification)

let _ =
   if Array.length Sys.argv < 6 then
    Printf.eprintf "Usage: %s quota min max step threshold_fractions\n" Sys.argv.(0)
  else
    let quota, min, max, n_intervals, threshold_fractions =
      Quota.of_string Sys.argv.(1),
      int_of_string Sys.argv.(2),
      int_of_string Sys.argv.(3),
      int_of_string Sys.argv.(4),
      int_of_string Sys.argv.(5) in
    let n_list = Generator.gen_list (Generator.pow 2 min) (Generator.pow 2 max) (fun n -> n*2) in
    let annotated_fact e = annotate_fv (OriginalFunAlgorithm.term_map (fun e -> compute_hash e) e) in
    let prog_list = List.map ~f:(fun n -> (n,  annotated_fact (Generator.fact_gen_ast n))) n_list in
    let len = List.length prog_list in
    Printf.printf "name, fvc, invalidation_parameter, nodecount, diffsz, threshold, rate\n"; flush stdout;
    List.iteri ~f:(fun i (n, e) -> (
      let interval_list = Generator.gen_list 1 n (fun s -> s + int_of_float (0.5 +. float_of_int (n-1)/. float_of_int (n_intervals - 1))) in
      Printf.eprintf "[%d/%d] n=%d ...\n" (i+1) len n;
      flush stderr;
      List.iteri ~f:(fun j xi_invalidated ->
        Printf.eprintf "\t[%d/%d] xi_invalidated=%d ... \n" (j+1) (List.length interval_list) xi_invalidated;
        flush stderr;
        let gamma_init = FunContext.get_empty_context () in
        let nc = nodecount e in
        let t_list = (List.map ~f:(fun v -> Some v) (0::(Generator.gen_list (nc/threshold_fractions) nc (fun s -> s + int_of_float (0.5 +. float_of_int (nc-1)/. float_of_int (threshold_fractions - 1))))))@[None] in
        List.iteri ~f:(fun k t ->
        Printf.eprintf "\t\t[%d/%d] threshold=%d ... " (k+1) (List.length t_list) (Option.value t ~default:(-1));
        flush stderr;
        let simplified_results = Experiments.throughput_original_vs_inc
            quota
            Core_bench.Verbosity.Quiet
            ?threshold:t
            IncrementalFunAlgorithm.typing
            Generator.fact_sim_change
            xi_invalidated
            0
            gamma_init
            e
        in
          Experiments.print_csv simplified_results;
          Printf.eprintf "done!\n";
          flush stderr;
          ) t_list
        ) interval_list
      )
    )
    prog_list
