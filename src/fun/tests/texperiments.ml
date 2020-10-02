open Core
open Core_bench
open FunSpecification.FunSpecification
open VarSet

module IncrementalFunAlgorithm = Incrementalizer.TypeAlgorithm(FunSpecification.FunSpecification)

let _ =
   if Array.length Sys.argv < 5 then
    Printf.eprintf "%s quota min_depth max_depth threshold_fractions\n" Sys.argv.(0)
  else
     let quota, min_depth, max_depth, threshold_fractions =
      Quota.of_string Sys.argv.(1),
      int_of_string Sys.argv.(2),
      int_of_string Sys.argv.(3),
      int_of_string Sys.argv.(4) in
    let depth_list = Generator.gen_list min_depth max_depth (fun n -> n+2) in  (* Seems that big AST have ~20k nodes, cfr. [Erdweg et al.] *)
    let len = List.length depth_list in
    Printf.printf "name, fvc, invalidation_parameter, nodecount, diffsz, threshold, rate\n"; Out_channel.flush stdout;
    List.iteri ~f:(fun i depth -> (
      let fv_c_list =  1 :: Generator.gen_list (Generator.pow 2 7) (Generator.pow 2 (depth-1)) (fun n -> n*2) in
      let inv_depth_list = [1; 2; 3] @ Generator.gen_list 4 (depth - 1) (fun n -> n + 2) in
      Printf.eprintf "[%d/%d] depth=%d ...\n" (i+1) len depth;
      Out_channel.flush stderr;
        List.iteri ~f:(fun j fv_c -> (
          Printf.eprintf "\t[%d/%d] fv_c=%d ...\n" (j+1) (List.length fv_c_list) fv_c;
          Out_channel.flush stderr;
          let e = Generator.ibop_gen_ast depth "+" fv_c in
          let nc = Generator.nodecount e in
          let initial_gamma_list e = (List.map ~f:(fun id -> (id, TInt)) (VarSet.elements (compute_fv e))) in
          let t_list = (List.map ~f:(fun v -> Some v) (0::(Generator.gen_list (nc/threshold_fractions) nc (fun s -> s + int_of_float (0.5 +. float_of_int (nc-1)/. float_of_int (threshold_fractions - 1))))))@[None] in
          List.iteri ~f:(fun k inv_depth ->
            Printf.eprintf "\t\t[%d/%d] inv_depth=%d ... \n" (k+1) (List.length inv_depth_list) inv_depth;
            Out_channel.flush stderr;
            let gamma_init = (FunContext.add_list (initial_gamma_list e) (FunContext.get_empty_context ()) ) in
            List.iteri ~f:(fun l t ->
              Printf.eprintf "\t\t[%d/%d] threshold=%d ... " (l+1) (List.length t_list) (Option.value t ~default:(-1));
              Out_channel.flush stderr;
              let orig_vs_inc_res =
                Experiments.throughput_original_vs_inc
                  quota
                  Core_bench.Verbosity.Quiet
                  ?threshold:t
                  IncrementalFunAlgorithm.typing
                  Generator.ibop_sim_change
                  inv_depth
                  fv_c
                  gamma_init
                  e in
              let caches_res =
                Experiments.throughput_caches
                  quota
                  Core_bench.Verbosity.Quiet
                  IncrementalFunAlgorithm.typing
                  fv_c
                  gamma_init
                  e in
                    Experiments.print_csv orig_vs_inc_res;
                    Experiments.print_csv caches_res;
                    Printf.eprintf "done!\n";
                    Out_channel.flush stderr;
            ) t_list
          ) inv_depth_list
        )) fv_c_list
        )
      )
    depth_list
