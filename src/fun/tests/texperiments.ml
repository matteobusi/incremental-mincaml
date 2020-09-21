open Core
open Core_bench
(* open Experiments *)
open FunSpecification.FunSpecification
open VarSet

let _ =
   if Array.length Sys.argv < 4 then
    Printf.eprintf "%s quota min_depth max_depth\n" Sys.argv.(0)
  else
    let quota, min_depth, max_depth =
      Quota.of_string Sys.argv.(1),
      int_of_string Sys.argv.(2),
      int_of_string Sys.argv.(3) in
    let depth_list = Generator.gen_list min_depth max_depth (fun n -> n+2) in  (* Seems that big AST have ~20k nodes, cfr. [Erdweg et al.] *)
    let len = List.length depth_list in
    Printf.printf "name, fvc, invalidation_parameter, nodecount, diffsz, rate\n"; flush stdout;
    List.iteri ~f:(fun i depth -> (
      let fv_c_list = 1 :: Generator.gen_list (Generator.pow 2 7) (Generator.pow 2 (depth-1)) (fun n -> n*2) in
      let inv_depth_list = [1; 2; 3] @ Generator.gen_list 4 (depth - 1) (fun n -> n + 2) in
      Printf.eprintf "[%d/%d] depth=%d ...\n" (i+1) len depth;
      flush stderr;
        List.iteri ~f:(fun j fv_c -> (
          Printf.eprintf "\t[%d/%d] fv_c=%d ...\n" (j+1) (List.length fv_c_list) fv_c;
          flush stderr;
          let e = Generator.gen_ibop_ids_ast depth "+" fv_c in
          let initial_gamma_list e = (List.map ~f:(fun id -> (id, TInt)) (VarSet.elements (compute_fv e))) in
          List.iteri ~f:(fun k inv_depth ->
            Printf.eprintf "\t\t[%d/%d] inv_depth=%d ... " (k+1) (List.length inv_depth_list) inv_depth;
            flush stderr;
            let gamma_init = (FunContext.add_list (initial_gamma_list e) (FunContext.get_empty_context ()) ) in
            let simplified_results = Experiments.throughput_original_vs_inc quota Core_bench.Verbosity.Quiet Generator.simulate_modification inv_depth fv_c gamma_init e in
              Experiments.print_csv simplified_results;
              Printf.eprintf "done!\n";
              flush stderr;
          ) inv_depth_list
        )) fv_c_list
        )
      )
    depth_list
