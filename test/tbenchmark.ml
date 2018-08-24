open Core_bench

open M
open Annotast
open Cache
open Generator
open Incremental
open Typing
open Varset

(*
Takes two annotats, then
  - benchmarks typechecking on 'e'
  - benchmarks build_cache on 'e'
  - benchmarks typechecking and incremental type checking on 'em'
  - print the results
*)
let benchmark e em = 
  (* Fill up the initial gamma with needed identifiers *)
  let initial_gamma_list e = (List.map (fun id -> (id, Type.Int)) (VarSet.to_list (Annotast.free_variables e))) in
  let gamma_init = (M.add_list (initial_gamma_list e) M.empty) in
  let gamma_init_m = (M.add_list (initial_gamma_list em) M.empty) in
  (* These are just to avoid multiple recomputations *)
  let typed_aast = Typing.typecheck gamma_init e in
  let cache = Cache.build_cache typed_aast gamma_init in
  [
      Bench.Test.create ~name:"TE" (fun () -> ignore (Typing.typecheck gamma_init e));
      Bench.Test.create ~name:"TEM" (fun () -> ignore (Typing.typecheck gamma_init_m em));
      Bench.Test.create ~name:"ITEM" (fun () -> ignore (IncrementalTyping.typecheck cache gamma_init_m em));
  ] |> Bench.make_command |> Core.Command.run
  (* let benchmark_result = Benchmark.latencyN ~repeat:1 10000L [
    ("TE", (fun () -> ignore (Typing.typecheck gamma_init e)), ()); 
    ("TEM", (fun () -> ignore (Typing.typecheck gamma_init_m em)), ());
    (* ("CE", (fun () -> ignore (Cache.build_cache typed_aast gamma_init)), ()); Thanks to OCaml's eagerness *)
    ("ITEM", (fun () -> ignore (IncrementalTyping.typecheck cache gamma_init_m em)), ()) 
  ] in Benchmark.tabulate benchmark_result *)

let _ = List.iter (fun n -> Printf.printf "Using %d.\n" n; (let e = Generator.gen_ibop_id_ast n "+" "x" in benchmark e e)) [2; 4; 6; 8; 10; 12; 14; 16]