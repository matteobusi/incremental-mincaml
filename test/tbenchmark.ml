(* open Core_bench *)
open Benchmark

open M
open Annotast
open Cache
open Generator
open Incremental
open Typing
open Varset

let bench_original_vs_inc e repeat time = 
  (* Fill up the initial gamma with needed identifiers *)
  let initial_gamma_list e = (List.map (fun id -> (id, Type.Int)) (VarSet.elements (Annotast.free_variables e))) in
  let gamma_init = (M.add_list (initial_gamma_list e) M.empty) in
  (* These are just to avoid multiple recomputations *)
  let typed_e = Typing.typecheck gamma_init e in
  let init_sz = M.cardinal gamma_init in
  let empty_cache = Cache.create_empty init_sz in
  let full_cache = Cache.copy (Cache.create_empty init_sz) in
  Cache.build_cache typed_e gamma_init full_cache;
  Benchmark.throughputN ~style:Benchmark.Nil ~repeat:repeat time [
    ("Origin", (fun () -> ignore (Typing.typecheck gamma_init e)), ());
    ("CacInc", (fun () -> ignore (IncrementalTyping.typecheck full_cache gamma_init e)), ());
    ("EmpInc", (fun () -> ignore (Cache.clear empty_cache; IncrementalTyping.typecheck empty_cache gamma_init e)), ())
  ] 

let bench_original_vs_incr_add e d repeat time = 
  (* Fill up the initial gamma with needed identifiers *)
  let initial_gamma_list e = (List.map (fun id -> (id, Type.Int)) (VarSet.elements (Annotast.free_variables e))) in
  let gamma_init = (M.add_list (initial_gamma_list e) M.empty) in
  (* These are just to avoid multiple recomputations *)
  let typed_e = Typing.typecheck gamma_init e in
  let init_sz = M.cardinal gamma_init in
  let full_cache = Cache.create_empty init_sz in
  (* Build the full cache for e *)
  Cache.build_cache typed_e gamma_init full_cache;
  (* Invalidate part of the cache, corresponding to the rightmost subtree of depth tree_depth - d; This simulates addition of code. *)
  invalidate_rsubast full_cache e d;
  Benchmark.throughputN ~style:Benchmark.Nil ~repeat:repeat time [
    ("Origin", (fun () -> ignore (Typing.typecheck gamma_init e)), ());
    ("CacInc", (fun () -> ignore (invalidate_rsubast full_cache e d; IncrementalTyping.typecheck full_cache gamma_init e)), ());
  ] 

let bench_original_vs_incr_elim e d repeat time = ()

let _ = 
  (* Printf.printf "repeat, time, fvc, depth, name, rate\n"; *)
  let repeat, time = 1, 2 in
  let param_list = [(16, 16); (32768, 16)] in
    (* Original typing algorithm vs. Incremental w full cache & no mofications vs. Incremental w empty cache *)
    List.iter (fun (fv_c, depth) -> (
      Printf.printf "transf=id; fv_c=%d; depth=%d\n" fv_c depth;
      let e = Generator.gen_ibop_ids_ast depth "+" fv_c in 
      (bench_original_vs_inc e repeat time) |> Benchmark.tabulate)) param_list;
    (* (Simulated) code addition: full re-typing vs. incremental w full cache *)
    List.iter (fun (fv_c, depth) -> (
      let inv_depth = depth/3 in
      Printf.printf "transf=add; fv_c=%d; depth=%d; inv_depth=%d\n" fv_c depth inv_depth;
      let e = Generator.gen_ibop_ids_ast depth "+" fv_c in 
      (bench_original_vs_incr_add e inv_depth repeat time) |> Benchmark.tabulate)) param_list;
    (* Code elimination: full re-typing vs. incremental w full cache *)

    
    (*
    List.iter (fun n -> Printf.printf "\t One free variable; depth %d.\n" n; (let e = Generator.gen_ibop_id_ast n "+" "x" in benchmark e e)) [2; 4; 8; 16; 20];
    List.iter (fun n -> Printf.printf "\t %d free variable(s); depth %d.\n" n n; (let e = Generator.gen_ibop_ids_ast n "+" n in benchmark e e)) [2; 4; 8; 16; 20];
    List.iter (fun n -> let fv_n = (Batteries.Int.pow 2 n) in Printf.printf "\t %d free variable(s); depth %d.\n" fv_n n; (let e = Generator.gen_ibop_ids_ast n "+" fv_n in benchmark e e)) [2; 4; 8; 16; 20]
    (* Code motion: full re-typing vs. incremental w full cache *)
    (* Code elimination: full re-typing vs. incremental w full cache *) *)