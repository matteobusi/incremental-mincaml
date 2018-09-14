(* open Core_bench *)
open Benchmark

open M
open Annotast
open Cache
open Generator
open Incremental
open Typing
open Varset

let compat_env slow_prob env envp e = (* To fairly simulate real world cases correct randomly the else branch *)
  let fv = snd (Annotast.get_annot e) in
  let res = M.equal (=) env envp in
  if (res && Random.float 1.0 <= 1. -. slow_prob) then res
  else (* Slower algorithm: if k2 is a free variable check if v1=v2, otherwise return true *)
    BatEnum.equal (fun (k1,v1) (k2,v2) ->  if (VarSet.mem k2 fv) then v1=v2 else true) (M.enum env) (M.enum envp)  

let bench_original_vs_inc e repeat time slow_prob = 
  (* Fill up the initial gamma with needed identifiers *)
  let initial_gamma_list e = (List.map (fun id -> (id, Type.Int)) (VarSet.elements (Annotast.get_fv e))) in
  let gamma_init = (M.add_list (initial_gamma_list e) M.empty ) in
  (* These are just to avoid multiple recomputations *)
  let typed_e = Typing.typecheck gamma_init e in
  let init_sz = M.cardinal gamma_init in
  let empty_cache = Cache.create_empty init_sz in
  let full_cache = Cache.create_empty init_sz in
  Cache.build_cache typed_e gamma_init full_cache;
  Benchmark.throughputN ~style:Benchmark.Nil ~repeat:repeat time [
    ("orig", (fun () -> ignore (Typing.typecheck gamma_init e)), ());
    ("inc", (fun () -> ignore (IncrementalTyping.typecheck full_cache gamma_init e)), ());
    ("einc", (fun () -> ignore (let copy_cache = Cache.copy empty_cache in assert (Cache.is_empty copy_cache); IncrementalTyping.typecheck ~compat_env:(compat_env slow_prob) copy_cache gamma_init e)), ())
  ] 

let bench_original_vs_incr_add e d repeat time slow_prob = 
  (* Fill up the initial gamma with needed identifiers *)
  let initial_gamma_list e = (List.map (fun id -> (id, Type.Int)) (VarSet.elements (Annotast.get_fv e))) in
  let gamma_init = (M.add_list (initial_gamma_list e) M.empty ) in
  (* These are just to avoid multiple recomputations *)
  let typed_e = Typing.typecheck gamma_init e in
  let init_sz = M.cardinal   gamma_init in
  let full_cache = Cache.create_empty init_sz in
  (* Build the full cache for e *)
  Cache.build_cache typed_e gamma_init full_cache;
  let gamma_init_m = gamma_init in (* In this environment we will have some useless bindings, but this doesn't affect the benchmarking *)
  (* Invalidate part of the cache, corresponding to the rightmost subtree of depth tree_depth - d; This simulates addition of code. *)
  invalidate_rsubast full_cache e d;
  Benchmark.throughputN ~style:Benchmark.Nil ~repeat:repeat time [
    ("orig", (fun () -> ignore (Typing.typecheck gamma_init e)), ());
    ("inc", (fun () -> ignore (let copy_cache = Cache.copy full_cache in IncrementalTyping.typecheck copy_cache gamma_init_m e ~compat_env:(compat_env slow_prob) )), ());
  ] 

(* 
  This one cannot be simulated by manipulating the cache, so it requires to actually modify the tree. 
  In this case the idea is pretty simple: descend along e to the rightmost subtree of depth d, then simply remove it and replace with a leaf.
*)
let bench_original_vs_incr_elim e d repeat time slow_prob = 
  let initial_gamma_list e = (List.map (fun id -> (id, Type.Int)) (VarSet.elements (Annotast.get_fv e))) in
  let gamma_init = (M.add_list (initial_gamma_list e) M.empty ) in
  (* These are just to avoid multiple recomputations *)
  let typed_e = Typing.typecheck gamma_init e in
  let init_sz = M.cardinal   gamma_init in
  let full_cache = Cache.create_empty init_sz in
  (* Build the full cache for e *)
  Cache.build_cache typed_e gamma_init full_cache;
  (* The modified program: eliminate the rightmost subtree at depth d by substituting it with a constant leaf *)
  let em = tree_subst_rm e d (Annotast.Int(42, (Hashing.compute_hash 42, VarSet.empty))) in 
  let gamma_init_m = (M.add_list (initial_gamma_list em) M.empty ) in
  Benchmark.throughputN ~style:Benchmark.Nil ~repeat:repeat time [
    ("orig", (fun () -> ignore (Typing.typecheck gamma_init em)), ());
    ("inc", (fun () -> ignore (let copy_cache = Cache.copy full_cache in IncrementalTyping.typecheck copy_cache gamma_init_m em ~compat_env:(compat_env slow_prob) )), ());
  ] 

(* 
  This one cannot be simulated by manipulating the cache, so it requires to actually modify the tree. 
  Again, pretty simple: descend along e to the rightmost subtree of depth d, descend along to the leftmost subtree of depth d+1 (to avoid locality and simmetricity in changes that makes the computations artificially faster) and swap them.
*)
let bench_original_vs_incr_move e d repeat time slow_prob =
  let initial_gamma_list e = (List.map (fun id -> (id, Type.Int)) (VarSet.elements (Annotast.get_fv e))) in
  let gamma_init = (M.add_list (initial_gamma_list e) M.empty ) in
  (* These are just to avoid multiple recomputations *)
  let typed_e = Typing.typecheck gamma_init e in
  let init_sz = M.cardinal   gamma_init in
  let full_cache = Cache.create_empty init_sz in
  (* Build the full cache for e *)
  Cache.build_cache typed_e gamma_init full_cache;
  (* The modified program: swap the two subtrees *)
  let rm_tree = get_rm e d in 
  let lm_tree = get_lm e (d + 1) in
  let em' = tree_subst_rm e d lm_tree in 
  let em = tree_subst_lm em' (d + 1) rm_tree in
  let gamma_init_m = (M.add_list (initial_gamma_list em) M.empty ) in
  Benchmark.throughputN ~style:Benchmark.Nil ~repeat:repeat time [
    ("orig", (fun () -> ignore (Typing.typecheck gamma_init em)), ());
    ("inc", (fun () -> ignore (let copy_cache = Cache.copy full_cache in IncrementalTyping.typecheck copy_cache gamma_init_m em ~compat_env:(compat_env slow_prob) )), ());
  ] 

let gen_list min max next = 
  let rec gen_aux curr = 
    if curr >= max then [max] else curr :: (gen_aux (next curr))
  in gen_aux min

let rec cartesian a b = match b with 
| [] -> []
| be :: bs ->  (List.map (fun ae -> (be, ae)) a) @ (cartesian a bs)

let print_res ?(inv_depth=(-1)) csv results repeat time slow_prob transf fv_c depth =
  if csv then 
    List.iter (
      fun (name, reslist) -> 
      List.iter (
        fun res -> 
        let rate = (Int64.to_float res.iters) /. (res.utime +. res.stime) in 
          Printf.printf "%s, %d, %d, %f, %s, %d, %d, %d, %f\n" name repeat time slow_prob transf fv_c depth inv_depth rate) reslist; flush stdout
      ) results
  else
    (Printf.printf "slow_prob=%f; transf=%s; fv_c=%d; depth=%d; inv_depth=%d\n" slow_prob transf fv_c depth inv_depth; (results |> Benchmark.tabulate))

let _ = 
  Random.self_init ();
  if Array.length Sys.argv < 6 then
    Printf.printf "%s repeat time min_depth max_depth csv\n" Sys.argv.(0)
  else
    let repeat, time, min_depth, max_depth, csv = int_of_string Sys.argv.(1), int_of_string Sys.argv.(2), int_of_string Sys.argv.(3), int_of_string Sys.argv.(4), bool_of_string Sys.argv.(5) in
    let depth_list = gen_list min_depth max_depth (fun n -> n+2) in  (* Seems that big AST trees has ~20k nodes, [Erdweg et al., §6] in Paper *)
    let fv_c_list = 1 :: (gen_list 1024 (BatInt.pow 2 (max_depth-1)) (fun n -> 4*n)) in  (* Saturating the leaves w all different variables: 2^(depth-1) *)
    let inv_depth_list =  gen_list 1 (max_depth/2) (fun n -> n*2) @ gen_list (max_depth/2) max_depth (fun n -> n + 2) in (* Invalidating a tree of 2^(depth - inv_depth) - 1 nodes, i.e. ~2^(-inv_depth) % *)
    let slow_prob_list = [0.0; 0.50; 0.75; 0.9; 1.0] in (* Probability of falling back to executing the "slow" algorithm *)
    let tpl_cmp (a_id, (a_fvc, a_d)) (b_id, (b_fvc, b_d)) = if (a_id = b_id && a_fvc=b_fvc && a_d = b_d) then 0 else -1 in
    let param_list = List.sort_uniq tpl_cmp  (cartesian (cartesian depth_list fv_c_list) inv_depth_list) in
    let param_list = List.filter (fun (inv_depth, (fv_c, depth)) -> fv_c <= (BatInt.pow 2 (depth-1)) && inv_depth < depth) param_list in
    let param_list = cartesian slow_prob_list param_list in
    let len = List.length param_list in
    if csv then 
      Printf.printf "name, repeat, time, slow_prob, transf, fvc, depth, inv_depth, rate\n"
    else ();
    List.iteri (fun i ((inv_depth, (fv_c, depth)), slow_prob) -> (
      Printf.eprintf "[%d/%d]\n" (i+1) len;
      flush stderr;
      let e = Generator.gen_ibop_ids_ast depth "+" fv_c in 
      (* Original typing algorithm vs. Incremental w full cache & no mofications vs. Incremental w empty cache *)
      print_res csv (bench_original_vs_inc e repeat time slow_prob) repeat time slow_prob "id" fv_c depth;
      (* (Simulated) code addition: full re-typing vs. incremental w full cache *)
      print_res csv (bench_original_vs_incr_add e inv_depth repeat time slow_prob) repeat time slow_prob "add" fv_c depth ~inv_depth:inv_depth;
      (* Code elimination: full re-typing vs. incremental w full cache *)
      print_res csv (bench_original_vs_incr_elim e inv_depth repeat time slow_prob) repeat time slow_prob "elim" fv_c depth ~inv_depth:inv_depth;
      (* Code motion: full re-typing vs. incremental w full cache *)
      print_res csv (bench_original_vs_incr_move e inv_depth repeat time slow_prob) repeat time slow_prob "move" fv_c depth ~inv_depth:inv_depth
      )
    ) param_list