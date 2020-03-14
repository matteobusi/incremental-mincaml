open Batteries

module OriginalFunAlgorithm = Original.TypeAlgorithm(FunSpecification.FunSpecification)
module IncrementalFunAlgorithm = Incrementalizer.TypeAlgorithm(FunSpecification.FunSpecification)

open FunSpecification.FunSpecification

open Hashing
open VarSet

(*
  Generate a balanced aAST of the specified depth (1 means just the root) whose nodes are ibop and with k
*)
let gen_ibop_ids_ast h ibop k =
  let counter = ref 0 in
  let rec aux h ibop k =
    match h with
    | 0 -> failwith "gen_ibop_ids_ast: h must be >= 1."
    | 1 -> incr counter;
          counter := !counter mod k;
          let curr_id = "x" ^ (string_of_int !counter) in (* Assure that all the variables are different! *)
            Var(curr_id, ())
    | _ ->
      let laast, raast = aux (h-1) ibop k, aux (h-1) ibop k in
        IBop(ibop, laast, raast, ()) in
  let e = aux h ibop k in
    OriginalFunAlgorithm.term_map (fun e -> (compute_hash e, compute_fv e)) e

(* Invalidate the entries corresponding to the rightmost subtree at depth d (0 for the root) in e to simulate a modification to e *)
let rec simulate_modification cache e d =
  let rec invalidate_cache cache e =
    Cache.remove_all cache (fst (term_getannot e));
    match e with (* Restricted to IBop and Var *)
    | Var(id, _) -> ()
    | IBop(op, l, r, _) -> invalidate_cache cache l; invalidate_cache cache r
    | _ -> failwith "invalidate_cache: unsupported aAST."
  in
  match (e, d) with
  | (_, 0) -> invalidate_cache cache e
  | (Var(_, _), _) -> failwith "simulate_modification: d is too big!"
  | (IBop(_, _, r, _), _) ->  Cache.remove_all cache (fst (term_getannot e)); simulate_modification cache r (d-1)
  | _ -> failwith "simulate_modification: unsupported aAST."

(*
  Substitues the rightmost tree at depth d in e with n
*)
(* let rec tree_subst_rm e d n =
  match (e, d) with
  | (_, 0) -> n
  | (Var(_, _), _) -> failwith "tree_subst_rm: d is too big!"
  | (IBop(op, l, r, a), _) ->
    let raast = tree_subst_rm r (d-1) n in
    let hash_l, hash_r = fst (Annotast.get_annot l), fst (Annotast.get_annot raast) in
    let fv_l, fv_r = snd (Annotast.get_annot l), snd (Annotast.get_annot raast) in
    let hash = Hashing.combine_hashes [ Hashing.compute_hash op; hash_l; hash_r] in
    Annotast.IBop(op, l, raast, (hash, VarSet.union fv_l fv_r))
  | _ -> failwith "tree_subst_rm: unsupported aAST." *)

(*
  Substitues the leftmost tree at depth d in e with n
*)
(* let rec tree_subst_lm e d n =
  match (e, d) with
  | (_, 0) -> n
  | (Var(_, _), _) -> failwith "tree_subst_lm: d is too big!"
  | (IBop(op, l, r, a), _) ->
    let laast = tree_subst_lm l (d-1) n in
    let hash_l, hash_r = fst (Annotast.get_annot laast), fst (Annotast.get_annot r) in
    let fv_l, fv_r = snd (Annotast.get_annot laast), snd (Annotast.get_annot r) in
    let hash = Hashing.combine_hashes [ Hashing.compute_hash op; hash_l; hash_r] in
    Annotast.IBop(op, laast, r, (hash, VarSet.union fv_l fv_r))
  | _ -> failwith "tree_subst_lm: unsupported aAST." *)

(*
let rec get_lm e d =
    match (e, d) with
  | (_, 0) -> e
  | (Var(_, _), _) -> failwith "get_lm: d is too big!"
  | (IBop(_, l, _, _), _) -> get_lm l (d-1)
  | _ -> failwith "get_lm: unsupported aAST."

let rec get_rm e d =
    match (e, d) with
  | (_, 0) -> e
  | (Var(_, _), _) -> failwith "get_rm: d is too big!"
  | (IBop(_, _, r, _), _) -> get_rm r (d-1)
  | _ -> failwith "get_rm: unsupported aAST." *)
