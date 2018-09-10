open Annotast
open Hashing
open Cache

(*
  Generate a balanced aAST of the specified depth (1 means just the root) whose nodes are ibop and leaves are all identical to id
*)
let rec gen_ibop_id_ast h ibop id = 
  match h with
  | 0 -> failwith "gen_ibop_id_ast: h must be >= 1."
  | 1 -> Annotast.Var(id, Hashing.compute_hash id)
  | _ ->
    let laast = gen_ibop_id_ast (h-1) ibop id in 
    let raast = gen_ibop_id_ast (h-1) ibop id in
    let hash = Hashing.combine_hashes [ Hashing.compute_hash ibop; Hashing.extract_simple_hash laast; Hashing.extract_simple_hash raast] in
    Annotast.IBop(ibop, laast, raast, hash)

(*
  Generate a balanced aAST of the specified depth (1 means just the root) whose nodes are ibop and with k 
*)
let gen_ibop_ids_ast h ibop k = 
  let counter = ref 0 in
  let rec aux h ibop k = 
    match h with
    | 0 -> failwith "gen_ibop_ids_ast: h must be >= 1."
    | 1 -> incr counter; counter := !counter mod k; let curr_id = "x" ^ (string_of_int !counter) in Annotast.Var(curr_id, Hashing.compute_hash curr_id)
    | _ ->
      let laast = aux (h-1) ibop k in 
      let raast = aux (h-1) ibop k in
      let hash = Hashing.combine_hashes [ Hashing.compute_hash ibop; Hashing.extract_simple_hash laast; Hashing.extract_simple_hash raast] in
      Annotast.IBop(ibop, laast, raast, hash) in 
  aux h ibop k

(* Invalidate the entries corresponding to the rightmost subtree at depth d (0 for the root) in e  *)
let rec invalidate_rsubast cache e d = 
  let rec invalidate_cache cache e = 
    Cache.remove_all cache (Annotast.get_annot e);
    match e with (* Restricted to IBop and Var*)
    | Var(id, _) -> ()
    | IBop(op, l, r, _) -> invalidate_cache cache l; invalidate_cache cache r
    | _ -> failwith "invalidate_cache: unsupported aAST."
  in
  match (e, d) with
  | (_, 0) -> invalidate_cache cache e
  | (Var(_, _), _) -> failwith "get_r_subaast: d is too big!"
  | (IBop(_, _, r, _), _) ->  Cache.remove_all cache (Annotast.get_annot e); invalidate_rsubast cache r (d-1)
  | _ -> failwith "get_r_subaast: unsupported aAST."

(*
Substitues the rightmost tree at depth d in e with n
*)
let rec tree_subst_rm e d n =
  match (e, d) with
  | (_, 0) -> n
  | (Var(_, _), _) -> failwith "tree_subst_rm: d is too big!"
  | (IBop(op, l, r, a), _) ->  
    let rn = tree_subst_rm r (d-1) n in
    let annot = Hashing.combine_hashes [ Hashing.compute_hash op; Hashing.extract_simple_hash l; Hashing.extract_simple_hash rn] in
    IBop(op, l, rn, annot)
  | _ -> failwith "tree_subst_rm: unsupported aAST."

(*
Substitues the leftmost tree at depth d in e with n
*)
let rec tree_subst_lm e d n =
  match (e, d) with
  | (_, 0) -> n
  | (Var(_, _), _) -> failwith "tree_subst_lm: d is too big!"
  | (IBop(op, l, r, a), _) ->  
    let ln = tree_subst_lm l (d-1) n in
    let annot = Hashing.combine_hashes [ Hashing.compute_hash op; Hashing.extract_simple_hash ln; Hashing.extract_simple_hash r] in
    IBop(op, ln, r, annot)
  | _ -> failwith "tree_subst_lm: unsupported aAST."

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
  | _ -> failwith "get_rm: unsupported aAST."