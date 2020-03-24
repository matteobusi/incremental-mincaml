open Batteries

module OriginalFunAlgorithm = Original.TypeAlgorithm(FunSpecification.FunSpecification)
module IncrementalFunAlgorithm = Incrementalizer.TypeAlgorithm(FunSpecification.FunSpecification)

open FunSpecification.FunSpecification
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
    IncrementalFunAlgorithm.Cache.remove_all cache (fst (term_getannot e));
    match e with (* Restricted to IBop and Var *)
    | Var(id, _) -> ()
    | IBop(op, l, r, _) -> invalidate_cache cache l; invalidate_cache cache r
    | _ -> failwith "invalidate_cache: unsupported aAST."
  in
  match (e, d) with
  | (_, 0) -> invalidate_cache cache e
  | (Var(_, _), _) -> failwith "simulate_modification: d is too big!"
  | (IBop(_, _, r, _), _) ->
    IncrementalFunAlgorithm.Cache.remove_all cache (fst (term_getannot e));
    simulate_modification cache r (d-1)
  | _ -> failwith "simulate_modification: unsupported aAST."
