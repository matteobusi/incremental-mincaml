open Core

open FunSpecification.FunSpecification
open VarSet

module OriginalFunAlgorithm = Original.TypeAlgorithm(FunSpecification.FunSpecification)
module IncrementalFunAlgorithm = Incrementalizer.TypeAlgorithm(FunSpecification.FunSpecification)

let gen_list min max next =
  let rec gen_aux curr =
    if curr >= max then [max] else curr :: (gen_aux (next curr))
  in gen_aux min

let rec cartesian a b = match b with
| [] -> []
| be :: bs ->  (List.map a ~f:(fun ae -> (be, ae))) @ (cartesian a bs)

let rec pow base exp =
  if exp <= 0 then 1
  else base * (pow base (exp-1))

let rec nodecount e = match e with
  | Unit(annot)
  | Bool(_, annot)
  | Int(_, annot)
  | Float(_, annot)
  | Var(_, annot) -> 1
  | Not(e1, annot)
  | Neg(e1, annot)
  | FNeg(e1, annot) -> 1 + nodecount e1
  | IBop(_, e1, e2, annot)
  | FBop(_, e1, e2, annot)
  | Rel(_, e1, e2, annot) -> 1 + nodecount e1 + nodecount e2
  | If(e1, e2, e3, annot) -> 1 + nodecount e1 + nodecount e2 + nodecount e3
  | Let(_, e1, e2, annot) -> 2 + nodecount e1 + nodecount e2 (* curr node + x *)
  | LetRec ({ name = _; args = yts; body = e1 }, e2, annot) -> 2 + (List.length yts) + nodecount e1 + nodecount e2
  | App (e1, es, annot) -> 1 + (List.fold_left ~f:(+) ~init:0 (List.map ~f:nodecount (e1::es)))
  | Tuple(es, annot) -> 1 + (List.fold_left ~f:(+) ~init:0 (List.map ~f:nodecount es))
  | LetTuple(xs, e1, e2, annot) -> 1 + List.length xs + nodecount e1 + nodecount e2
  | Array(e1, e2, annot)
  | Get (e1, e2, annot) -> nodecount e1 + nodecount e2
  | Put (e1, e2, e3, annot) -> nodecount e1 + nodecount e2 + nodecount e3

(** GENERATOR 1: Generator and cache invalidator for the standard performance comparison *)
(* Generate a balanced aAST of the specified depth (1 means just the root) whose nodes are ibop and with k distinct free variables *)
let ibop_gen_ast h ibop k =
  let counter = ref 0 in
  let rec aux h ibop k =
    match h with
    | 0 -> failwith "(gen_ibop_ids_ast) h must be >= 1."
    | 1 -> incr counter;
          counter := !counter mod k;
          let curr_id = "x" ^ (string_of_int !counter) in (* Makes sure that all the variables are different! *)
            Var(curr_id, ())
    | _ ->
      let laast, raast = aux (h-1) ibop k, aux (h-1) ibop k in
        IBop(ibop, laast, raast, ()) in
  let e = aux h ibop k in
    OriginalFunAlgorithm.term_map (fun e -> (compute_hash e, compute_fv e)) e

(* Invalidate the entries corresponding to the rightmost subtree at depth d (0 for the root) in e to simulate a modification to e *)
let rec ibop_sim_change cache e d =
  let rec invalidate_cache cache e =
    IncrementalFunAlgorithm.Cache.remove cache (fst (term_getannot e));
    match e with (* Restricted to IBop and Var *)
    | Var(id, _) -> ()
    | IBop(op, l, r, _) -> invalidate_cache cache l; invalidate_cache cache r
    | _ -> failwith "(ibop_sim_change) Unsupported aAST."
  in
  match (e, d) with
  | (_, 0) -> invalidate_cache cache e
  | (Var(_, _), _) -> failwith "(ibop_sim_change) d is too big!"
  | (IBop(_, _, r, _), _) ->
    IncrementalFunAlgorithm.Cache.remove cache (fst (term_getannot e));
    ibop_sim_change cache r (d-1)
  | _ -> failwith "(ibop_sim_change) Unsupported aAST."

(** GENERATORS 2: generator and corresponding cache invalidator for code with a lot of internal deps. *)
(* Generates an expression computing the factorial n, that corresponds to the unrolled recursion of classical factorial *)
let fact_gen_ast n =
  let rec aux (i : int) (n : int) : unit FunSpecification.FunSpecification.term =
    if i = 1 then
          Let ("x_" ^ (string_of_int i),
            Int (i, ()),
            aux (i+1) n,
          ())
    else if (i <= n) then
      Let ("x_" ^ (string_of_int i),
          IBop ("*",
            (Int (i, ())),
            (Var ("x_" ^ (string_of_int (i-1)), ())), ()),
            aux (i+1) n,
          ())
    else
      (Var ("x_" ^ (string_of_int (i-1)), ())) in
  aux 1 n

(*
This simulates a complete change of x_i (e.g., its type) by invalidating cache entries corresponding to
  - x_i itself
  - the tree associated with x_i by the let
  - all the nodes on the path to x_i (root included)
  - for i' > i does not invalidate let x_i' = ... in ...

For i' > i one may think that we should not invalidate let x_i' = ... in ... because
the term should _syntactically_ present in the cache, though its existing type might not be correct anymore.
However since our changes are synthetic and nodes related to i' are expected to depend on i, we still need to invalidate all the nodes.
*)
let rec fact_sim_change cache e i =
  (* Remove all the nodes on the path to let x_i = ... in ... *)
    IncrementalFunAlgorithm.Cache.remove cache (fst (term_getannot e));
    match e with
    | Let (xi, e1, e2, _) ->
      (
        let i' = int_of_string (String.slice xi 2 0) in
          if i' < i then fact_sim_change cache e2 i
          else (fact_sim_change cache e1 i; fact_sim_change cache e2 i)
      )
    | Int (_, _) | Var(_, _) -> ()
    | IBop(op, l, r, _) -> fact_sim_change cache l i; fact_sim_change cache r i
    | _ -> failwith "(fact_sim_change) Error: unsupported aAST."

(* Generator #3: exponential growth of types from Kanellakis&Mitchell 1989 *)
let exp_gen_ast n =
  let rec aux (i : int) (n : int) : unit FunSpecification.FunSpecification.term =
    if i = 1 then
      Let ("x_" ^ (string_of_int i),
        Int (i, ()),
        aux (i+1) n,
      ())
    else if (i <= n) then
      Let ("x_" ^ (string_of_int i),
          Tuple
          ([
            (Var ("x_" ^ (string_of_int (i-1)), ()));
            (Var ("x_" ^ (string_of_int (i-1)), ()))
          ], ()),
          aux (i+1) n,
          ())
    else
      (Var ("x_" ^ (string_of_int (i-1)), ())) in
  aux 1 n

let rec exp_sim_change cache e i =
  (* Remove all the nodes on the path to let x_i = ... in ... *)
    IncrementalFunAlgorithm.Cache.remove cache (fst (term_getannot e));
    match e with
    | Let (xi, e1, e2, _) ->
      (
        let i' = int_of_string (String.slice xi 2 0) in
          if i' < i then exp_sim_change cache e2 i
          else (exp_sim_change cache e1 i; exp_sim_change cache e2 i)
      )
    | Int (_, _) | Var(_, _) -> ()
    | Tuple (es, _) -> List.iter ~f:(fun e -> exp_sim_change cache e i) es
    | _ -> failwith "(exp_sim_change) Error: unsupported aAST."
