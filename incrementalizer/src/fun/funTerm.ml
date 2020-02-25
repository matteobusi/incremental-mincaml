(* Define the language *)
module FunTerm : Term = struct
    type 'a t =
        | Unit of 'a
        | Bool of bool * 'a
        | Int of int * 'a
        | Float of float * 'a
        | Not of 'a t * 'a
        | Neg of 'a t * 'a
        | IBop of string * 'a t * 'a t * 'a
        | FNeg of 'a t * 'a
        | FBop of string * 'a t * 'a t * 'a
        | Rel of string * 'a t * 'a t * 'a
        | If of 'a t * 'a t * 'a t * 'a
        | Let of string * 'a t * 'a t *'a
        | Var of string * 'a
        | LetRec of 'a fundef * 'a t * 'a
        | App of 'a t * 'a t list * 'a
        | Tuple of 'a t list * 'a
        | LetTuple of string list * 'a t * 'a t * 'a
        | Array of 'a t * 'a t * 'a
        | Get of 'a t * 'a t * 'a
        | Put of 'a t * 'a t * 'a t * 'a
        and 'a fundef = { name : string * Type.t; args : (string * Type.t) list; body : 'a t }

    let get_annot e =
        match e with
        | Unit(annot)
        | Bool(_, annot)
        | Int(_, annot)
        | Float(_, annot)
        | Not(_, annot)
        | Var(_, annot)
        | Neg(_, annot)
        | FNeg(_, annot)
        | IBop(_,_,_, annot)
        | FBop(_,_,_, annot)
        | Rel(_,_,_, annot)
        | If(_,_,_, annot)
        | Let(_,_,_, annot)
        | LetRec(_,_, annot)
        | App(_,_, annot)
        | Tuple(_, annot)
        | LetTuple(_,_,_, annot)
        | Array(_,_, annot)
        | Get(_,_, annot)
        | Put(_, _,_, annot) -> annot

    let compute_fv e =
        let rec free_variables_cps e k =
            let invRemove e s = VarSet.remove s e in
            match e with
                | Unit(_)
                | Bool(_)
                | Int(_)
                | Float(_) -> k VarSet.empty
                | Var(x, _) -> k (VarSet.singleton x)
                | Not(e1, _)
                | Neg(e1, _)
                | FNeg(e1, _) -> free_variables_cps e1 (fun r1 -> k r1)
                | IBop(_, e1, e2, _)
                | FBop(_, e1, e2, _)
                | Rel(_, e1, e2, _) -> free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> k (VarSet.union r1 r2)))
                | If(e1, e2, e3, _) -> free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> free_variables_cps e3 (fun r3 -> k (VarSet.union r1 (VarSet.union r2 r3)))))
                | Let(x, e1, e2, _) -> free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> k (VarSet.remove x (VarSet.union r1 r2))))
                | LetRec ({ name = (x, _); args = yts; body = e1 }, e2, _) ->
                free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> k (List.fold_left invRemove (VarSet.union r1 r2) (x::(List.map fst yts)))))
                | App (e1, es, _) -> free_variables_cps e1 (fun r1 -> k (VarSet.union r1 (List.fold_left VarSet.union VarSet.empty (List.map (fun e -> free_variables_cps e k) es))))
                | Tuple(es, _) -> k (List.fold_left VarSet.union VarSet.empty (List.map (fun e -> free_variables_cps e k) es))
                | LetTuple(xs, e1, e2, _) -> free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> k (List.fold_left invRemove (VarSet.union r1 r2)xs)))
                | Array(e1, e2, _)
                | Get (e1, e2, _) -> free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> k (VarSet.union r1 r2)))
                | Put (e1, e2, e3, _) -> free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> free_variables_cps e3 (fun r3 -> k (VarSet.union r1 (VarSet.union r2 r3)))))
        in free_variables_cps e (fun d -> d)

    let rec compute_hash e =
        let hash_of_fundef fundef = ... in
            match e with
            | Unit(_) -> Hashing.hash "unit"
            | Bool(v, _)
            | Int(v, _)
            | Float(v, _) -> Hashing.hash v
            | Var(id, _) -> Hashing.hash id
            | Not(e1, _) -> Hashing.combine_hashes [Hashing.hash "not"; compute_hash e1]
            | Neg(e1, _) -> Hashing.combine_hashes [Hashing.hash "neg"; compute_hash e1]
            | FNeg(e1, _) -> Hashing.combine_hashes [Hashing.hash "fneg"; compute_hash e1]
            | IBop(s, e1, e2, _)
            | FBop(s, e1, e2, _)
            | Rel(s, e1, e2, _) -> Hashing.combine_hashes [Hashing.hash s; compute_hash e1; compute_hash e2]
            | If(b, e1, e2, _) -> Hashing.combine_hashes [compute_hash b; compute_hash e1; compute_hash e2]
            | Let(s, e1, e2, _) -> Hashing.combine_hashes [Hashing.hash "let"; Hashing.hash s; compute_hash e1; compute_hash e2]
            | LetRec(fundef, e1, _) -> Hashing.combine_hashes [Hashing.hash "lrec"; hash_of_fundef fundef; compute_hash exp]
            | App(e1, e2, _) -> combine_hashes ([Hashing.hash "app" ; compute_hash e1] @ (List.map compute_hash e2))
            | Tuple(e1, _) -> combine_hashes ((compute_hash "tuple")::(List.map get_hash e1))
            | LetTuple(pat, e1, e2, _) ->
                combine_hashes ((Hashing.hash "ltup") :: (compute_hash e1) :: (compute_hash e2) :: (List.map Hashing.hash pat))
            | Array(e1, e2, _) -> Hashing.combine_hashes [Hashing.hash "mkar"; compute_hash e1; compute_hash e2]
            | Get(e1, e2, _) -> Hashing.combine_hashes [Hashing.hash "get"; compute_hash e1; compute_hash e2]
            | Put(e1, e2, e3, _) -> Hashing.combine_hashes [Hashing.hash "put"; compute_hash e1; compute_hash e2; compute_hash e3]


    (* STILL MISSING
    val map : 'a t -> ('a -> 'b) -> 'b t

    (* Must be in **REVERSED** order for typing *)
    val get_ordered_children : 'a t -> ('a t) list *)
end
