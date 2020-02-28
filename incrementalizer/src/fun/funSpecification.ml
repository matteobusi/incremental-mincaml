open Batteries

open FunContext
open LanguageSpecification

module FunSpecification (* : LanguageSpecification *) = struct
    (* Syntax + meta-functions *)
    type 'a term =
        | Unit of 'a
        | Bool of bool * 'a
        | Int of int * 'a
        | Float of float * 'a
        | Not of 'a term * 'a
        | Neg of 'a term * 'a
        | IBop of string * 'a term * 'a term * 'a
        | FNeg of 'a term * 'a
        | FBop of string * 'a term * 'a term * 'a
        | Rel of string * 'a term * 'a term * 'a
        | If of 'a term * 'a term * 'a term * 'a
        | Let of string * 'a term * 'a term *'a
        | Var of string * 'a
        | LetRec of 'a fundef * 'a term * 'a
        | App of 'a term * 'a term list * 'a
        | Tuple of 'a term list * 'a
        | LetTuple of string list * 'a term * 'a term * 'a
        | Array of 'a term * 'a term * 'a
        | Get of 'a term * 'a term * 'a
        | Put of 'a term * 'a term * 'a term * 'a
    and 'a fundef = { name : string * res; args : (string * res) list; body : 'a term }
    and res =
        | TUnit
        | TBool
        | TInt
        | TFloat
        | TFun of res list * res (* arguments are uncurried *)
        | TTuple of res list
        | TArray of res
    and context = res FunContext.t

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
        let hash_of_fundef ({name=(id, rt); args=xs; body=e}) =
            let hash_id = Hashing.hash id in
            let hash_tr = Hashing.hash rt in
            let hash_args = Hashing.combine_hashes (List.map (fun (id, t) ->
                                    Hashing.combine_hashes [Hashing.hash id; Hashing.hash t]) xs) in
            Hashing.combine_hashes [hash_id ; hash_tr; hash_args; compute_hash e] in
        match e with
        | Unit(_) -> Hashing.hash "unit"
        | Bool(v, _) -> Hashing.hash v
        | Int(v, _) -> Hashing.hash v
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
        | LetRec(fundef, e1, _) -> Hashing.combine_hashes [Hashing.hash "lrec"; hash_of_fundef fundef; compute_hash e1]
        | App(e1, e2, _) -> Hashing.combine_hashes ([Hashing.hash "app" ; compute_hash e1] @ (List.map compute_hash e2))
        | Tuple(es, _) -> Hashing.combine_hashes ((Hashing.hash "tuple")::(List.map compute_hash es))
        | LetTuple(pat, e1, e2, _) ->
            Hashing.combine_hashes ((Hashing.hash "ltup") :: (compute_hash e1) :: (compute_hash e2) :: (List.map Hashing.hash pat))
        | Array(e1, e2, _) -> Hashing.combine_hashes [Hashing.hash "mkar"; compute_hash e1; compute_hash e2]
        | Get(e1, e2, _) -> Hashing.combine_hashes [Hashing.hash "get"; compute_hash e1; compute_hash e2]
        | Put(e1, e2, e3, _) -> Hashing.combine_hashes [Hashing.hash "put"; compute_hash e1; compute_hash e2; compute_hash e3]

    let get_rev_children e =
        match e with
        | Unit(_) -> []
        | Bool(_, _)
        | Int(_, _)
        | Float(_, _)
        | Var(_, _) -> []
        | Not(e1, _)
        | Neg(e1, _)
        | FNeg(e1, _) -> [(0, e1)]
        | IBop(s, e1, e2, _)
        | FBop(s, e1, e2, _)
        | Rel(s, e1, e2, _) -> [(1, e2); (0, e1)]
        | If(b, e1, e2, _) -> [(2, e2); (1, e1); (0, b)]
        | Let(s, e1, e2, _) -> [(1, e2); (0, e1)]
        | LetRec({name=(id, rt); args=xs; body=e1}, e2, _) -> [(1, e2); (0, e1)]
        | App(e1, e2, _) -> let len = List.length e2 in
                                List.rev ((len, e1)::(List.combine (List.of_enum (0--^len)) e2))
        | Tuple(es, _) -> List.rev (List.combine (List.of_enum (0--^List.length es)) es)
        | LetTuple(pat, e1, e2, _) -> [(1, e2); (0, e1)]
        | Array(e1, e2, _)
        | Get(e1, e2, _) -> [(1, e2); (0, e1)]
        | Put(e1, e2, e3, _) -> [(2, e3); (1, e2); (0, e1)]


    let compat gamma gamma' at = true

    (* i indicates that the i-th element is being processed (0-based) *)
    let tr (i : int) (ti : (int * VarSet.t) term) (t : (int * VarSet.t) term) (gamma : context) (rs : res list) : context =
        (* This will never be invoked on base cases! *)
        match t with
        | Unit(_) -> failwith "Tr invoked on base case: Unit"
        | Bool(_, _)
        | Int(_, _)
        | Float(_, _)
        | Var(_, _) -> failwith "Tr invoked on base case."
        | Not(_, _)
        | Neg(_, _)
        | FNeg(_, _) -> gamma
        | IBop(_, _, _, _)
        | FBop(_, _, _, _)
        | Rel(_, _, _, _) -> gamma
        | If(_, _, _, _) -> gamma
        | Let(x, e1, e2, _) -> match i with
                                | 0 -> gamma
                                | 1 -> (FunContext.add x (List.at rs i) gamma)
                                | _ -> failwith "Wrong index for Tr in Let!"
        | _ -> failwith "Not implemented!"
        (* | LetRec({name=(id, rt); args=xs; body=e1}, e2, _) -> [e2; e1]
        | App(e1, e2, _) -> List.rev (e1::e2)
        | Tuple(_, _) -> gamma
        | LetTuple(pat, e1, e2, _) -> [e2; e1]
        | Array(e1, e2, _)
        | Get(e1, e2, _) -> gamma
        | Put(e1, e2, e3, _) -> gamma *)

    let checkjoin (t : (int * VarSet.t) term) (gamma : context) (rs : res list) = Some TUnit
end
