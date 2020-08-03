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


    (* ============================================================================================================== *)
    let rec type_ppf ppf type_t =
        match type_t with
        | TUnit -> Format.fprintf ppf "unit"
        | TInt -> Format.fprintf ppf "int"
        | TBool -> Format.fprintf ppf "bool"
        | TFloat -> Format.fprintf ppf "float"
        | TArray(t) -> Format.fprintf ppf "[%a]" type_ppf t
        | TTuple(ts) -> Format.fprintf ppf "@[<2>";
        typelist_syntax_ppf ppf ts;
        Format.fprintf ppf "@]"
        | TFun(args,rt) -> Format.fprintf ppf "@[<2>";
        typelist_syntax_ppf ppf args;
        Format.fprintf ppf " -> %a@]" type_ppf rt
    and typelist_syntax_ppf ppf ts =
        Format.pp_print_list type_ppf ~pp_sep:(fun ppf () -> Format.pp_print_char ppf '*') ppf ts


    let string_of_term ppf_annot e : string =
        let rec ppf_term ppf_annot ppf e =
            let ppf_tree = ppf_term ppf_annot in
            match e with
            | Unit(annot)             	  -> Format.fprintf ppf "@[<2>Unit{%a}@]" ppf_annot annot
            | Bool(b, annot)         	    -> Format.fprintf ppf "@[<2>%b{%a}@]" b ppf_annot annot
            | Int(n, annot)          	    -> Format.fprintf ppf "@[<2>%d{%a}@]" n ppf_annot annot
            | Float(f, annot)        	    -> Format.fprintf ppf "@[<2>%f{%a}@]" f ppf_annot annot
            | Not(e, annot)          	    -> Format.fprintf ppf "@[<2>Not(%a){%a}@]" ppf_tree e ppf_annot annot
            | Neg(e ,annot)          	    -> Format.fprintf ppf "@[<2>-(%a){%a}@]" ppf_tree e ppf_annot annot
            | IBop(op, e1, e2, annot)     -> Format.fprintf ppf "@[<2>%s(%a, %a){%a}@]" op ppf_tree e1 ppf_tree e2 ppf_annot annot
            | FNeg(e, annot)         	    -> Format.fprintf ppf "@[<2>-.(%a){%a}@]" ppf_tree e ppf_annot annot
            | FBop(op, e1, e2, annot)     -> Format.fprintf ppf "@[<2>%s(%a, %a){%a}@]" op ppf_tree e1 ppf_tree e2 ppf_annot annot
            | Rel(op, e1, e2, annot)      -> Format.fprintf ppf "@[<2>%s(%a,%a){%a}@]" op ppf_tree e1 ppf_tree e2 ppf_annot annot
            | If(e1, e2,e3, annot)   	    -> Format.fprintf ppf "@[<2>If(%a,@,%a,@,%a){%a}@]" ppf_tree e1 ppf_tree e2 ppf_tree e3 ppf_annot annot
            | Let(id, e1, e2, annot)      -> Format.fprintf ppf "@[<2>Let(%s,%a,@,%a){%a}@]" id ppf_tree e1 ppf_tree e2 ppf_annot annot
            | Var(id, annot)        	    -> Format.fprintf ppf "@[<2>Var(%s){%a}@]" id  ppf_annot annot
            | Array(e1, e2, annot)        -> Format.fprintf ppf "@[<2>Array(%a,%a){%a}@]" ppf_tree e1 ppf_tree e2 ppf_annot annot
            | Get(e1,e2, annot)           -> Format.fprintf ppf "@[<2>Get(%a,%a){%a}@]" ppf_tree e1 ppf_tree e2 ppf_annot annot
            | Put(e1, e2, e3, annot)      -> Format.fprintf ppf "@[<2>Put(%a,%a,%a){%a}@]" ppf_tree e1 ppf_tree e2 ppf_tree e3 ppf_annot annot
            | LetRec(f, e, annot)   	    -> Format.fprintf ppf "@[<2>LetRec(%a,@,%a){%a}@]" (fundef_ppf ppf_tree) f ppf_tree e ppf_annot annot
            | App(e, es, annot)           -> Format.fprintf ppf "@[<2>App(%a," ppf_tree e;
                                            list_syntax_ppf ppf_tree ppf es;
                                            Format.fprintf ppf "){%a}@]" ppf_annot annot
            | Tuple(es, annot)      	    -> Format.fprintf ppf "@[<2>Tuple(";
                                            list_syntax_ppf ppf_tree ppf es;
                                            Format.fprintf ppf "){%a}@]" ppf_annot annot
            | LetTuple(bs, e1, e2, annot) -> Format.fprintf ppf "@[<2>LetTuple(%s,%a,@,%a){%a}@]" (String.concat " " bs)
                                            ppf_tree e1 ppf_tree e2 ppf_annot annot

        and list_syntax_ppf syntax_ppf ppf es =
        Format.pp_print_list syntax_ppf ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ") ppf es

        and fundef_ppf syntax_ppf ppf ({name = (id,t); args=formals; body=e}) =
        Format.fprintf ppf "@[<2>FunVal((%s : %a),(" id type_ppf t;
        list_binding_ppf ppf formals;
        Format.fprintf ppf "),@,%a)@]"  syntax_ppf e

        and list_binding_ppf ppf bs = if List.length bs > 0 then
            let (id, t) = List.hd bs in
            Format.fprintf ppf "%s : %a" id type_ppf t;
            List.iter (fun (id, t) -> Format.fprintf ppf ", %s : %a" id type_ppf t) (List.tl bs);
        else
            ()
        in
        ppf_term ppf_annot Format.str_formatter e;
        Format.flush_str_formatter ()


    (* Use the pretty printer to extract string from a type *)
    let string_of_type (t : res) = Format.fprintf Format.str_formatter "%a" type_ppf t; Format.flush_str_formatter ()


    let string_of_context (gamma : context) =
        let context_ppf ppf gamma =
            Format.fprintf ppf "[";
            (FunContext.iter (fun id res -> Format.fprintf ppf ", %s |> %a" id type_ppf res) gamma);
            Format.fprintf ppf "]";
        in Format.fprintf Format.str_formatter "%a" context_ppf gamma; Format.flush_str_formatter ()


    let term_getannot t =
        match t with
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


    let term_edit (t : 'a term) (ti : ('b term) list) (a : 'b) : ('b term) =
        match (t, ti) with
        | (Unit(_), _) -> Unit(a)
        | (Bool(v, _), _) -> Bool(v, a)
        | (Int(v, _), _) -> Int(v, a)
        | (Float(v, _), _) -> Float(v, a)
        | (Not(e1, _), [e1']) -> Not(e1', a)
        | (Var(x, _), _) -> Var(x, a)
        | (Neg(e1, _), [e1']) -> Neg(e1', a)
        | (FNeg(e1, _), [e1']) -> FNeg(e1', a)
        | (IBop(o, e1, e2, _), [e1'; e2']) -> IBop(o, e1', e2', a)
        | (FBop(o, e1, e2, _), [e1'; e2']) -> FBop(o, e1', e2', a)
        | (Rel(o, e1, e2, _), [e1'; e2']) -> Rel(o, e1', e2', a)
        | (If(b, e1, e2, _), [b'; e1'; e2']) -> If(b', e1', e2', a)
        | (Let(x, e1, e2, annot), [e1'; e2']) -> Let(x, e1', e2', a)
        | (LetRec({ name = (f, t); args = al; body = e1 }, e2, _), [e1'; e2']) ->
            LetRec({ name = (f, t); args = al; body = e1' }, e2', a)
        | (App(e1, e2, _), e1'::e2') -> App(e1', e2', a)
        | (Tuple(e1, _), _) -> Tuple (ti, a)
        | (LetTuple(xs, e1, e2, _), [e1'; e2']) -> LetTuple(xs, e1', e2', a)
        | (Array(e1, e2, _), [e1'; e2']) -> Array(e1', e2', a)
        | (Get(e1, e2, _), [e1'; e2']) -> Get(e1', e2', a)
        | (Put(e1, e2, e3, _), [e1'; e2';e3']) -> Put(e1', e2', e3', a)
        | _ -> failwith (Printf.sprintf "Wrong parameter list: %s." (string_of_term (fun _ _ -> ()) t))


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


    (* let rec compute_hash e = Hashtbl.hash e *)
    let rec compute_hash e = Hashtbl.hash_param max_int max_int e

    let get_sorted_children e =
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
        | Rel(s, e1, e2, _) -> [(0, e1); (1, e2)]
        | If(b, e1, e2, _) -> [(0, b); (1, e1); (2, e2)]
        | Let(s, e1, e2, _) -> [(0, e1); (1, e2)]
        | LetRec({name=(id, rt); args=xs; body=e1}, e2, _) -> [(0, e1); (1, e2)]
        | App(e, es, _) -> let len = List.length es in
            (0, e)::(List.combine (List.of_enum (1--len)) es)
        | Tuple(es, _) -> (List.combine (List.of_enum (0--^List.length es)) es)
        | LetTuple(pat, e1, e2, _) -> [(0, e1); (1, e2)]
        | Array(e1, e2, _)
        | Get(e1, e2, _) -> [(0, e1); (1, e2)]
        | Put(e1, e2, e3, _) -> [(0, e1); (1, e2); (2, e3)]

    let compat gamma gamma' at =
        (* Straightorward implementation from the theory: *)
        let fv = snd (term_getannot at) in
            VarSet.for_all (fun v -> (FunContext.find_option gamma v) = (FunContext.find_option gamma' v)) fv

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
        | Let(x, e1, e2, _) ->
            begin
                match i with
                | 0 -> gamma
                | 1 -> (FunContext.add x (List.at rs 0) gamma)
                | _ -> failwith "Wrong index for Tr in Let!"
            end
        | LetRec({name=(x, t); args=yts; body=e1}, e2, _) ->
            let gamma' = (FunContext.add x (TFun(List.map snd yts, t)) gamma) in
            begin
                match i with
                | 0 -> FunContext.add_list yts gamma'
                | 1 -> gamma'
                | _ -> failwith "Wrong index for Tr in LetRec!"
            end
        | App(e, es, _) -> gamma
        | Tuple(es, _) -> gamma
        | LetTuple(xs, e1, e2, _) ->
            begin
                match i with
                | 0 -> gamma
                | 1 -> (match (List.at rs 0) with
                        | TTuple(ts) -> FunContext.add_list2 xs ts gamma
                        | _ -> failwith "Wrong type of e1 in LetTuple.")
                | _ -> failwith "Wrong index for Tr in Let!"
            end
        | Array(_, _, _) -> gamma
        | Get (_, _, _) -> gamma
        | Put (_, _, _, _) -> gamma


    let checkjoin (t : (int * VarSet.t) term) (gamma : context) (rs : res list) : res option =
        let rec check t1 t2 =
            match (t1, t2) with
                | (TUnit, TUnit) -> Some TUnit
                | (TInt, TInt) -> Some TInt
                | (TFloat, TFloat) -> Some TFloat
                | (TBool, TBool) -> Some TBool
                | (TArray(t1), TArray(t2)) -> (match check t1 t2 with
                    | Some t -> Some (TArray t)
                    | None -> None)
                | (TTuple(ts1), TTuple(ts2)) ->
                    let args = List.map (fun (a, b) -> check a b) (List.combine ts1 ts2) in
                    if List.mem None args then None
                    else Some (TTuple (List.map (fun (Some t) -> t) args))
                | (TFun(ts1, tr1),TFun(ts2, tr2)) ->
                    let args = List.map (fun (a, b) -> check a b) (List.combine ts1 ts2) in
                    let trs = check tr1 tr2 in
                    if List.mem None args then None
                    else
                        (match trs with
                        | None -> None
                        | Some t -> Some (TFun (List.map (fun (Some t) -> t) args, t)))
                | _ -> None in
        match t with
        | Unit(_) -> Some TUnit
        | Bool(_, _) -> Some TBool
        | Int(_, _) -> Some TInt
        | Float(_, _) -> Some TFloat
        | Var(x, _) -> FunContext.find_option gamma x
        | Not(e1, _) -> check (List.at rs 0) TBool
        | Neg(e1, _) -> check (List.at rs 0) TInt
        | FNeg(e1, _) -> check (List.at rs 0) TFloat
        | IBop(_, e1, e2, _) -> (match (check (List.at rs 0) TInt, check (List.at rs 1) TInt) with
            | (Some _, Some _) -> Some TInt
            | _ -> None)
        | FBop(_, e1, e2, _) -> (match (check (List.at rs 0) TFloat, check (List.at rs 1) TFloat) with
            | (Some _, Some _) -> Some TFloat
            | _ -> None)
        | Rel(_, _, _, _) -> (match (check (List.at rs 0) (List.at rs 1)) with
            | Some _ -> Some TBool
            | _ -> None)
        | If(_, _, _, _) ->
            (match (check (List.at rs 0) TBool, check (List.at rs 1) (List.at rs 2)) with
            | (Some _, t) -> t
            | _ -> None)
        | Let(x, e1, e2, _) -> Some (List.at rs 1)
        | LetRec({name=(x, t); args=yts; body=e1}, e2, _) -> Some (List.at rs 1)
        | App(e, es, _) ->
            (
                let ([te], tes) = List.split_at 1 rs in
                let (TFun(ts, tr) as t) = te in
                    match check t (TFun (tes , tr)) with
                    | None -> None
                    | Some _ -> Some tr
            )
        | Tuple(es, _) -> Some (TTuple rs) (* [ (es_0, 0), (es_1, 1), ..., (es_(len-1), len-1)] *)
        | LetTuple(xs, e1, e2, _) -> Some (List.at rs 1)
        | Array(_, _, _) -> (match check (List.at rs 0) TInt with
            | None -> None
            | Some t -> Some (TArray (List.at rs 1)))
        | Get (_, _, _) -> (match (List.at rs 0, check TInt (List.at rs 1)) with
            | (TArray te1, Some te2) -> Some te1
            | _ -> None)
        | Put (_, _, _, _) -> (match (check TInt (List.at rs 1), check (TArray(List.at rs 2)) (List.at rs 0)) with
            | (Some te2, Some te3) -> Some TUnit
            | _ -> None)
end
