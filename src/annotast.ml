(* 
  MinCaml Annotated Abstract Syntax Tree (aAST)
*)
open Batteries
open Varset

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
  | Let of Id.t * 'a t * 'a t *'a
  | Var of Id.t * 'a
  | LetRec of 'a fundef * 'a t * 'a
  | App of 'a t * 'a t list * 'a
  | Tuple of 'a t list * 'a
  | LetTuple of Id.t list * 'a t * 'a t * 'a
  | Array of 'a t * 'a t * 'a
  | Get of 'a t * 'a t * 'a
  | Put of 'a t * 'a t * 'a t * 'a
and 'a fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : 'a t }

(* 
  Extracts the annotation in the aAST
*)
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

(*
  Pretty prints the given aAST, given the pretty printer function for the annotation
*)
let string_of_annotast ppf_annot (e : 'a t) : string = 
  let rec ppf_annotast ppf_annot ppf (e : 'a t) =
    let ppf_tree = ppf_annotast ppf_annot in
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
    | LetRec(f, e, annot)   	    -> Format.fprintf ppf "@[<2>Let(%a,@,%a){%a}@]" (fundef_ppf ppf_tree) f ppf_tree e ppf_annot annot
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
    Format.fprintf ppf "@[<2>FunVal((%s : %a),(" id Type.type_ppf t;
    list_binding_ppf ppf formals;
    Format.fprintf ppf "),@,%a)@]"  syntax_ppf e

  and list_binding_ppf ppf bs = if List.length bs > 0 then
      let (id, t) = List.hd bs in
      Format.fprintf ppf "%s : %a" id Type.type_ppf t;
      List.iter (fun (id, t) -> Format.fprintf ppf ", %s : %a" id Type.type_ppf t) (List.tl bs);
    else
      ()
  in
  ppf_annotast ppf_annot Format.str_formatter e;
  Format.flush_str_formatter ()

(*
  Given an aAST, return a new aAST decorated with the set of free variables of its nodes
*)
let free_variables e =
  let invRemove e s = VarSet.remove s e in
  let rec free_variables_cps (e : 'a t) k : (('a * VarSet.t) t) = match e with
    | Unit(a) -> k (Unit((a, VarSet.empty)))
    | Bool(v, a)-> k (Bool(v, (a, VarSet.empty)))
    | Int(v, a)-> k (Int(v, (a, VarSet.empty)))
    | Float(v, a) -> k (Float(v, (a, VarSet.empty)))
    | Var(x, a) -> k (Var(x, (a, VarSet.singleton x)))
    | Not(e1, a) -> free_variables_cps e1 (fun r1 -> k (Not(r1, (a,  snd (get_annot r1)))))
    | Neg(e1, a) -> free_variables_cps e1 (fun r1 -> k (Neg(r1, (a,  snd (get_annot r1)))))
    | FNeg(e1, a) -> free_variables_cps e1 (fun r1 -> k (FNeg(r1, (a,  snd (get_annot r1)))))
    | IBop(op, e1, e2, a) -> free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> k (IBop(op, r1, r2, (a, VarSet.union ( snd (get_annot r1)) ( snd (get_annot r2)))))))
    | FBop(op, e1, e2, a) ->  free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> k (FBop(op, r1, r2, (a, VarSet.union ( snd (get_annot r1)) ( snd (get_annot r2)))))))
    | Rel(op, e1, e2, a) -> free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> k (Rel(op, r1, r2, (a, VarSet.union ( snd (get_annot r1)) ( snd (get_annot r2)))))))
    | If(e1, e2, e3, a) -> free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> free_variables_cps e3 (fun r3 -> k (If(r1, r2, r3, (a, VarSet.union ( snd (get_annot r1)) (VarSet.union ( snd (get_annot r2)) ( snd (get_annot r3)))))))))
    | Let(x, e1, e2, a) -> free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> k (Let(x, r1, r2, (a, VarSet.remove x (VarSet.union ( snd (get_annot r1)) ( snd (get_annot r2))))))))
    | LetRec ({ name = (x, t); args = yts; body = e1 }, e2, a) -> 
      free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> k (LetRec({ name = (x, t); args = yts; body = r1 }, r2, (a, List.fold_left invRemove (VarSet.union ( snd (get_annot r1)) ( snd (get_annot r2))) (x::(List.map fst yts)))))))
    | App (e1, es, a) -> free_variables_cps e1 (fun r1 -> k (let es_fv = List.map (fun e -> free_variables_cps e k) es in App(r1, es_fv, (a,(VarSet.union ( snd (get_annot r1)) (List.fold_left VarSet.union (VarSet.empty) (List.map (fun e -> snd (get_annot e)) es_fv)))))))
    | Tuple(es, a) -> k (let es_fv = List.map (fun e -> free_variables_cps e k) es in Tuple(es_fv, (a, List.fold_left VarSet.union (VarSet.empty) (List.map (fun e -> snd (get_annot e)) es_fv))))
    | LetTuple(xs, e1, e2, a) -> free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> k (LetTuple(xs, r1, r2, (a, List.fold_left invRemove (VarSet.union ( snd (get_annot r1)) ( snd (get_annot r2))) xs)))))
    | Array(e1, e2, a) -> free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> k (Array(r1, r2, (a, VarSet.union ( snd (get_annot r1)) ( snd (get_annot r2)))))))
    | Get (e1, e2, a) -> free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> k (Get(r1, r2, (a, VarSet.union ( snd (get_annot r1)) ( snd (get_annot r2)))))))
    | Put (e1, e2, e3, a) -> free_variables_cps e1 (fun r1 -> free_variables_cps e2 (fun r2 -> free_variables_cps e3 (fun r3 -> k (Put(r1, r2, r3,(a, (VarSet.union ( snd (get_annot r1)) (VarSet.union ( snd (get_annot r2)) ( snd (get_annot r3))))))))))
  in free_variables_cps e (fun d -> d)

(*
  Given an aAST compute the number of its nodes
*)
let rec node_count e = match e with 
  | Unit(annot)
  | Bool(_, annot)
  | Int(_, annot)
  | Float(_, annot)
  | Var(_, annot) -> 1
  | Not(e1, annot)
  | Neg(e1, annot)
  | FNeg(e1, annot) -> 1 + node_count e1
  | IBop(_, e1, e2, annot)
  | FBop(_, e1, e2, annot)
  | Rel(_, e1, e2, annot) -> 1 + node_count e1 + node_count e2
  | If(e1, e2, e3, annot) -> 1 + node_count e1 + node_count e2 + node_count e3
  | Let(_, e1, e2, annot) -> 2 + node_count e1 + node_count e2 (* curr node + x *)
  | LetRec ({ name = _; args = yts; body = e1 }, e2, annot) -> 2 + (List.length yts) + node_count e1 + node_count e2
  | App (e1, es, annot) -> 1 + (List.fold_left (+) 0 (List.map node_count (e1::es)))
  | Tuple(es, annot) -> 1 + (List.fold_left (+) 0 (List.map node_count es))
  | LetTuple(xs, e1, e2, annot) -> 1 + List.length xs + node_count e1 + node_count e2
  | Array(e1, e2, annot)
  | Get (e1, e2, annot) -> node_count e1 + node_count e2
  | Put (e1, e2, e3, annot) -> node_count e1 + node_count e2 + node_count e3