(* Annotated Abstract Syntax Tree *)
type 'a t = (* MinCaml *)
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
  | Put(_, _,_, annot)      -> annot


let map_annot f e =
  let rec iter e = match e with
    | Unit(annot)        -> Unit(f annot)
    | Bool(b, annot)     -> Bool(b, f annot)
    | Int(i, annot)      -> Int(i, f annot)
    | Var(id, annot)     -> Var(id, f annot)
    | Float(fv, annot)   -> Float(fv, f annot)
    | Not(e1, annot)     -> Not(iter e1, f annot)
    | Neg(e1, annot)     -> Neg(iter e1, f annot)
    | FNeg(e1, annot)    -> FNeg(iter e1, f annot)
    | IBop(op, e1, e2, annot)       -> IBop(op, iter e1, iter e2, f annot)
    | FBop(op, e1, e2, annot)       -> FBop(op, iter e1, iter e2, f annot)
    | Rel(op, e1, e2, annot)        -> Rel(op, iter e1, iter e2, f annot)
    | Array(e1, e2, annot)          -> Array(iter e1, iter e2, f annot)
    | Get(e1, e2, annot)            -> Get(iter e1, iter e2, f annot)
    | Let(id, e1, e2, annot)        -> Let(id, iter e1, iter e2, f annot)
    | LetTuple(id, e1, e2, annot)   -> LetTuple(id, iter e1, iter e2, f annot)
    | If(e1, e2, e3, annot)         -> If(iter e1, iter e2, iter e3, f annot)
    | Put(e1, e2, e3, annot)        -> Put(iter e1, iter e2, iter e3, f annot)
    | App(e1, es, annot)            -> App(iter e1, List.map iter es, f annot)
    | Tuple(es, annot)              -> Tuple(List.map iter es, f annot)
    | LetRec({name=_ ; args=_ ; body = e1} as fdef, e2, annot) ->
      LetRec({fdef with body = iter e1}, iter e2, f annot)
  in
  iter e


let rec ppf_annotast ppf_payload ppf (e : 'a t) =
  let ppf_tree = ppf_annotast ppf_payload in
  match e with
  | Unit(annot)            	-> Format.fprintf ppf "@[<2>Unit{%a}@]" ppf_payload annot
  | Bool(b, annot)         	-> Format.fprintf ppf "@[<2>%b{%a}@]" b ppf_payload annot
  | Int(n, annot)          	-> Format.fprintf ppf "@[<2>%d{%a}@]" n ppf_payload annot
  | Float(f, annot)        	-> Format.fprintf ppf "@[<2>%f{%a}@]" f ppf_payload annot
  | Not(e, annot)          	-> Format.fprintf ppf "@[<2>Not(%a){%a}@]" ppf_tree e ppf_payload annot
  | Neg(e ,annot)          	-> Format.fprintf ppf "@[<2>-(%a){%a}@]" ppf_tree e ppf_payload annot
  | IBop(op, e1, e2, annot)    	-> Format.fprintf ppf "@[<2>%s(%a, %a){%a}@]" op ppf_tree e1 ppf_tree e2 ppf_payload annot
  | FNeg(e, annot)         	-> Format.fprintf ppf "@[<2>-.(%a){%a}@]" ppf_tree e ppf_payload annot
  | FBop(op, e1, e2, annot)    	-> Format.fprintf ppf "@[<2>%s(%a, %a){%a}@]" op ppf_tree e1 ppf_tree e2 ppf_payload annot
  | Rel(op, e1, e2, annot)     	-> Format.fprintf ppf "@[<2>%s(%a,%a){%a}@]" op ppf_tree e1 ppf_tree e2 ppf_payload annot
  | If(e1, e2,e3, annot)   	-> Format.fprintf ppf "@[<2>If(%a,@,%a,@,%a){%a}@]" ppf_tree e1 ppf_tree e2 ppf_tree e3 ppf_payload annot
  | Let(id, e1, e2, annot)      -> Format.fprintf ppf "@[<2>Let(%s,%a,@,%a){%a}@]" id ppf_tree e1 ppf_tree e2 ppf_payload annot
  | Var(id, annot)        	-> Format.fprintf ppf "@[<2>Var(%s){%a}@]" id  ppf_payload annot
  | Array(e1, e2, annot)    	-> Format.fprintf ppf "@[<2>Array(%a,%a){%a}@]" ppf_tree e1 ppf_tree e2 ppf_payload annot
  | Get(e1,e2, annot)       	-> Format.fprintf ppf "@[<2>Get(%a,%a){%a}@]" ppf_tree e1 ppf_tree e2 ppf_payload annot
  | Put(e1, e2, e3, annot)  	-> Format.fprintf ppf "@[<2>Put(%a,%a,%a){%a}@]" ppf_tree e1 ppf_tree e2 ppf_tree e3 ppf_payload annot

  | LetRec(f, e, annot)   	-> Format.fprintf ppf "@[<2>Let(%a,@,%a){%a}@]" (fundef_ppf ppf_tree) f ppf_tree e ppf_payload annot
  | App(e, es, annot)     	-> Format.fprintf ppf "@[<2>App(%a," ppf_tree e;
                                   list_syntax_ppf ppf_tree ppf es;
                                   Format.fprintf ppf "){%a}@]" ppf_payload annot
  | Tuple(es, annot)      	-> Format.fprintf ppf "@[<2>Tuple(";
                                   list_syntax_ppf ppf_tree ppf es;
                                   Format.fprintf ppf "){%a}@]" ppf_payload annot
  | LetTuple(bs, e1, e2, annot) -> Format.fprintf ppf "@[<2>LetTuple(%s,%a,@,%a){%a}@]" (String.concat " " bs)
                                     ppf_tree e1 ppf_tree e2 ppf_payload annot

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

let string_of_annotast string_of_payload  (expr : 'a t) : string =
  ppf_annotast string_of_payload Format.str_formatter expr;
  Format.flush_str_formatter ()


(*
   Dato che viene usato solo dentro
   free_variables si può spostare
   come funzione locale.
   Si può semplificare l'implementazione usando
   List.filter

let rec list_remove l e = match l with
  | [] -> []
  | x::xs when (String.equal x e) -> list_remove xs e
  | x::xs -> x :: (list_remove xs e)
 *)

let rec free_variables e =
  let list_remove l e = List.filter (fun x -> not (String.equal e x)) l in
  match e with
  | Unit(_)
  | Bool(_)
  | Int(_)
  | Float(_) -> []
  | Var(x, _) -> [x]
  | Not(e1, _)
  | Neg(e1, _)
  | FNeg(e1, _) -> free_variables e1
  | IBop(_, e1, e2, _)
  | FBop(_, e1, e2, _)
  | Rel(_, e1, e2, _) -> List.append (free_variables e1) (free_variables e2)
  | If(e1,e2,e3, _) -> List.append (List.append (free_variables e1) (free_variables e2)) (free_variables e3)
  | Let(x, e1, e2, _) -> list_remove (List.append (free_variables e1) (free_variables e2)) x
  | LetRec ({ name = (x, t); args = yts; body = e1 }, e2, _) -> List.fold_left list_remove (List.append (free_variables e1) (free_variables e2)) (x::(List.map fst yts))
  | App (e1, es, _) -> List.append (free_variables e1) (List.concat (List.map free_variables es))
  | Tuple(es, _) -> List.concat (List.map free_variables es)
  | LetTuple(xs, e1, e2, _) -> List.fold_left list_remove (List.append (free_variables e1) (free_variables e2)) xs
  | Array(e1, e2, _) -> List.append (free_variables e1) (free_variables e2)
  | Get (e1, e2, _) -> List.append (free_variables e1) (free_variables e2)
  | Put (e1, e2, e3, _) ->  List.append (free_variables e1) (List.append (free_variables e2) (free_variables e3))
