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
  Given an aAST, return the list of its free variables
*)
let free_variables e =
  let list_remove l e = List.filter (fun x -> not (String.equal e x)) l in
  let rec free_variables_list e = match e with
  | Unit(_)
  | Bool(_)
  | Int(_)
  | Float(_) -> []
  | Var(x, _) -> [x]
  | Not(e1, _)
  | Neg(e1, _)
  | FNeg(e1, _) -> free_variables_list e1
  | IBop(_, e1, e2, _)
  | FBop(_, e1, e2, _)
  | Rel(_, e1, e2, _) -> List.append (free_variables_list e1) (free_variables_list e2)
  | If(e1,e2,e3, _) -> List.append (List.append (free_variables_list e1) (free_variables_list e2)) (free_variables_list e3)
  | Let(x, e1, e2, _) -> list_remove (List.append (free_variables_list e1) (free_variables_list e2)) x
  | LetRec ({ name = (x, t); args = yts; body = e1 }, e2, _) -> 
    List.fold_left list_remove (List.append (free_variables_list e1) (free_variables_list e2)) (x::(List.map fst yts))
  | App (e1, es, _) -> List.append (free_variables_list e1) (List.concat (List.map free_variables_list es))
  | Tuple(es, _) -> List.concat (List.map free_variables_list es)
  | LetTuple(xs, e1, e2, _) -> List.fold_left list_remove (List.append (free_variables_list e1) (free_variables_list e2)) xs
  | Array(e1, e2, _) -> List.append (free_variables_list e1) (free_variables_list e2)
  | Get (e1, e2, _) -> List.append (free_variables_list e1) (free_variables_list e2)
  | Put (e1, e2, e3, _) ->  List.append (free_variables_list e1) (List.append (free_variables_list e2) (free_variables_list e3))
  in VarSet.of_list(free_variables_list e)

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