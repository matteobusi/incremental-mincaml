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

(* Ugly: use ppf instead!
let rec string_of_annotast (string_of_payload : 'a -> string) (expr : 'a t) : string =
  let string_of_tree = string_of_annotast string_of_payload in
  match expr with
  | Unit(annot)            	-> Printf.sprintf "Unit{%s}" (string_of_payload annot)
  | Bool(b, annot)         	-> Printf.sprintf "%b{%s}" b (string_of_payload annot)
  | Int(n, annot)          	-> Printf.sprintf "%d{%s}" n (string_of_payload annot)
  | Float(f, annot)        	-> Printf.sprintf "%f{%s}" f (string_of_payload annot)
  | Not(e, annot)          	-> Printf.sprintf "Not(%s){%s}" (string_of_tree e) (string_of_payload annot)
  | Neg(e ,annot)          	-> Printf.sprintf "-(%s){%s}" (string_of_tree e) (string_of_payload annot)
  | IBop(op, e1, e2, annot)    	-> Printf.sprintf "%s(%s, %s){%s}" op (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | FNeg(e, annot)         	-> Printf.sprintf "-.(%s){%s}" (string_of_tree e) (string_of_payload annot)
  | FBop(op, e1, e2, annot)    	-> Printf.sprintf "%s(%s, %s){%s}" op (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | Rel(op, e1, e2, annot)     	-> Printf.sprintf "%s(%s,%s){%s}" op (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | If(e1, e2,e3, annot)   	-> Printf.sprintf "If(%s,%s,%s){%s}" (string_of_tree e1) (string_of_tree e2) (string_of_tree e3) (string_of_payload annot)
  | Let(id, e1, e2, annot)      -> Printf.sprintf "Let(%s,%s,%s){%s}" id (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | Var(id, annot)        	-> Printf.sprintf "Var(%s){%s}" id  (string_of_payload annot)
  | LetRec(f, e, annot)   	-> Printf.sprintf "Let(%s,%s){%s}" (string_of_fundef string_of_payload f) (string_of_tree e) (string_of_payload annot)
  | App(e, es, annot)     	-> let args = String.concat " " (List.map string_of_tree es) in
                          	   Printf.sprintf "App(%s, %s){%s}" (string_of_tree e) args (string_of_payload annot)
  | Tuple(es, annot)      	-> let args = String.concat " " (List.map string_of_tree es) in
                                   Printf.sprintf "Tuple(%s){%s}" args (string_of_payload annot)
  | LetTuple(bs, e1, e2, annot) -> Printf.sprintf "LetTuple(%s,%s,%s){%s}" (String.concat " " bs) (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | Array(e1, e2, annot)    	-> Printf.sprintf "Array(%s,%s){%s}" (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | Get(e1,e2, annot)       	-> Printf.sprintf "Get(%s,%s){%s}" (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | Put(e1, e2, e3, annot)  	-> Printf.sprintf "Put(%s,%s,%s){%s}" (string_of_tree e1) (string_of_tree e2) (string_of_tree e3) (string_of_payload annot)
and string_of_fundef (string_of_payload : 'a -> string) ({name = (id,t); args=formals; body=e}) =
  let bindings = List.map (fun (id,t) -> Printf.sprintf "%s : %s" id (Type.string_of_type t)) formals in
  Printf.sprintf "FunVal((%s : %s),(%s),%s)" id (Type.string_of_type t) (String.concat " " bindings) (string_of_annotast string_of_payload e)
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
  | Put(_, _,_, annot)      -> annot


let rec ppf_annotast  ppf_payload ppf (e : 'a t) =
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
