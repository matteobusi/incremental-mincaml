(* Annotated Abstract Syntax Tree *)


type 'a t = (* MinCaml *)
  | Unit of 'a
  | Bool of bool * 'a
  | Int of int * 'a
  | Float of float * 'a
  | Not of 'a t * 'a
  | Neg of 'a t * 'a
  | Add of 'a t * 'a t * 'a
  | Sub of 'a t * 'a t * 'a
  | FNeg of 'a t * 'a
  | FAdd of 'a t * 'a t * 'a
  | FSub of 'a t * 'a t * 'a
  | FMul of 'a t * 'a t * 'a
  | FDiv of 'a t * 'a t * 'a
  | Eq of 'a t * 'a t * 'a
  | LE of 'a t * 'a t * 'a
  | If of 'a t * 'a t * 'a t * 'a
  | Let of (Id.t * Type.t) * 'a t * 'a t *'a
  | Var of Id.t * 'a
  | LetRec of 'a fundef * 'a t * 'a
  | App of 'a t * 'a t list * 'a
  | Tuple of 'a t list * 'a
  | LetTuple of (Id.t * Type.t) list * 'a t * 'a t * 'a
  | Array of 'a t * 'a t * 'a
  | Get of 'a t * 'a t * 'a
  | Put of 'a t * 'a t * 'a t * 'a
and 'a fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : 'a t }

(* Ugly: use ppf instead! *)
let rec string_of_annotast (string_of_payload : 'a -> string) (expr : 'a t) : string =
  let string_of_tree = string_of_annotast string_of_payload in
  match expr with
  | Unit(annot)            	-> Printf.sprintf "Unit{%s}" (string_of_payload annot)
  | Bool(b, annot)         	-> Printf.sprintf "Bool(%b){%s}" b (string_of_payload annot)
  | Int(n, annot)          	-> Printf.sprintf "Int(%d){%s}" n (string_of_payload annot)
  | Float(f, annot)        	-> Printf.sprintf "Float(%f){%s}" f (string_of_payload annot)
  | Not(e, annot)          	-> Printf.sprintf "Not(%s){%s}" (string_of_tree e) (string_of_payload annot)
  | Neg(e ,annot)          	-> Printf.sprintf "Neg(%s){%s}" (string_of_tree e) (string_of_payload annot)
  | Add(e1, e2, annot)     	-> Printf.sprintf "Add(%s, %s){%s}" (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | Sub(e1, e2, annot)     	-> Printf.sprintf "Sub(%s, %s){%s}" (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | FNeg(e, annot)         	-> Printf.sprintf "FNeg(%s){%s}" (string_of_tree e) (string_of_payload annot)
  | FAdd(e1, e2, annot)    	-> Printf.sprintf "FAdd(%s, %s){%s}" (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | FSub(e1, e2, annot)    	-> Printf.sprintf "FSub(%s, %s){%s}" (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | FMul(e1, e2, annot)    	-> Printf.sprintf "FMul(%s, %s){%s}" (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | FDiv(e1, e2, annot)    	-> Printf.sprintf "FDiv(%s, %s){%s}" (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | Eq(e1, e2, annot)      	-> Printf.sprintf "Eq(%s,%s){%s}" (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | LE(e1, e2, annot)      	-> Printf.sprintf "LE(%s,%s){%s}" (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | If(e1, e2,e3, annot)   	-> Printf.sprintf "If(%s,%s,%s){%s}" (string_of_tree e1) (string_of_tree e2) (string_of_tree e3) (string_of_payload annot)
  | Let((id, t), e1, e2, annot) -> Printf.sprintf "Let(%s : %s,%s,%s){%s}" id (Type.string_of_type t) (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | Var(id, annot)        	-> Printf.sprintf "Var(%s){%s}" id  (string_of_payload annot)
  | LetRec(f, e, annot)   	-> Printf.sprintf "Let(%s,%s){%s}" (string_of_fundef string_of_payload f) (string_of_tree e) (string_of_payload annot)
  | App(e, es, annot)     	-> let args = String.concat " " (List.map string_of_tree es) in
                          	   Printf.sprintf "App(%s, %s){%s}" (string_of_tree e) args (string_of_payload annot)
  | Tuple(es, annot)      	-> let args = String.concat " " (List.map string_of_tree es) in
                                   Printf.sprintf "Tuple(%s){%s}" args (string_of_payload annot)
  | LetTuple(bs, e1, e2, annot) -> let bindings = List.map (fun (id,t) -> Printf.sprintf "%s : %s" id (Type.string_of_type t)) bs in
                                   Printf.sprintf "LetTuple(%s,%s,%s){%s}" (String.concat " " bindings) (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | Array(e1, e2, annot)    	-> Printf.sprintf "Array(%s,%s){%s}" (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | Get(e1,e2, annot)       	-> Printf.sprintf "Get(%s,%s){%s}" (string_of_tree e1) (string_of_tree e2) (string_of_payload annot)
  | Put(e1, e2, e3, annot)  	-> Printf.sprintf "Put(%s,%s,%s){%s}" (string_of_tree e1) (string_of_tree e2) (string_of_tree e3) (string_of_payload annot)
and string_of_fundef (string_of_payload : 'a -> string) ({name = (id,t); args=formals; body=e}) =
  let bindings = List.map (fun (id,t) -> Printf.sprintf "%s : %s" id (Type.string_of_type t)) formals in
  Printf.sprintf "FunVal((%s : %s),(%s),%s)" id (Type.string_of_type t) (String.concat " " bindings) (string_of_annotast string_of_payload e)
