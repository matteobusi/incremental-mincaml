%{
(* parser *)
(* From https://github.com/esumii/min-caml *)
(* Adapted to slightly different language @ UniPI 
Improvements/fixes:
    - English comments
    - Better errors
*)
open Hashing
open Annotast
open Varset

let get_hash (e : (int * VarSet.t) Annotast.t) = fst (Annotast.get_annot e)
let get_fv (e : (int * VarSet.t) Annotast.t) = snd (Annotast.get_annot e)

let hash_of_fundef ({name=(id,rt); args=xs; body=e}) =
    let hash_id = compute_hash id in
    let hash_tr = compute_hash rt in
    let hash_args = combine_hashes (List.map (fun (id, t) ->
    		    		    combine_hashes [compute_hash id; compute_hash t]) xs) in
    combine_hashes [hash_id ; hash_tr; hash_args; get_hash e]

let invRemove e s = VarSet.remove s e

let fv_of_fundef ({name=(id,rt); args=xs; body=e1}) e2 =
    let fv_e1 = get_fv e1 in
    let fv_e2 = get_fv e2 in
    List.fold_left invRemove (VarSet.union fv_e1 fv_e2) (id::(List.map fst xs))

%}

/* (* Token definition *) */
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token NOT
%token MINUS
%token PLUS
%token MINUS_DOT
%token PLUS_DOT
%token AST_DOT
%token SLASH_DOT
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <Id.t> IDENT
%token LET
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token DOT
/* (* New tokes: operators *) */
%token COLON
%token ARROW
%token AST
%token LBRACKET
%token RBRACKET
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
/* (* New tokens: types *) */
%token TYPE_INT
%token TYPE_BOOL
%token TYPE_FLOAT
%token TYPE_UNIT
%token EOF

/* (* Operators priority and associativity *) */
%nonassoc IN
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%nonassoc prec_tuple
%right ARROW
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST_DOT SLASH_DOT AST
%right prec_unary_minus
%left prec_app
%left DOT


/* (* Start symbol *) */
%type <(int * Varset.VarSet.t) Annotast.t> exp
%start exp

%%

simple_exp: /* (* simple expressions *) */
| LPAREN exp RPAREN
    { $2 }
| LPAREN RPAREN
    { Unit ((compute_hash "unit", VarSet.empty)) }
| BOOL
    { Bool($1, (compute_hash $1, VarSet.empty)) }
| INT
    { Int($1, (compute_hash $1, VarSet.empty)) }
| FLOAT
    { Float($1, (compute_hash $1, VarSet.empty)) }
| IDENT
    { Var($1, (compute_hash $1, VarSet.singleton $1)) }
| simple_exp DOT LPAREN exp RPAREN
    {
        let hs = [compute_hash "get"; get_hash $1; get_hash $4] in
        let fv = VarSet.union (get_fv $1) (get_fv $4) in
        Get($1, $4, (combine_hashes hs, fv))
    }

exp: /* (* Expressions *) */
| simple_exp
    { $1 }
| NOT exp
    %prec prec_app
    { Not($2, (combine_hashes [compute_hash "not"; get_hash $2], get_fv $2) ) }
| MINUS exp
    %prec prec_unary_minus
    { match $2 with
    | Float(f, _) -> Float(-.f, (compute_hash (-.f), get_fv $2)) (* e.g. -1.23 *)
    | e -> Neg(e, (combine_hashes [compute_hash "neg"; get_hash e], get_fv $2)) }
| exp PLUS exp /* (* Arithmetic operations on integers *) */
    {
      let hs = [compute_hash "+"; get_hash $1; get_hash $3] in
      let fv = VarSet.union (get_fv $1) (get_fv $3) in
      IBop("+", $1, $3, (combine_hashes hs, fv))
    }
| exp MINUS exp
    {
      let hs = [compute_hash "-"; get_hash $1; get_hash $3 ] in
      let fv = VarSet.union (get_fv $1) (get_fv $3) in
      IBop("-", $1, $3, (combine_hashes hs, fv))
    }
| exp AST exp
    {
      let hs = [compute_hash "*"; get_hash $1; get_hash $3 ] in
      let fv = VarSet.union (get_fv $1) (get_fv $3) in
      IBop("*", $1, $3, (combine_hashes hs, fv))
    }
| exp EQUAL exp     /* (* Relational operators *) */
    {
     let hs = [compute_hash "="; get_hash $1; get_hash $3] in
     let fv = VarSet.union (get_fv $1) (get_fv $3) in
     Rel("=",$1, $3, (combine_hashes hs, fv))
    }
| exp LESS_GREATER exp
    {
     let hs = combine_hashes [compute_hash "="; get_hash $1; get_hash $3 ] in
     let fv = VarSet.union (get_fv $1) (get_fv $3) in
     Not(Rel("=", $1, $3, (hs, fv)), (combine_hashes [compute_hash "not"; hs], fv))
     }
| exp LESS exp
    {
     let hs = [compute_hash "<"; get_hash $1 ; get_hash $3 ] in
     let fv = VarSet.union (get_fv $1) (get_fv $3) in
     Rel("<", $3, $1, (combine_hashes hs, fv))
    }
| exp GREATER exp
    {
      let hs = [compute_hash ">"; get_hash $1 ; get_hash $3 ] in
      let fv = VarSet.union (get_fv $1) (get_fv $3) in
      Rel(">", $1, $3, (combine_hashes hs, fv))
    }
| exp LESS_EQUAL exp
    {
    let hs = [compute_hash "<="; get_hash $1 ; get_hash $3 ] in
    let fv = VarSet.union (get_fv $1) (get_fv $3) in
    Rel("<=", $1, $3, (combine_hashes hs, fv))
    }
| exp GREATER_EQUAL exp
    {
    let hs = [compute_hash ">="; get_hash $1 ; get_hash $3 ] in
    let fv = VarSet.union (get_fv $1) (get_fv $3) in
    Rel(">=", $3, $1, (combine_hashes hs, fv))
    }
| IF exp THEN exp ELSE exp
    %prec prec_if
    {
    let hs = [compute_hash "ifthen"; get_hash $2 ; get_hash $4 ; get_hash $6] in
    let fv = VarSet.union (get_fv $2) (VarSet.union (get_fv $4) (get_fv $6)) in
    If($2, $4, $6, (combine_hashes hs, fv))
    }
| MINUS_DOT exp
    %prec prec_unary_minus
    { FNeg($2, (combine_hashes [compute_hash "fneg"; get_hash $2], get_fv $2)) }
| exp PLUS_DOT exp
    {
    let hs = [compute_hash "+."; get_hash $1 ; get_hash $3 ] in
    let fv = VarSet.union (get_fv $1) (get_fv $3) in
    FBop("+.",$1, $3, (combine_hashes hs, fv))
    }
| exp MINUS_DOT exp
    {
    let hs = [compute_hash "-."; get_hash $1 ; get_hash $3 ] in
    let fv = VarSet.union (get_fv $1) (get_fv $3) in
    FBop("-.",$1, $3, (combine_hashes hs, fv))
    }
| exp AST_DOT exp
    {
    let hs = [compute_hash "*."; get_hash $1 ; get_hash $3 ] in
    let fv = VarSet.union (get_fv $1) (get_fv $3) in
    FBop("*.",$1, $3, (combine_hashes hs, fv))
    }
| exp SLASH_DOT exp
    {
    let hs = [compute_hash "/."; get_hash $1 ; get_hash $3 ] in
    let fv = VarSet.union (get_fv $1) (get_fv $3) in
    FBop("/.",$1, $3, (combine_hashes hs, fv))
    }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { Let($2, $4, $6, (combine_hashes [compute_hash "let"; compute_hash $2; get_hash $4; get_hash $6], VarSet.remove $2 (VarSet.union (get_fv $4) (get_fv $6)))) }
| LET REC fundef IN exp
    %prec prec_let
    { LetRec($3, $5, (combine_hashes [compute_hash "lrec"; hash_of_fundef $3; get_hash $5], fv_of_fundef $3 $5)) }
| simple_exp actual_args
    %prec prec_app
    {
      let hargs = List.map get_hash $2 in
      let fv_args = List.fold_left VarSet.union VarSet.empty (List.map get_fv $2) in 
      App($1, $2, (combine_hashes ([compute_hash "app" ; get_hash $1] @ hargs), VarSet.union fv_args (get_fv $1)))
    }
| elems
    %prec prec_tuple
    { 
        let fv_tuple = List.fold_left VarSet.union VarSet.empty (List.map (fun e -> get_fv e) $1) in
        Tuple($1, (combine_hashes ((compute_hash "tuple")::(List.map get_hash $1)), fv_tuple)) 
    }
| LET LPAREN pat RPAREN EQUAL exp IN exp
    {
      let identHash = List.map compute_hash $3 in
      let hs = (compute_hash "ltup") :: (get_hash $6) :: (get_hash $8) :: identHash in
      let fv = List.fold_left invRemove (VarSet.union (get_fv $6) (get_fv $8)) $3 in
      LetTuple($3, $6, $8, (combine_hashes hs, fv))
    }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp
    { Put($1, $4, $7, (combine_hashes [compute_hash "put"; get_hash $1; get_hash $4; get_hash $7], VarSet.union (get_fv $1) (VarSet.union (get_fv $4) (get_fv $7)))) }
| exp SEMICOLON exp
    {
        let nid = Id.gentmp Type.Unit in
        let hs = [compute_hash "semicol";
	            compute_hash nid;
                get_hash $1;
		        get_hash $3 ] in
        Let(nid, $1, $3, (combine_hashes hs, VarSet.union (get_fv $1) (get_fv $3)))
    }
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { Array($2, $3, (combine_hashes [compute_hash "mkar"; get_hash $3; get_hash $3 ], VarSet.union (get_fv $2) (get_fv $3))) }
| error
    { failwith
        (Printf.sprintf "Parse error at line %d."
           (Parsing.symbol_start_pos ()).pos_lnum) }

fundef: /* (* e.g. f (y_1 : \tau_1, ..., y_n : \tau_n) : \tau_f = ...*) */
| IDENT formal_args COLON types EQUAL exp
    { { name = ($1,$4); args = $2; body = $6 } }

formal_args: /* (* e.g. (y_1 : \tau_1, ..., y_n : \tau_n) *) */
| LPAREN IDENT COLON types RPAREN formal_args
    { ($2,$4) :: $6 }
| LPAREN IDENT COLON types RPAREN
    { [($2, $4)] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }

elems:
| elems COMMA exp
    { $1 @ [$3] }
| exp COMMA exp
    { [$1; $3] }

pat:
| pat COMMA IDENT
    { $1 @ [ $3] }
| IDENT COMMA IDENT
    { [ $1;  $3] }

types:
 | TYPE_INT
    { Type.Int }
 | TYPE_BOOL
    { Type.Bool }
 | TYPE_UNIT
    { Type.Unit }
 | TYPE_FLOAT
    { Type.Float }
 | LBRACKET types RBRACKET
    { Type.Array($2) }

 | LPAREN ltypes RPAREN
    {
      (* Consider in a special way list of length 1 *)
      if List.length $2 > 1 then
      	 Type.Tuple($2)
      else
         List.hd $2
    }
 | ltypes ARROW types
    {
        Type.Fun($1, $3)
    }

ltypes:
 | types AST ltypes
   { $1 :: $3 }
 | types
   { [$1] }
