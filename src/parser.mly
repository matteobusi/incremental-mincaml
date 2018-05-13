%{
(* parser *)
(* From https://github.com/esumii/min-caml *)
open Annotast
open Hashing

let get_hash (e : int Annotast.t) =
  Annotast.get_annot e

let hash_of_fundef ({name=(id,rt); args=xs; body=e}) =
    let hash_id = compute_hash id in
    let hash_tr = compute_hash rt in
    let hash_args = combine_hashes (List.map (fun (id, t) ->
    		    		    combine_hashes [compute_hash id; compute_hash t]) xs) in
    combine_hashes [hash_id ; hash_tr; hash_args; get_hash e]
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
/* (* NUOVI TOKEN: OPERATORI *) */
%token COLON
%token ARROW
%token AST
%token LBRACKET
%token RBRACKET
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
/* (* NUOVI TOKEN: tipi *) */
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
%type <int Annotast.t> exp
%start exp

%%

simple_exp: /* (* simple expressions *) */
| LPAREN exp RPAREN
    { $2 }
| LPAREN RPAREN
    { Unit (compute_hash "unit") }
| BOOL
    { Bool($1, compute_hash $1) }
| INT
    { Int($1, compute_hash $1) }
| FLOAT
    { Float($1, compute_hash $1) }
| IDENT
    { Var($1, compute_hash $1) }
| simple_exp DOT LPAREN exp RPAREN
    {
     let hs =  [compute_hash "get";
                get_hash $1 ;
		get_hash $4 ] in
       Get($1, $4, (combine_hashes hs))
     }

exp: /* (* Expressions *) */
| simple_exp
    { $1 }
| NOT exp
    %prec prec_app
    { Not($2, combine_hashes [compute_hash "not"; get_hash $2] ) }
| MINUS exp
    %prec prec_unary_minus
    { match $2 with
    | Float(f, _) -> Float(-.f, compute_hash (-.f)) (* -1.23 *)
    | e -> Neg(e, combine_hashes [compute_hash "neg"; get_hash e]) }
| exp PLUS exp /* (* Arithmetic operations on integers *) */
    {
      let hs = [compute_hash "+";
                get_hash $1 ;
		get_hash $3 ] in
        IBop("+", $1, $3, combine_hashes hs)
    }
| exp MINUS exp
    {
      let hs = [compute_hash "-";
                get_hash $1 ;
		get_hash $3 ] in
        IBop("-", $1, $3, combine_hashes hs)
    }
| exp AST exp
    {
      let hs = [compute_hash "*";
                get_hash $1 ;
		get_hash $3 ] in
        IBop("*", $1, $3, combine_hashes hs)
    }
| exp EQUAL exp     /* (* Relational operators *) */
    {
     let hs = [compute_hash "=";
                get_hash $1 ;
		get_hash $3 ] in
       Rel("=",$1, $3, combine_hashes hs)
    }
| exp LESS_GREATER exp
    {
     let hs = combine_hashes [compute_hash "=";
                               get_hash $1 ;
		               get_hash $3 ] in
       Not(Rel("=", $1, $3, hs), combine_hashes [compute_hash "="; hs])
     }
| exp LESS exp
    {
     let hs = [compute_hash "<";
                get_hash $1 ;
		get_hash $3 ] in
       Rel("<", $3, $1, combine_hashes hs)
    }
| exp GREATER exp
    {
      let hs = [compute_hash ">";
                get_hash $1 ;
		get_hash $3 ] in
      Rel(">", $1, $3, combine_hashes hs)
    }
| exp LESS_EQUAL exp
    {
     let hs = [compute_hash "<=";
                get_hash $1 ;
		get_hash $3 ] in
       Rel("<=", $1, $3, combine_hashes hs)
    }
| exp GREATER_EQUAL exp
    {
     let hs = [compute_hash ">=";
                get_hash $1 ;
		get_hash $3 ] in
       Rel(">=", $3, $1, combine_hashes hs)
    }
| IF exp THEN exp ELSE exp
    %prec prec_if
    {
     let hs = [compute_hash "ifthen";
                get_hash $2 ;
		get_hash $4 ;
		get_hash $6] in
       If($2, $4, $6, combine_hashes hs)
    }
| MINUS_DOT exp
    %prec prec_unary_minus
    { FNeg($2, combine_hashes [compute_hash "fneg"; get_hash $2]) }
| exp PLUS_DOT exp
    {
     let hs = [compute_hash "+.";
                get_hash $1 ;
		get_hash $3 ] in
      FBop("+.",$1, $3, combine_hashes hs)
    }
| exp MINUS_DOT exp
    {
      let hs = [compute_hash "-.";
                get_hash $1 ;
		get_hash $3 ] in
      FBop("-.",$1, $3, combine_hashes hs)
    }
| exp AST_DOT exp
    {
      let hs = [compute_hash "*.";
                get_hash $1 ;
		get_hash $3 ] in
      FBop("*.",$1, $3, combine_hashes hs)
    }
| exp SLASH_DOT exp
    {
     let hs = [compute_hash "/.";
                get_hash $1 ;
		get_hash $3 ] in
     FBop("/.",$1, $3, combine_hashes hs)
    }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { Let($2, $4, $6, combine_hashes [compute_hash "let"; compute_hash $2; get_hash $4; get_hash $6]) }
| LET REC fundef IN exp
    %prec prec_let
    { LetRec($3, $5, combine_hashes [compute_hash "lrec"; hash_of_fundef $3; get_hash $5]) }
| simple_exp actual_args
    %prec prec_app
    {
      let hargs = List.map get_hash $2 in
      App($1, $2, combine_hashes ([compute_hash "app" ; get_hash $1] @ hargs))
    }
| elems
    %prec prec_tuple
    { Tuple($1, combine_hashes ((compute_hash "tuple")::(List.map get_hash $1))) }
| LET LPAREN pat RPAREN EQUAL exp IN exp
    {
      let identHash = List.map compute_hash $3 in
      let hs = (compute_hash "ltup") :: (get_hash $6) :: (get_hash $8) :: identHash in
      LetTuple($3, $6, $8, combine_hashes hs)
    }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp
    { Put($1, $4, $7, combine_hashes [compute_hash "put"; get_hash $1; get_hash $4; get_hash $7]) }
| exp SEMICOLON exp
    {
        let nid =  Id.gentmp Type.Unit in
        let hs = [compute_hash "let;";
	          compute_hash nid;
                  get_hash $1 ;
		  get_hash $3 ] in
        Let(nid, $1, $3, combine_hashes hs)
    }
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { Array($2, $3, combine_hashes [compute_hash "mkar"; get_hash $3; get_hash $3 ]) }
| error
    { failwith
        (Printf.sprintf "parse error near characters %d-%d"
           (Parsing.symbol_start ())
           (Parsing.symbol_end ())) }

fundef:
| IDENT formal_args COLON types EQUAL exp
    { { name = ($1,$4); args = $2; body = $6 } }

formal_args:
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
