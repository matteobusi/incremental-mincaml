%{
(* parser *)
(* From https://github.com/esumii/min-caml *)
(* Adapted to slightly different language @ UniPI
Improvements/fixes:
    - English comments
    - Better errors
*)
open Batteries
open FunSpecification.FunSpecification
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
%token <string> IDENT
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
%type <unit FunSpecification.FunSpecification.term> exp
%start exp

%%

simple_exp: /* (* simple expressions *) */
| LPAREN exp RPAREN
    { $2 }
| LPAREN RPAREN
    { Unit () }
| BOOL
    { Bool($1, ()) }
| INT
    { Int($1, ()) }
| FLOAT
    { Float($1, ()) }
| IDENT
    { Var($1, ()) }
| simple_exp DOT LPAREN exp RPAREN
    {
        Get($1, $4, ())
    }

exp: /* (* Expressions *) */
| simple_exp
    { $1 }
| NOT exp
    %prec prec_app
    { Not($2, () ) }
| MINUS exp
    %prec prec_unary_minus
    { match $2 with
    | Float(f, _) -> Float(-.f, ()) (* e.g. -1.23 *)
    | e -> Neg(e, ()) }
| exp PLUS exp /* (* Arithmetic operations on integers *) */
    {
      IBop("+", $1, $3, ())
    }
| exp MINUS exp
    {
      IBop("-", $1, $3, ())
    }
| exp AST exp
    {
      IBop("*", $1, $3, ())
    }
| exp EQUAL exp     /* (* Relational operators *) */
    {
     Rel("=",$1, $3, ())
    }
| exp LESS_GREATER exp
    {
     Not(Rel("=", $1, $3, ()), ())
     }
| exp LESS exp
    {
     Rel("<", $3, $1, ())
    }
| exp GREATER exp
    {
      Rel(">", $1, $3, ())
    }
| exp LESS_EQUAL exp
    {
    Rel("<=", $1, $3, ())
    }
| exp GREATER_EQUAL exp
    {
    Rel(">=", $3, $1, ())
    }
| IF exp THEN exp ELSE exp
    %prec prec_if
    {
    If($2, $4, $6, ())
    }
| MINUS_DOT exp
    %prec prec_unary_minus
    { FNeg($2, ()) }
| exp PLUS_DOT exp
    {
    FBop("+.",$1, $3, ())
    }
| exp MINUS_DOT exp
    {
    FBop("-.",$1, $3, ())
    }
| exp AST_DOT exp
    {
    FBop("*.",$1, $3, ())
    }
| exp SLASH_DOT exp
    {
    FBop("/.",$1, $3, ())
    }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { Let($2, $4, $6, ()) }
| LET REC fundef IN exp
    %prec prec_let
    { LetRec($3, $5, ()) }
| simple_exp actual_args
    %prec prec_app
    {
      App($1, $2, ())
    }
| elems
    %prec prec_tuple
    {
        Tuple($1, ())
    }
| LET LPAREN pat RPAREN EQUAL exp IN exp
    {
      LetTuple($3, $6, $8, ())
    }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp
    { Put($1, $4, $7, ()) }
| exp SEMICOLON exp
    {
        let nid = Id.gentmp TUnit in
        Let(nid, $1, $3, ())
    }
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { Array($2, $3, ()) }
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
    { TInt }
 | TYPE_BOOL
    { TBool }
 | TYPE_UNIT
    { TUnit }
 | TYPE_FLOAT
    { TFloat }
 | LBRACKET types RBRACKET
    { TArray($2) }

 | LPAREN ltypes RPAREN
    {
      (* Consider in a special way list of length 1 *)
      if List.length $2 > 1 then
      	 TTuple($2)
      else
         List.hd $2
    }
 | ltypes ARROW types
    {
        TFun($1, $3)
    }

ltypes:
 | types AST ltypes
   { $1 :: $3 }
 | types
   { [$1] }
