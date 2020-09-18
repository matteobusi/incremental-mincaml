%{
(* parser *)
(* From https://github.com/esumii/min-caml *)
(* Adapted to slightly different language @ UniPI
Improvements/fixes:
    - English comments
    - Better errors
*)
open Core
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
%token TYPE_ARRAY
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

exp: iexp { Id.reset (); $1 }

simple_exp: /* (* simple expressions *) */
| LPAREN iexp RPAREN
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
| simple_exp DOT LPAREN iexp RPAREN
    {
        Get($1, $4, ())
    }

iexp: /* (* Expressions *) */
| simple_exp
    { $1 }
| NOT iexp
    %prec prec_app
    { Not($2, () ) }
| MINUS iexp
    %prec prec_unary_minus
    { match $2 with
    | Float(f, _) -> Float(-.f, ()) (* e.g. -1.23 *)
    | e -> Neg(e, ()) }
| iexp PLUS iexp /* (* Arithmetic operations on integers *) */
    {
      IBop("+", $1, $3, ())
    }
| iexp MINUS iexp
    {
      IBop("-", $1, $3, ())
    }
| iexp AST iexp
    {
      IBop("*", $1, $3, ())
    }
| iexp EQUAL iexp     /* (* Relational operators *) */
    {
     Rel("=",$1, $3, ())
    }
| iexp LESS_GREATER iexp
    {
     Not(Rel("=", $1, $3, ()), ())
     }
| iexp LESS iexp
    {
     Rel("<", $3, $1, ())
    }
| iexp GREATER iexp
    {
      Rel(">", $1, $3, ())
    }
| iexp LESS_EQUAL iexp
    {
    Rel("<=", $1, $3, ())
    }
| iexp GREATER_EQUAL iexp
    {
    Rel(">=", $3, $1, ())
    }
| IF iexp THEN iexp ELSE iexp
    %prec prec_if
    {
    If($2, $4, $6, ())
    }
| MINUS_DOT iexp
    %prec prec_unary_minus
    { FNeg($2, ()) }
| iexp PLUS_DOT iexp
    {
    FBop("+.",$1, $3, ())
    }
| iexp MINUS_DOT iexp
    {
    FBop("-.",$1, $3, ())
    }
| iexp AST_DOT iexp
    {
    FBop("*.",$1, $3, ())
    }
| iexp SLASH_DOT iexp
    {
    FBop("/.",$1, $3, ())
    }
| LET IDENT EQUAL iexp IN iexp
    %prec prec_let
    { Let($2, $4, $6, ()) }
| LET REC fundef IN iexp
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
| LET LPAREN pat RPAREN EQUAL iexp IN iexp
    {
      LetTuple($3, $6, $8, ())
    }
| simple_exp DOT LPAREN iexp RPAREN LESS_MINUS iexp
    { Put($1, $4, $7, ()) }
| iexp SEMICOLON iexp
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

fundef: /* (* e.g. f (y_1 : \tau_1)  ... (y_n : \tau_n) : \tau_f = ...*) */
| IDENT formal_args COLON types EQUAL iexp
    { { name = ($1,$4); args = $2; body = $6 } }

formal_args: /* (* e.g. (y_1 : \tau_1)  ... (y_n : \tau_n) *) */
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
| elems COMMA iexp
    { $1 @ [$3] }
| iexp COMMA iexp
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
 | types TYPE_ARRAY
    { TArray($1) }
 | LPAREN ltypes RPAREN
    {
      (* Consider in a special way list of length 1 *)
      if List.length $2 > 1 then
      	 TTuple($2)
      else
         List.hd_exn $2
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
