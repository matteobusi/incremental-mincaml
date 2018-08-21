{
(* lexer *)
(* From https://github.com/esumii/min-caml *)
(* Adapted to slightly different language @ UniPI 
Improvements/fixes:
    - English comments
    - Better errors
*)
open Parser
open Type
}

(* Shorthands for frenquently used regexes *)
let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

(* Actual rules *)
rule token = parse
| space+
    { token lexbuf }
| "(*"
    { comment lexbuf;
      token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| "true"
    { BOOL(true) }
| "false"
    { BOOL(false) }
| "not"
    { NOT }
| digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| '-' (* Int ops *)
    { MINUS }
| '+'
    { PLUS }
| "-." (* Float ops *)
    { MINUS_DOT }
| "+."
    { PLUS_DOT }
| "*."
    { AST_DOT }
| "/."
    { SLASH_DOT }
| '='
    { EQUAL }
| "<>"
    { LESS_GREATER }
| "<="
    { LESS_EQUAL }
| ">="
    { GREATER_EQUAL }
| '<'
    { LESS }
| '>'
    { GREATER }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "let"
    { LET }
| "in"
    { IN }
| "rec"
    { REC }
| ','
    { COMMA }
| '_'
    { IDENT(Id.gentmp Type.Unit) }
| "Array.create" | "Array.make" (* [XX] ad hoc *)
    { ARRAY_CREATE }
| '.'
    { DOT }
| "<-"
    { LESS_MINUS }
| ';'
    { SEMICOLON }
| eof
    { EOF }
| ":"
    { COLON }
| "->"
    { ARROW }
| "*"
    { AST }
| "int"
    { TYPE_INT }
| "bool"
    { TYPE_BOOL }
| "float"
    { TYPE_FLOAT }
| "unit"
    { TYPE_UNIT }
| "["
    { LBRACKET }
| "]"
    { RBRACKET }
| "->"
    { ARROW }
| lower (digit|lower|upper|'_')* (* Identifiers: start with lower case letters, then continue with 0 or more chars *)
    { IDENT(Lexing.lexeme lexbuf) }
| _
    { failwith
        (Printf.sprintf "Syntax Error: unknown token %s @ %d:%d"
           (Lexing.lexeme lexbuf)
           ((Lexing.lexeme_start_p lexbuf).pos_lnum)
           ((Lexing.lexeme_start_p lexbuf).pos_cnum)) }
and comment = parse
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { Format.eprintf "Warning: unterminated comment!" }
| _
    { comment lexbuf }
