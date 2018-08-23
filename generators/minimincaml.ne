# Nearley's BNF for MiniMinCaml
# This means that, w.r.t MinCaml in generated programs
#   - let-bound variables are never used
#   - there is no function declaration
#   - if statements are restricted
#   - free variables in expressions are from productions at the end of file
#   - application specify a function name (from fid) and a single integer parameter (from nid)
#   - just int and bool expressions are present
#   - all parentheses are explicit
#   - no sequencing
# it is guaranteed that all generated programs are in fact typeable

e ->  "if" _ bexp _ "then" _ "(" e ")" _ "else" _ "(" e ")" # If takes a bexp as condition, 
    | "let" _ lid _ "=" _  "(" e ")" _ "in" _  "(" e ")" # All new identifiers have prefix id_ 
    | "(" fid _ nexp ")" # Application must use one of the fid names above, actual arguments are nexp
    | nexp

id -> [a-zA-Z] # Single letter ids

bval -> "false" | "true" | bid
digit -> [0-9]
num -> digit | nid # Single digit numbers

bexp -> bval | "(" nexp _ cop _ nexp ")" | "(" "not" _ bexp ")"
cop -> "=" | "<>" | "<" | ">" | "<=" | ">="

nexp -> num | "(" nexp _ ibop _ nexp ")"
ibop -> "+" | "-" | "*" 

_ -> [ ]

# WARNING: the following names must be known in the initial context, i.e. the initial context should be aware of b00-b99, n00-n99, f00-f99 
bid -> "b_" idPost # bool
nid -> "n_" idPost # int
fid -> "f_" idPost # int -> int
lid -> "l_" idPost # int

idPost -> digit:+