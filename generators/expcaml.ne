# Nearley's BNF for ExpMinCaml
# This means that programs are:
#   - Numerical/Boolean expressions
#   - Function applications
# it is guaranteed that all generated programs are in fact typeable
# 
# Type checker is expected to bind in \Gamma any free variable either to int, bool or int -> int

e ->  "(" fid _ nexp ")" # Application must use one of the fid names above, actual arguments are nexp
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
fid -> "f_" idPostdigit # int -> int

idPost -> digit+