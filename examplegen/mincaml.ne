# Nearley's BNF for MinCamL
e ->  "if" __ e __ "then" __ "(" e ")" __ "else" __ "(" e ")" # WARN: restricted to just binary expressions!
    |   "let" __ id "="  "(" e ")" __ "in" __  "(" e ")" | "let" __ "rec" __ fdef __ "in" __  "(" e ")" | "let" __ "(" pat ")" __ "=" __  "(" e ")" __ "in" __  "(" e ")"
    |   "Array.create" __ "(" e ")" __ "(" e ")"
    |   "(" se __ aargs ")" 
    |   els
    |   "(" e ";" __ e ")"
    |   se 
    |   bexp | nexp | fexp

se ->   "(" e ")"
    |   "(" ")"
    |   bval | num | fl | id
    |   se "." "(" e ")"

fdef -> id __ fargs ":" type __ "=" __ "(" e ")"
fargs -> "(" id ":" type ")" __ fargs | "(" id ":" type ")"
aargs -> aargs __ se | se
els -> els "," __ e | e "," __ e
pat -> pat "," __ id | id "," __ id 
type -> "int" | "bool" | "unit" | "float" | "[" type "]" | "(" ltype ")" | ltype "->" type 
ltype -> type "*" ltype | type 
id -> [a-z] [a-zA-Z0-9]:*

bval -> "false" | "true"
num -> [0-9]:+
fl -> [0-9]:+ "." [0-9]:+

bexp -> bval | "(" nexp __ cop __ nexp ")" | "(" "not" __ bexp ")"
cop -> "=" | "<>" | "<" | ">" | "<=" | ">="

nexp -> num | "(" nexp __ ibop __ nexp ")" | "(" "-" __ nexp ")"
ibop -> "+" | "-" | "*" 

fexp -> fl | "(" fexp __ fbop __ fexp ")" | "(" "-." __ fexp ")"
fbop -> "+." | "-." | "*." | "/."

__ -> wschar
wschar -> [ ]