(* known function optimization from https://github.com/esumii/min-caml *)
(* Cf. http://www.yl.is.s.u-tokyo.ac.jp/~sumii/pub/compiler-enshu-2002/Mail/8 *)
let rec f (x: int) : int = x + 123 in
let rec g (y : int) : int -> int = f in
print_int ((g 456) 789)
