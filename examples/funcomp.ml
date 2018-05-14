let rec compose (f : int -> int) (g : int -> int) : int -> int =
  let rec composed (x : int) : int = g (f x) in
  composed in
let rec dbl (x : int) : int = x + x in
let rec inc (x : int) : int = x + 1 in
let rec dec (x : int) : int = x - 1 in
let h = compose inc (compose dbl dec) in
print_int (h 123)
