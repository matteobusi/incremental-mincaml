let rec make_adder (x : int) : int -> int =
  let rec adder (y : int) : int = x + y in
  adder in
print_int((make_adder 3) 7)
