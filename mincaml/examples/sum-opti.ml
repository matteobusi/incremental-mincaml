let rec sum (x : int) : int =
  if x <= 1 then x else
  sum (x - 1) + x in
print_int (sum 10000)
