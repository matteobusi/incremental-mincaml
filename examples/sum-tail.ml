let rec sum (acc: int) (x : int) : int =
  if x <= 0 then acc else
  sum (acc + x) (x - 1) in
print_int (sum 0 10000)
