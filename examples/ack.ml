let rec ack (x : int) (y : int) : int =
  if x <= 0 then
    y + 1
  else
    if y <= 0 then
      ack (x - 1) 1
    else
      ack (x - 1) (ack x (y - 1))
in
print_int(ack 3 10)
