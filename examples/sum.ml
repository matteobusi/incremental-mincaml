let rec sum (x : int) : int =
  if x <= 0 then 0 else
  sum (x - 1) + x in
print_int (sum 10000)

let rec sum (x : int) : int =
  if x <= 0 then 0 else
  if x <= 1 then (x-1) + x else
  sum (x - 1) + x in
print_int (sum 10000)

let rec sum (x : int) : int =
  if x <= 1 then x else
  sum (x - 1) + x in
print_int (sum 10000)

let sum (x : int) : int =
  if x <= 1 then x else
  let rec sum_prime acc x = 
      if x <= 1 then sum x else
  in sum_prime (acc + x) (x - 1) 
in sum 10 

let rec sum_cps (x: int) (k : int -> int) : int =
  if x <= 0 then 0 else
  k (sum_cps (x-1) (let rec tmp s x = s + x in tmp)) x

sum 2 k
 -> k (sum 1 (fun s x -> s + x)) 2
  -> k ( k (sum 0 (fun s x -> )) 1)
    -> k ( k (0) 1) 2
      -> k (1) 2
        -> 3