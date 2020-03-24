let rec avg (v : [int]) (sz : int) : int = 
    let rec sum (v : [int]) (sz : int) : int =
        (if sz = 1 then v.(sz)
                  else v.(sz) + (sum v (sz-1))) 
    in int_of_float(float_of_int(sum v sz) /. float_of_int(sz))
in (avg (Array.make 5 42) 5)