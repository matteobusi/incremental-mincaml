let rec avg (v : [int]) (sz : int) : float = 
    let rec sum (v : [int]) (sz : int) : float =
        (if sz = 1 then float_of_int(v.(sz)) 
                  else float_of_int(v.(sz)) +. (sum v (sz-1))) 
    in (sum v sz) /. float_of_int(sz)
in (avg (Array.make 5 42) 5)