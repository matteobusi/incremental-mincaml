let rec sum (v : [float]) (sz : int) : float = (
    if sz = 1 then v.(sz) 
    else v.(sz) +. (sum v (sz-1))
) in 
let rec avg (v : [float]) (sz : int) : float = (sum v sz) /. float_of_int(sz) in 
(avg (Array.make 5 2.56) 5)