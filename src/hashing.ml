let factor1 : int = 3 (* 37 *)
let factor2 : int = 53


let combine_hashes hashes =
  List.fold_left (fun pres h -> factor1 * h + pres) 0 hashes

let compute_hash = Hashtbl.hash
