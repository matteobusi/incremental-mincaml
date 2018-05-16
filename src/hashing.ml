open Annotast

let factor1 : int = 3 (* 37 *)
let factor2 : int = 53

let combine_hashes hashes =
  List.fold_left (fun pres h -> factor1 * h + pres) 0 hashes

let compute_hash = Hashtbl.hash

let extract_hash e = match e with
| Unit(h, _)
| Bool(_,(h, _))
| Int(_, (h, _))
| Float(_,(h, _))
| Not(_, (h, _))
| Var(_, (h, _))
| Neg(_, (h, _))
| FNeg(_, (h, _))
| IBop(_,_,_,(h, _))
| FBop(_,_,_,(h, _))
| Rel(_,_,_,(h, _))
| If(_,_,_, (h, _))
| Let(_,_,_, (h, _))
| LetRec(_,_, (h, _))
| App(_,_,(h, _))
| Tuple(_,(h, _))
| LetTuple(_,_,_,(h, _))
| Array(_,_, (h, _))
| Get(_,_, (h, _))
| Put(_, _,_, (h, _))-> h