open Annotast

let factor1 : int = 3 (* 37 *)
let factor2 : int = 53

let combine_hashes hashes =
  List.fold_left (fun pres h -> factor1 * h + pres) 0 hashes

let compute_hash = Hashtbl.hash


(*
   Questa può essere definita in termini di Annotast.get_annot
   e di snd

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
*)

let extract_hash e = Annotast.get_annot e |> fst

(*
   Questa può essere definita in termini di Annotast.get_annot

let extract_simple_hash e = match e with
| Unit(h)
| Bool(_,h)
| Int(_, h)
| Float(_,h)
| Not(_, h)
| Var(_, h)
| Neg(_, h)
| FNeg(_, h)
| IBop(_,_,_,h)
| FBop(_,_,_,h)
| Rel(_,_,_,h)
| If(_,_,_, h)
| Let(_,_,_, h)
| LetRec(_,_, h)
| App(_,_,h)
| Tuple(_,h)
| LetTuple(_,_,_,h)
| Array(_,_, h)
| Get(_,_, h)
| Put(_, _,_, h)-> h
 *)

let extract_simple_hash e = Annotast.get_annot e
