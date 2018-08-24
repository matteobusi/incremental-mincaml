(* customized version of Map *)
open Batteries
open Varset

module M =
struct
  include Map.Make 
      (struct
        type t = Id.t
        let compare = compare
      end)

  let add_list xys env = List.fold_left (fun env (x, y) -> add x y env) env xys
  let add_list2 xs ys env = List.fold_left2 (fun env x y -> add x y env) env xs ys
  let restrict gamma e = let fv = Annotast.free_variables e in filter (fun k _ -> VarSet.mem k fv) gamma
  let restrict_varset gamma fv = filter (fun k _ -> VarSet.mem k fv) gamma

end