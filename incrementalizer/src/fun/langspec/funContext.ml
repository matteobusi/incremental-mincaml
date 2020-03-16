open Batteries

include Hashtbl.Make
(struct
    type t = string
    let compare = String.compare
    let hash = Hashtbl.hash
    let equal = String.equal
end)

let get_empty_context () = create 4096

let add x t env = let envc = copy env in add envc x t; envc
let add_list xys env = List.fold_left (fun env (x, y) -> add x y env) env xys
let add_list2 xs ys env = List.fold_left2 (fun env x y -> add x y env) env xs ys

let find = find
