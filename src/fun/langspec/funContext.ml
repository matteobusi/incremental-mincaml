open Core

include Hashtbl.Make
(struct
    type t = string
    [@@deriving compare, sexp, hash]
end)

let get_empty_context () = create ()

let add x t env = let envc = copy env in ignore (add envc x t); envc
let add_list xys env = List.fold_left xys ~init:env ~f:(fun env (x, y) -> add x y env)
let add_list2 xs ys env = add_list (List.zip_exn xs ys) env

[@@deriving compare, sexp, hash]
