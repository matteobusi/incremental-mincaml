open Core

include Hashtbl.Make
(struct
    type t = string
    [@@deriving compare, sexp, hash]
end)

let get_empty_context ?size = create ~growth_allowed:true ?size:size

let inplace_add = add

let add x t env = let envc = copy env in ignore (inplace_add envc x t); envc
let add_list xys env = let envc = copy env in List.iter xys ~f:(fun (x, y) -> ignore (inplace_add envc x y)); envc
let add_list2 xs ys env = add_list (List.zip_exn xs ys) env

[@@deriving compare, sexp, hash]
