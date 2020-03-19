open Core_bench
open Batteries

module Table = Batteries.Hashtbl.Make(struct
  type t = string
  let compare = Core.String.compare
  let equal = Core.String.equal
  let hash = Hashtbl.hash
end)

let table_of_sexp sexp =
  let tab = Table.create 100 in
  let rec _tab_of_sexp sexp =
    match sexp with
    | Core.Sexp.Atom k -> Printf.eprintf "W: add single atom %s\n" k; Table.add tab k "()"
    | Core.Sexp.List tl ->
      (match tl with
      | [] -> Printf.eprintf "W: empty\n"; ()
      | [Core.Sexp.Atom k; Core.Sexp.Atom v] -> Printf.eprintf "I: add (%s, %s)\n" k v; Table.add tab k v
      | _ -> List.iter _tab_of_sexp tl) in
  _tab_of_sexp sexp; tab

let print_table table =
  Enum.iter
    (fun k ->
      Printf.printf "%s |> [" k; List.iter (Printf.printf "%s;") (Table.find_all table k); Printf.printf "]\n")
    (Table.keys table)

let extract_result_csv field_list = ()
