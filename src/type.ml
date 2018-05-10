(* From https://github.com/esumii/min-caml *)

type t = (* MinCaml *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t

(*
   Var of t option ref

let gentyp () = Var(ref None)
 *)

(* ugly *)
let rec string_of_type (type_t : t) : string =
  match type_t with
  | Unit             -> "unit"
  | Bool             -> "bool"
  | Int              -> "int"
  | Float            -> "float"
  | Array(t)         -> Printf.sprintf "%s array" (string_of_type t)
  | Tuple(ts)        -> string_of_typelist ts
  | Fun(args,rt)     -> Printf.sprintf "%s -> %s" (string_of_typelist args) (string_of_type rt)
and string_of_typelist (ts : t list) : string =
  let body = String.concat "*" (List.map string_of_type ts) in
  Printf.sprintf "(%s)" body


let rec type_ppf ppf (type_t : t) =
  match type_t with
  | Unit -> Format.fprintf ppf "@[<2>unit@]"
  | Int -> Format.fprintf ppf "@[<2>int@]"
  | Bool -> Format.fprintf ppf "@[<2>bool@]"
  | Float -> Format.fprintf ppf "@[<2>float@]"
  | Array(t) -> Format.fprintf ppf "@[<2>%a array@]" type_ppf t
  | Tuple(ts) -> Format.fprintf ppf "@[<2>";
                 typelist_syntax_ppf ppf ts;
                 Format.fprintf ppf "@]"
  | Fun(args,rt) -> Format.fprintf ppf "@[<2>";
                    typelist_syntax_ppf ppf args;
                    Format.fprintf ppf "@]"
and typelist_syntax_ppf ppf ts =
  Format.pp_print_list type_ppf ~pp_sep:(fun ppf () -> Format.pp_print_char ppf '*') ppf ts
