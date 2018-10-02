(* From https://github.com/esumii/min-caml *)

(* MinCamL types *)
type t =
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t

(* Type pretty printer *)
let rec type_ppf ppf (type_t : t) =
  match type_t with
  | Unit -> Format.fprintf ppf "unit"
  | Int -> Format.fprintf ppf "int"
  | Bool -> Format.fprintf ppf "bool"
  | Float -> Format.fprintf ppf "float"
  | Array(t) -> Format.fprintf ppf "[%a]" type_ppf t
  | Tuple(ts) -> Format.fprintf ppf "@[<2>";
    typelist_syntax_ppf ppf ts;
    Format.fprintf ppf "@]"
  | Fun(args,rt) -> Format.fprintf ppf "@[<2>";
    typelist_syntax_ppf ppf args;
    Format.fprintf ppf " -> %a@]" type_ppf rt
and typelist_syntax_ppf ppf ts =
  Format.pp_print_list type_ppf ~pp_sep:(fun ppf () -> Format.pp_print_char ppf '*') ppf ts

(* Use the pretty printer to extract string from a type *)
let string_of_type t = Format.fprintf Format.str_formatter "%a" type_ppf t; Format.flush_str_formatter ()
