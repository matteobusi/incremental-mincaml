open FunSpecification

(* Type pretty printer *)
let rec type_ppf ppf type_t =
  match type_t with
  | FunSpecification.TUnit -> Format.fprintf ppf "unit"
  | FunSpecification.TInt -> Format.fprintf ppf "int"
  | FunSpecification.TBool -> Format.fprintf ppf "bool"
  | FunSpecification.TFloat -> Format.fprintf ppf "float"
  | FunSpecification.TArray(t) -> Format.fprintf ppf "[%a]" type_ppf t
  | FunSpecification.TTuple(ts) -> Format.fprintf ppf "@[<2>";
    typelist_syntax_ppf ppf ts;
    Format.fprintf ppf "@]"
  | FunSpecification.TFun(args,rt) -> Format.fprintf ppf "@[<2>";
    typelist_syntax_ppf ppf args;
    Format.fprintf ppf " -> %a@]" type_ppf rt
and typelist_syntax_ppf ppf ts =
  Format.pp_print_list type_ppf ~pp_sep:(fun ppf () -> Format.pp_print_char ppf '*') ppf ts

(* Use the pretty printer to extract string from a type *)
let string_of_type t = Format.fprintf Format.str_formatter "%a" type_ppf t; Format.flush_str_formatter ()
