open FunSpecification

(* From https://github.com/esumii/min-caml *)
type t = string (* MinCaml *)
type l = L of string

let rec pp_list = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ pp_list xs

let counter = ref 0
let genid s =
  incr counter;
  Printf.sprintf "%s.%d" s !counter

let rec id_of_typ = function
  | FunSpecification.TUnit -> "u"
  | FunSpecification.TBool -> "b"
  | FunSpecification.TInt -> "i"
  | FunSpecification.TFloat -> "d"
  | FunSpecification.TFun _ -> "f"
  | FunSpecification.TTuple _ -> "t"
  | FunSpecification.TArray _ -> "a"
(*  | TVar _ -> assert false *)

let gentmp typ =
  incr counter;
  Printf.sprintf "T%s%d" (id_of_typ typ) !counter

let equal = String.equal
