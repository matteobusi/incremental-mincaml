(*
  Hello World
*)

open Annotast
open Typing

(*
   Prove di annotazioni
 *)

type hash_annot = H of int
let string_of_hash (H(n)) = string_of_int n
let string_of_hashtype (H(n),t) = Printf.sprintf "%d - %s" n (Type.string_of_type t)


let  letExpr1 =  Let(("z", Type.Int), Int(17,H(0)),
                     Add(
                         Let(("z", Type.Int), Int(22,H(1)),
                             Add(Int(100,H(3)), Var("z",H(4)), H(5)),H(6)),
                       Var("z",H(6)),H(7)),H(8))

let tupleExpr1 = Tuple([Int(1, H(0)); Int(2,H(1)); Int(3,H(2))], H(3))

let letTuple1 =  LetTuple(["ciao", Type.Int; "casa", Type.Bool],
                          Tuple([Int(10,H(0)); Bool(true, H(1))], H(2)),
                          (Int(1,H(3))),H(4))



let exprs = [letExpr1; tupleExpr1 ; letTuple1]


(*
let  letExpr1 =  Let(("z", Type.Int), Int 17,
                     Add(
                         Let(("z", Type.Int), Int 22,
                             Add(Int 100, Var "z")),
                         Var("z")))


let _ =  Printf.printf "Hello there:\n %s\n" (Syntax.string_of_syntax letExpr1);
         print_string (Type.string_of_type (Typing.f letExpr1));
         print_newline ()
 *)
let _ = List.iter (fun e -> print_string (Annotast.string_of_annotast string_of_hashtype  e); print_newline ()) (List.map Typing.f exprs)
(*
  let print_expr = Syntax.syntax_ppf Format.std_formatter in
  List.iter (fun e -> print_expr e; print_newline ()) exprs

Printf.printf "Hello there:\n %s\n" (Syntax.string_of_syntax letExpr1);
*)
