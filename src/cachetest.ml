open Annotast
open Cache.Cache

open OUnit2

let external_signatures = [
    "print_int" ,     Type.Fun([Type.Int], Type.Unit) ;
    "print_newline" , Type.Fun([Type.Unit], Type.Unit) ;
    "int_of_float",   Type.Fun([Type.Float], Type.Int) ;
    "float_of_int",   Type.Fun([Type.Int], Type.Float) ;
    "sin",            Type.Fun([Type.Float], Type.Float) ;
    "cos",            Type.Fun([Type.Float], Type.Float) ;
    "sqrt",           Type.Fun([Type.Float], Type.Float) ;
    "abs_float",      Type.Fun([Type.Float], Type.Float) ;
    "truncate",       Type.Fun([Type.Float], Type.Int);
  ]

let rec build_annot_list te = match te with
    | Unit(annot)
    | Bool(_, annot)
    | Int(_, annot)
    | Float(_, annot)
    | Var(_, annot) -> [annot]
    | Not(e1, annot)
    | Neg(e1, annot)
    | FNeg(e1, annot) -> annot :: build_annot_list e1
    | IBop(_, e1, e2, annot)
    | FBop(_, e1, e2, annot)
    | Rel(_, e1, e2, annot) -> annot :: List.append (build_annot_list e1) (build_annot_list e2)
    | If(e1, e2, e3, annot) -> annot :: List.append (build_annot_list e1) (List.append (build_annot_list e2) (build_annot_list e3))
    | Let(_, e1, e2, annot)
    | LetRec ({ name = _; args = _; body = e1 }, e2, annot) -> annot :: List.append (build_annot_list e1) (build_annot_list e2)
    | App (e1, es, annot) -> annot :: List.concat (List.map build_annot_list (e1::es))
    | Tuple(es, annot) -> annot :: List.concat (List.map build_annot_list es)
    | LetTuple(_, e1, e2, annot)
    | Array(e1, e2, annot)
    | Get (e1, e2, annot) -> annot :: List.append (build_annot_list e1) (build_annot_list e2)
    | Put (e1, e2, e3, annot) -> annot :: List.append (build_annot_list e1) (List.append (build_annot_list e2) (build_annot_list e3));;

let analyzeExpr (file : string) =
  Typing.extenv := M.add_list external_signatures M.empty;
  let channel = open_in file in
  let lexbuf = Lexing.from_channel channel in
  let e = Parser.exp Lexer.token lexbuf in
    let te = Typing.f e in
      let cache = Cache.buildCache te (M.add_list external_signatures M.empty) in (te, cache)

(* Test Fixture *)
let test_fixture =
"SimpleCache Tests">:::
[
  "fact">::( fun a ->
    let (te, cache) = analyzeExpr "examples/fact.ml" in
        let annot_list = build_annot_list te in
            (* Check the typing. TODO: check the context *)
            ignore (List.for_all (fun (hash, tau) -> (snd (Cache.Cache.find hash cache)) = tau) annot_list)
  )
]

(* Test Runner; ~verbose:true gives info on succ tests *)
let () = run_test_tt_main test_fixture


(*

open OUnit
open Awesome

let suite = "OUnit tests..." >:::  ["test_list_length" >::
                                    (fun () -> assert_equal "1" (str_of_t (succ one_t)))]

let _ =
  run_test_tt_main suite
*)
