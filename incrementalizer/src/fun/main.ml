module OriginalFunAlgorithm = Original.TypeAlgorithm(FunSpecification.FunSpecification)

open FunSpecification.FunSpecification

let initial_gamma_list = [
  "print_int" ,     TFun([TInt], TUnit) ;
  "print_newline" , TFun([TUnit], TUnit) ;
  "int_of_float",   TFun([TFloat], TInt) ;
  "float_of_int",   TFun([TInt], TFloat) ;
  "sin",            TFun([TFloat], TFloat) ;
  "cos",            TFun([TFloat], TFloat) ;
  "sqrt",           TFun([TFloat], TFloat) ;
  "abs_float",      TFun([TFloat], TFloat) ;
  "truncate",       TFun([TFloat], TInt);
]

let analyzeExpr (file : string) =
    let rec make_aast e = (
        let info = (compute_hash e, compute_fv e) in
        match e with
        | Unit(_) -> Unit(info)
        | Bool(v, _) -> Bool(v, info)
        | Int(v, _) -> Int(v, info)
        | Float(v, _) -> Float(v, info)
        | Not(e1, _) -> Not(make_aast e1, info)
        | Var(x, _) -> Var(x, info)
        | Neg(e1, _) -> Neg(make_aast e1, info)
        | FNeg(e1, _) -> FNeg(make_aast e1, info)
        | IBop(o, e1, e2, _) -> IBop(o, make_aast e1, make_aast e2, info)
        | FBop(o, e1, e2, _) -> FBop(o, make_aast e1, make_aast e2, info)
        | Rel(o, e1, e2, _) -> Rel(o, make_aast e1, make_aast e2, info)
        | If(b,e1,e2, _) -> If(make_aast b, make_aast e1, make_aast e2, info)
        | Let(x, e1, e2, annot) -> Let(x, make_aast e1, make_aast e2, info)
        | LetRec({ name = (f, t); args = al; body = e2 }, e1, _) ->
            LetRec({ name = (f, t); args = al; body = make_aast e2 }, make_aast e1, info)
        | App(e1, e2, _) -> App(make_aast e1, List.map make_aast e2, info)
        | Tuple(e1, _) -> Tuple (List.map make_aast e1, info)
        | LetTuple(xs, e1, e2, _) -> LetTuple(xs, make_aast e1, make_aast e2, info)
        | Array(e1, e2, _) -> Array(make_aast e1, make_aast e2, info)
        | Get(e1, e2, _) -> Get(make_aast e1, make_aast e2, info)
        | Put(e1, e2, e3, _) -> Put(make_aast e1, make_aast e2, make_aast e3, info)
    )
    in
    Printf.printf "Analyzing: Orig: %s ...\n" file;
    let channel = open_in file in
    let lexbuf = Lexing.from_channel channel in
    let e = Parser.exp Lexer.token lexbuf in (* For simplicity we use get_hash and get_fv directly in the parsing phase *)
    let aast = make_aast e in
    Id.counter := 0; (* Fix to avoid situations where the same subtree has different hashes *)
    let gamma_init = (FunContext.add_list (initial_gamma_list) (FunContext.empty ())) in
    let te = OriginalFunAlgorithm.typing gamma_init aast in
        Printf.printf "Type: %s\n" (FunPrettyPrinter.string_of_type te)

let _ =
    if Array.length Sys.argv = 2 then
        begin
            analyzeExpr Sys.argv.(1);
            exit 0
        end
    else
        Printf.eprintf "Usage:\n %s file1.ml\n" Sys.argv.(0)
