open Batteries

module OriginalFunAlgorithm = Original.TypeAlgorithm(FunSpecification.FunSpecification)
module IncrementalFunAlgorithm = Incrementalizer.TypeAlgorithm(FunSpecification.FunSpecification)

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

let rec nodecount e = match e with
  | Unit(annot)
  | Bool(_, annot)
  | Int(_, annot)
  | Float(_, annot)
  | Var(_, annot) -> 1
  | Not(e1, annot)
  | Neg(e1, annot)
  | FNeg(e1, annot) -> 1 + nodecount e1
  | IBop(_, e1, e2, annot)
  | FBop(_, e1, e2, annot)
  | Rel(_, e1, e2, annot) -> 1 + nodecount e1 + nodecount e2
  | If(e1, e2, e3, annot) -> 1 + nodecount e1 + nodecount e2 + nodecount e3
  | Let(_, e1, e2, annot) -> 2 + nodecount e1 + nodecount e2 (* curr node + x *)
  | LetRec ({ name = _; args = yts; body = e1 }, e2, annot) -> 2 + (List.length yts) + nodecount e1 + nodecount e2
  | App (e1, es, annot) -> 1 + (List.fold_left (+) 0 (List.map nodecount (e1::es)))
  | Tuple(es, annot) -> 1 + (List.fold_left (+) 0 (List.map nodecount es))
  | LetTuple(xs, e1, e2, annot) -> 1 + List.length xs + nodecount e1 + nodecount e2
  | Array(e1, e2, annot)
  | Get (e1, e2, annot) -> nodecount e1 + nodecount e2
  | Put (e1, e2, e3, annot) -> nodecount e1 + nodecount e2 + nodecount e3

let analyze_expr (file : string) (filem : string) =
    Printf.printf "Analyzing: Orig: %s ... Mod: %s ...\n" file filem;
    let channel, channelm = open_in file, open_in filem in
    let lexbuf, lexbufm = Lexing.from_channel channel, Lexing.from_channel channelm in
    let e, em = Parser.exp Lexer.token lexbuf, Parser.exp Lexer.token lexbufm in
    let e_hf, em_hf = (Id.counter := 0; OriginalFunAlgorithm.term_map (fun e -> (compute_hash e, compute_fv e)) e), (Id.counter := 0; OriginalFunAlgorithm.term_map (fun e -> (compute_hash e, compute_fv e)) em) in (* Id.counter := 0 to to avoid that the same subtree gets different hashes *)
    let gamma_init, gamma_initm = (FunContext.add_list (initial_gamma_list) (FunContext.get_empty_context ())), (FunContext.add_list (initial_gamma_list) (FunContext.get_empty_context ())) in
    (* Printf.printf "Program: %s\n" (FunSpecification.FunSpecification.string_of_term (fun f x -> ()) e);
    Printf.printf "Program Mod: %s\n" (FunSpecification.FunSpecification.string_of_term (fun f x -> ()) em); *)
    let cache = IncrementalFunAlgorithm.get_empty_cache 4096 in
    ignore (IncrementalFunAlgorithm.build_cache e_hf gamma_init cache);
    IncrementalFunAlgorithm.IncrementalReport.reset IncrementalFunAlgorithm.report;
    IncrementalFunAlgorithm.IncrementalReport.set_nc (nodecount em_hf) IncrementalFunAlgorithm.report;
    let te, tem = OriginalFunAlgorithm.typing gamma_init e_hf, IncrementalFunAlgorithm.typing cache gamma_initm em_hf in
        Printf.printf "Type: %s - IType: %s\n" (FunSpecification.FunSpecification.string_of_type te) (FunSpecification.FunSpecification.string_of_type tem);
        Printf.printf "%s\n" (IncrementalFunAlgorithm.IncrementalReport.string_of_report IncrementalFunAlgorithm.report)


let _ =
    if Array.length Sys.argv = 3 then
        begin
            analyze_expr Sys.argv.(1) Sys.argv.(2);
            exit 0
        end
    else
        Printf.printf "Usage:\n %s file1.ml file2.ml\n" Sys.argv.(0)
