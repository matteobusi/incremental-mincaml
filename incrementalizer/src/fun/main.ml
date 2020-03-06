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

let rec term_map (fn : 'a term -> 'b) (t : 'a term) : 'b term =
  let ts = List.map snd (get_sorted_children t) in
      let ts' = List.map (fun ti -> term_map fn ti) ts in
          term_edit t ts' (fn t)

let analyzeExpr (file : string) (filem : string) =
    Printf.printf "Analyzing: Orig: %s ... Mod: %s ...\n" file filem;
    let channel, channelm = open_in file, open_in filem in
    let lexbuf, lexbufm = Lexing.from_channel channel, Lexing.from_channel channelm in
    let e, em = Parser.exp Lexer.token lexbuf, Parser.exp Lexer.token lexbufm in
    let e_hf, em_hf = (Id.counter := 0; term_map (fun e -> (compute_hash e, compute_fv e)) e), (Id.counter := 0; term_map (fun e -> (compute_hash e, compute_fv e)) em) in (* Id.counter := 0 to to avoid that the same subtree gets different hashes *)
    let gamma_init, gamma_initm = (FunContext.add_list (initial_gamma_list) (FunContext.empty ())), (FunContext.add_list (initial_gamma_list) (FunContext.empty ())) in
    (* Printf.printf "Program: %s\n" (FunSpecification.FunSpecification.string_of_term (fun f x -> ()) aast); *)
    let cache = IncrementalFunAlgorithm.get_empty_cache in
    IncrementalFunAlgorithm.build_cache e_hf gamma_init cache;
    let te, tem = OriginalFunAlgorithm.typing gamma_init e_hf, IncrementalFunAlgorithm.typing cache gamma_initm em_hf in
        Printf.printf "Type: %s - IType: %s\n" (FunSpecification.FunSpecification.string_of_type te) (FunSpecification.FunSpecification.string_of_type tem)

let _ =
    if Array.length Sys.argv = 3 then
        begin
            analyzeExpr Sys.argv.(1) Sys.argv.(2);
            exit 0
        end
    else
        Printf.eprintf "Usage:\n %s file1.ml file2.ml\n" Sys.argv.(0)
