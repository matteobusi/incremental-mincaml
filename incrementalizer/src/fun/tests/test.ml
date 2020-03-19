open Batteries
open OUnit2

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

let inc_analyze_expr (file : string) =
  let channel = open_in file in
  let lexbuf = Lexing.from_channel channel in
  let e = (Parser.exp Lexer.token lexbuf) in
  let e_hf = (Id.counter := 0; OriginalFunAlgorithm.term_map (fun e -> (compute_hash e, compute_fv e)) e) in
  let gamma_init = FunContext.add_list (initial_gamma_list) (FunContext.get_empty_context ()) in
  let te = OriginalFunAlgorithm.typing gamma_init e_hf in
  let cache = IncrementalFunAlgorithm.get_empty_cache 4096 in
  let aast = IncrementalFunAlgorithm.build_cache e_hf gamma_init cache in
    (te, aast, cache)

let analyze_and_report (file : string) (filem : string) =
  let channel, channelm = open_in file, open_in filem in
  let lexbuf = Lexing.from_channel channel in
  let e = Parser.exp Lexer.token lexbuf in
  let e_hf = (Id.counter := 0; OriginalFunAlgorithm.term_map (fun e -> (compute_hash e, compute_fv e)) e) in
  Id.counter := 0; (* Fix to avoid situations where the same subtree has different hash *)
  let lexbufm = Lexing.from_channel channelm in
  let em = Parser.exp Lexer.token lexbufm in
  let em_hf = (Id.counter := 0; OriginalFunAlgorithm.term_map (fun e -> (compute_hash e, compute_fv e)) em) in
  let gamma_init = FunContext.add_list (initial_gamma_list) (FunContext.get_empty_context ()) in
  let te, tem = OriginalFunAlgorithm.typing gamma_init e_hf, OriginalFunAlgorithm.typing gamma_init em_hf in (* tem computed just to compare the results! *)
  let cache = IncrementalFunAlgorithm.get_empty_cache 4096 in
  ignore (IncrementalFunAlgorithm.build_cache e_hf gamma_init cache);
  IncrementalFunAlgorithm.IncrementalReport.reset IncrementalFunAlgorithm.report;
  IncrementalFunAlgorithm.IncrementalReport.set_nc (nodecount em_hf) IncrementalFunAlgorithm.report;
  let inctem = IncrementalFunAlgorithm.typing cache gamma_init em_hf in (*Analyse the modified program *)
  Printf.printf "[%s v. %s] - %s\n" file filem (IncrementalFunAlgorithm.IncrementalReport.string_of_report IncrementalFunAlgorithm.report);
    (tem, inctem)

let check_cache_result file =
  let (te, aast, cache) = inc_analyze_expr file in
  let annot_list = build_annot_list aast in
  assert_bool ("[Cache] BuildCache wrong type at root: " ^ file) (te = fst (term_getannot aast));
  assert_bool ("[Cache] Failed: " ^ file) ((List.for_all (fun (tau, (hash, fv)) -> (snd (IncrementalFunAlgorithm.Cache.find cache hash)) = tau) annot_list))

let run fv_c depth =
  (* Fill up the initial gamma with needed identifiers *)
  let e = Generator.gen_ibop_ids_ast depth "+" fv_c in
  let initial_gamma_list e = List.map (fun id -> (id, TInt)) (VarSet.elements (compute_fv e)) in
  let gamma_init = (FunContext.add_list (initial_gamma_list e) (FunContext.get_empty_context ()) ) in
  (* These are just to avoid multiple recomputations *)
  let full_cache = IncrementalFunAlgorithm.get_empty_cache 4096 in
  ignore (IncrementalFunAlgorithm.build_cache e gamma_init full_cache);
  IncrementalFunAlgorithm.IncrementalReport.reset IncrementalFunAlgorithm.report;
  IncrementalFunAlgorithm.IncrementalReport.set_nc (nodecount e) IncrementalFunAlgorithm.report;
  ignore (IncrementalFunAlgorithm.typing full_cache gamma_init e); (*Analyse the modified program *)
  Printf.printf "transf=id; fv_c=%d; depth=%d - %s\n\n" fv_c depth (IncrementalFunAlgorithm.IncrementalReport.string_of_report IncrementalFunAlgorithm.report)

let run_mod fv_c depth inv_depth =
  (* Fill up the initial gamma with needed identifiers *)
  let e = Generator.gen_ibop_ids_ast depth "+" fv_c in
  let initial_gamma_list e = (List.map (fun id -> (id, TInt)) (VarSet.elements (compute_fv e))) in
  let gamma_init = (FunContext.add_list (initial_gamma_list e) (FunContext.get_empty_context ()) ) in
  let full_cache = IncrementalFunAlgorithm.get_empty_cache 4096 in
  (* Build the full cache for e *)
  ignore (IncrementalFunAlgorithm.build_cache e gamma_init full_cache);
  (* Invalidate part of the cache, corresponding to the rightmost subtree of depth tree_depth - d; This simulates diffs. *)
  Generator.simulate_modification full_cache e inv_depth;
  IncrementalFunAlgorithm.IncrementalReport.reset IncrementalFunAlgorithm.report;
  IncrementalFunAlgorithm.IncrementalReport.set_nc (nodecount e) IncrementalFunAlgorithm.report;
  ignore (IncrementalFunAlgorithm.typing full_cache gamma_init e); (*Analyse the modified program *)
  Printf.printf "transf=mod; fv_c=%d; depth=%d; inv_depth=%d - %s\n\n" fv_c depth inv_depth (IncrementalFunAlgorithm.IncrementalReport.string_of_report IncrementalFunAlgorithm.report)

let check_incremental_result fileo filem = let res = analyze_and_report fileo filem in assert_equal (fst res) (snd res)

(* Test Fixture *)
let test_cache  = "Test: Cache">:::
  [
    "fact.ml">::(fun _ -> check_cache_result "src/fun/examples/fact.ml" );
    "sum-orig.ml">::(fun _ -> check_cache_result "src/fun/examples/sum-orig.ml" );
    "sum-tail.ml">::(fun _ -> check_cache_result "src/fun/examples/sum-tail.ml" );
    "ack.ml">::(fun _ -> check_cache_result "src/fun/examples/ack.ml" );
  ]

let test_incr = "Test: Incremental TC">:::
  [
    "fact.ml - fact.ml">::(fun _ -> check_incremental_result "src/fun/examples/fact.ml" "src/fun/examples/fact.ml" );
    "fact.ml - fact_opt.ml">::(fun _ -> check_incremental_result "src/fun/examples/fact.ml" "src/fun/examples/fact_opt.ml" );
    "sum-orig.ml - sum-tail.ml">::(fun _ -> check_incremental_result "src/fun/examples/sum-orig.ml" "src/fun/examples/sum-tail.ml" );
    "inprod.ml - inprod-loop.ml">::(fun _ -> check_incremental_result "src/fun/examples/inprod.ml" "src/fun/examples/inprod-loop.ml");
    "fact.ml - ack.ml">::(fun _ -> check_incremental_result "src/fun/examples/fact.ml" "src/fun/examples/ack.ml" );
    "sum-orig.ml - sum-opti.ml">::(fun _ -> check_incremental_result "src/fun/examples/sum-orig.ml" "src/fun/examples/sum-opti.ml" );
    "array-avg.ml - array-avg-codemotion.ml">::(fun _ -> check_incremental_result "src/fun/examples/array-avg.ml" "src/fun/examples/array-avg-codemotion.ml" );
    "array-avg.ml - array-avg-if.ml">::(fun _ -> check_incremental_result "src/fun/examples/array-avg.ml" "src/fun/examples/array-avg-if.ml" );
    "array-avg.ml - array-avg-ii.ml">::(fun _ -> check_incremental_result "src/fun/examples/array-avg.ml" "src/fun/examples/array-avg-ii.ml" );
  ]

let test_autogen = "Test: generated AST + transformed">:::
  [
    "id1">::(fun _ -> run 8 8);
    "id2">::(fun _ -> run 128 8);
    "add1">::(fun _ -> run_mod 8 8 1);
    "add2">::(fun _ -> run_mod 128 8 3);
  ]

(* Test Runner; ~verbose:true gives info on succ tests *)
let _ =
  run_test_tt_main test_cache;
  run_test_tt_main test_autogen;
  run_test_tt_main test_incr;
