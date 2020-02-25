open OUnit2

open Annotast
open M
open Incremental
open Typing
open Cache
open VarSet
open Generator

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

let analyzeExpr (file : string) =
  let channel = open_in file in
  let lexbuf = Lexing.from_channel channel in
  let e = Parser.exp Lexer.token lexbuf in
  let gamma_init = (M.add_list external_signatures (M.empty ()) ) in
  let te = Typing.typecheck gamma_init e in
  let cache = Cache.create_empty 100 in
  Cache.build_cache te gamma_init cache;
  (te, cache)

let analyze_and_report (file : string) (filem : string) =
  let channel, channelm = open_in file, open_in filem in
  let lexbuf = Lexing.from_channel channel in
  let e = Parser.exp Lexer.token lexbuf in
  Id.counter := 0; (* Fix to avoid situations where the same subtree has different hash *)
  let lexbufm = Lexing.from_channel channelm in
  let em = Parser.exp Lexer.token lexbufm in
  let gamma_init = (M.add_list external_signatures (M.empty ())) in
  let te, tem = Typing.typecheck gamma_init e, Typing.typecheck gamma_init em in (* tem computed just to compare the results! *)
  let cache = Cache.create_empty 100 in
  Cache.build_cache te gamma_init cache;
  IncrementalReport.reset IncrementalTyping.report;
  IncrementalReport.set_nc (nodecount tem) IncrementalTyping.report;
  let inctem = IncrementalTyping.typecheck cache gamma_init em in (*Analyse the modified program *)
  Printf.printf "[%s v. %s] - %s\n" file filem (IncrementalReport.string_of_report IncrementalTyping.report);
  (Typing.extract_type tem, inctem)

let check_cache_result file = let (te, cache) = analyzeExpr file in
  let annot_list = build_annot_list te in
  assert_bool ("[Cache] Failed: " ^ file) ((List.for_all (fun ((hash, fv), tau) -> (snd (Cache.find cache hash)) = tau) annot_list))

let run fv_c depth =
  (* Fill up the initial gamma with needed identifiers *)
  let e = Generator.gen_ibop_ids_ast depth "+" fv_c in
  let initial_gamma_list e = (List.map (fun id -> (id, Type.Int)) (VarSet.elements (Annotast.get_fv e))) in
  let gamma_init = (M.add_list (initial_gamma_list e) (M.empty ()) ) in
  (* These are just to avoid multiple recomputations *)
  let typed_e = Typing.typecheck gamma_init e in
  let init_sz = M.cardinal   gamma_init in
  let full_cache = Cache.copy (Cache.create_empty init_sz) in
  Cache.build_cache typed_e gamma_init full_cache;
  IncrementalReport.reset IncrementalTyping.report;
  IncrementalReport.set_nc (nodecount e) IncrementalTyping.report;
  ignore (IncrementalTyping.typecheck full_cache gamma_init e); (*Analyse the modified program *)
  Printf.printf "transf=id; fv_c=%d; depth=%d - %s\n\n" fv_c depth (IncrementalReport.string_of_report IncrementalTyping.report)

let run_mod fv_c depth inv_depth =
  (* Fill up the initial gamma with needed identifiers *)
  let e = Generator.gen_ibop_ids_ast depth "+" fv_c in
  let initial_gamma_list e = (List.map (fun id -> (id, Type.Int)) (VarSet.elements (Annotast.get_fv e))) in
  let gamma_init = (M.add_list (initial_gamma_list e) (M.empty ()) ) in
  (* These are just to avoid multiple recomputations *)
  let typed_e = Typing.typecheck gamma_init e in
  let init_sz = M.cardinal   gamma_init in
  let full_cache = Cache.create_empty init_sz in
  (* Build the full cache for e *)
  Cache.build_cache typed_e gamma_init full_cache;
  (* Invalidate part of the cache, corresponding to the rightmost subtree of depth tree_depth - d; This simulates addition of code. *)
  Generator.simulate_modification full_cache e inv_depth;
  IncrementalReport.reset IncrementalTyping.report;
  IncrementalReport.set_nc (nodecount e) IncrementalTyping.report;
  ignore (IncrementalTyping.typecheck full_cache gamma_init e); (*Analyse the modified program *)
  Printf.printf "transf=mod; fv_c=%d; depth=%d; inv_depth=%d - %s\n\n" fv_c depth inv_depth (IncrementalReport.string_of_report IncrementalTyping.report)

let check_incremental_result fileo filem = let res = analyze_and_report fileo filem in assert_equal (fst res) (snd res)

(* Test Fixture *)
let test_cache  = "Test: Cache">:::
  [
    "fact.ml">::(fun _ -> check_cache_result "examples/fact.ml" );
    "sum-orig.ml">::(fun _ -> check_cache_result "examples/sum-orig.ml" );
    "sum-tail.ml">::(fun _ -> check_cache_result "examples/sum-tail.ml" );
    "ack.ml">::(fun _ -> check_cache_result "examples/ack.ml" );
  ]

let test_incr = "Test: Incremental TC">:::
  [
    "fact.ml - fact.ml">::(fun _ -> check_incremental_result "examples/fact.ml" "examples/fact.ml" );
    "fact.ml - fact_opt.ml">::(fun _ -> check_incremental_result "examples/fact.ml" "examples/fact_opt.ml" );
    "sum-orig.ml - sum-tail.ml">::(fun _ -> check_incremental_result "examples/sum-orig.ml" "examples/sum-tail.ml" );
    "inprod.ml - inprod-loop.ml">::(fun _ -> check_incremental_result "examples/inprod.ml" "examples/inprod-loop.ml");
    "fact.ml - ack.ml">::(fun _ -> check_incremental_result "examples/fact.ml" "examples/ack.ml" );
    "sum-orig.ml - sum-opti.ml">::(fun _ -> check_incremental_result "examples/sum-orig.ml" "examples/sum-opti.ml" );
    "array-avg.ml - array-avg-codemotion.ml">::(fun _ -> check_incremental_result "examples/array-avg.ml" "examples/array-avg-codemotion.ml" );
    "array-avg.ml - array-avg-if.ml">::(fun _ -> check_incremental_result "examples/array-avg.ml" "examples/array-avg-if.ml" );
    "array-avg.ml - array-avg-ii.ml">::(fun _ -> check_incremental_result "examples/array-avg.ml" "examples/array-avg-ii.ml" );
  ]

let test_autogen = "Test: generated AST + transformed">:::
  [
    "id1">::(fun _ -> run 8 8);
    "id2">::(fun _ -> run 128 8);
    "add1">::(fun _ -> run_mod 8 8 1);
    "add2">::(fun _ -> run_mod 128 8 3);
    (* "elim1">::(fun _ -> run_imm 8 8 7);
    "elim2">::(fun _ -> run_imm 128 8 3); *)
  ]

(* Test Runner; ~verbose:true gives info on succ tests *)
let _ =
  run_test_tt_main test_cache;
  run_test_tt_main test_autogen;
  run_test_tt_main test_incr;

(*

open OUnit
open Awesome

let suite = "OUnit tests..." >:::  ["test_list_length" >::
                                    (fun () -> assert_equal "1" (str_of_t (succ one_t)))]

let _ =
  run_test_tt_main suite
*)
