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
        let gamma_init = (M.add_list external_signatures M.empty) in  
          let te = Typing.typecheck gamma_init e in
            let cache = Cache.build_cache te gamma_init in (te, cache)

let analyze_and_report (file : string) (filem : string) =
  let channel, channelm = open_in file, open_in filem in
    let lexbuf = Lexing.from_channel channel in
      let e = Parser.exp Lexer.token lexbuf in
        Id.counter := 0; (* Fix to avoid situations where the same subtree has different hash *)
        let lexbufm = Lexing.from_channel channelm in
          let em = Parser.exp Lexer.token lexbufm in
            let gamma_init = (M.add_list external_signatures M.empty) in
              let te, tem = Typing.typecheck gamma_init e, Typing.typecheck gamma_init em in (* tem computed just to compare the results! *)
                let cache = Cache.build_cache te gamma_init in 
                  Incrementaltyping.IncrementalReport.reset Incrementaltyping.report;
                  Incrementaltyping.IncrementalReport.set_nc (nodecount tem) Incrementaltyping.report;
                  let inctem = Incrementaltyping.incremental_tc gamma_init cache em in (*Analyse the modified program *)
                    Printf.printf "[%s v. %s] - %s\n" file filem (Incrementaltyping.IncrementalReport.string_of_report Incrementaltyping.report);       (Typing.extract_type tem, fst inctem)

let check_cache_result file = let (te, cache) = analyzeExpr file in
        let annot_list = build_annot_list te in
            (* Check the typing. TODO: check the context *)
            assert_bool ("[Cache] Failed: " ^ file) ((List.for_all (fun (hash, tau) -> (snd (Cache.Cache.find hash cache)) = tau) annot_list))

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

(* Test Runner; ~verbose:true gives info on succ tests *)
let _ = 
  (* run_test_tt_main test_cache;  *)
  run_test_tt_main test_incr;

(*

open OUnit
open Awesome

let suite = "OUnit tests..." >:::  ["test_list_length" >::
                                    (fun () -> assert_equal "1" (str_of_t (succ one_t)))]

let _ =
  run_test_tt_main suite
*)
