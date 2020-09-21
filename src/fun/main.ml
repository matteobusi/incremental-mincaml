open Core
open Graph

open FunSpecification.FunSpecification

module OriginalFunAlgorithm = Original.TypeAlgorithm(FunSpecification.FunSpecification)
module IncrementalFunAlgorithm = Incrementalizer.TypeAlgorithm(FunSpecification.FunSpecification)

(* This is the runtime needed *)
let initial_gamma_list = [
  "print_char" ,     TFun([TInt], TUnit) ;
  "print_int" ,     TFun([TInt], TUnit) ;
  "print_byte" ,    TFun([TInt], TUnit) ; (* *)
  "print_newline",  TFun([TUnit], TUnit) ;
  "read_token" ,    TFun([TBool], TInt) ; (* *)
  "read_int" ,      TFun([TUnit], TInt) ;
  "read_float" ,    TFun([TUnit], TFloat) ;
  "int_of_float",   TFun([TFloat], TInt) ;
  "float_of_int",   TFun([TInt], TFloat) ;
  "char_of_int",   TFun([TInt], TInt) ;
  "sin",            TFun([TFloat], TFloat) ;
  "cos",            TFun([TFloat], TFloat) ;
  "sqrt",           TFun([TFloat], TFloat) ;
  "abs_float",      TFun([TFloat], TFloat) ;
  "truncate",       TFun([TFloat], TInt);
  "floor",          TFun([TFloat], TFloat);
  "atan",           TFun([TFloat], TFloat);
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
  | Let(_, e1, e2, annot) -> 1 + nodecount e1 + nodecount e2 (* curr node + x *)
  | LetRec ({ name = _; args = yts; body = e1 }, e2, annot) -> 1 + nodecount e1 + nodecount e2
  | App (e1, es, annot) -> 1 + (List.fold_left ~f:(+) ~init:0 (List.map ~f:nodecount (e1::es)))
  | Tuple(es, annot) -> 1 + (List.fold_left ~f:(+) ~init:0 (List.map ~f:nodecount es))
  | LetTuple(xs, e1, e2, annot) -> 1 + nodecount e1 + nodecount e2
  | Array(e1, e2, annot)
  | Get (e1, e2, annot) -> 1 + nodecount e1 + nodecount e2
  | Put (e1, e2, e3, annot) -> 1 + nodecount e1 + nodecount e2 + nodecount e3

let rec annotate_fv e =
  match e with
    | Unit(annot) -> Unit((annot, VarSet.empty))
    | Bool(v, annot) -> Bool(v, (annot, VarSet.empty))
    | Int(v, annot) -> Int(v, (annot, VarSet.empty))
    | Float(v, annot) -> Float(v, (annot, VarSet.empty))
    | Var(x, annot) -> Var (x, (annot, VarSet.singleton x))
    | Not(e1, annot) -> let ae1 = annotate_fv e1 in Not (ae1, (annot, snd (term_getannot ae1)))
    | Neg(e1, annot) -> let ae1 = annotate_fv e1 in Neg (ae1, (annot, snd (term_getannot ae1)))
    | FNeg(e1, annot) -> let ae1 = annotate_fv e1 in FNeg (ae1, (annot, snd (term_getannot ae1)))
    | IBop(op, e1, e2, annot) ->
      let ae1, ae2 = annotate_fv e1, annotate_fv e2 in
        IBop (op, ae1, ae2, (annot, VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2))))
    | FBop(op, e1, e2, annot) ->
      let ae1, ae2 = annotate_fv e1, annotate_fv e2 in
        FBop (op, ae1, ae2, (annot, VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2))))
    | Rel(op, e1, e2, annot) ->
      let ae1, ae2 = annotate_fv e1, annotate_fv e2 in
        Rel (op, ae1, ae2, (annot, VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2))))
    | If(e1, e2, e3, annot) ->
      let ae1, ae2, ae3 = annotate_fv e1, annotate_fv e2, annotate_fv e3 in
        If (ae1, ae2, ae3,
          (annot,
            VarSet.union (snd (term_getannot ae3)) (VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2)))))
    | Let(x, e1, e2, annot) ->
      let ae1, ae2 = annotate_fv e1, annotate_fv e2 in
        Let (x, ae1, ae2,
          (annot, VarSet.remove (VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2))) x))
    | LetRec ({ name = (fn, ft); args = yts; body = e1 }, e2, annot) ->
      let ae1, ae2 = annotate_fv e1, annotate_fv e2 in
        let lrfv = List.fold_left (fn::(List.map yts ~f:fst)) ~init:(VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2))) ~f:VarSet.remove in
          LetRec ({ name = (fn, ft); args = yts; body = ae1 }, ae2, (annot, lrfv))
    | App (e1, es, annot) ->
      let ae1, aes = annotate_fv e1, List.map es ~f:annotate_fv in
        App (ae1, aes, (annot, (List.fold_left aes ~init:VarSet.empty ~f:(fun afv ae -> VarSet.union afv (snd (term_getannot ae))))))
    | Tuple(es, annot) ->
      let aes = List.map es ~f:annotate_fv in
        Tuple(aes, (annot, (List.fold_left aes ~init:VarSet.empty ~f:(fun afv ae -> VarSet.union afv (snd (term_getannot ae))))))
    | LetTuple(xs, e1, e2, annot) ->
      let ae1, ae2 = annotate_fv e1, annotate_fv e2 in
        let lrfv = List.fold_left xs ~init:(VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2))) ~f:VarSet.remove in
          LetTuple(xs, ae1, ae2, (annot, lrfv))
    | Array(e1, e2, annot) ->
      let ae1, ae2 = annotate_fv e1, annotate_fv e2 in
        Array(ae1, ae2, (annot, (VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2)))))
    | Get (e1, e2, annot) ->
      let ae1, ae2 = annotate_fv e1, annotate_fv e2 in
        Get (ae1, ae2, (annot, (VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2)))))
    | Put (e1, e2, e3, annot) ->
      let ae1, ae2, ae3 = annotate_fv e1, annotate_fv e2, annotate_fv e3 in
        Put (ae1, ae2, ae3, (annot, (VarSet.union (snd (term_getannot ae3)) (VarSet.union (snd (term_getannot ae1)) (snd (term_getannot ae2))))))

let lbl_of_term e =
  match e with
  | Unit(annot) -> "()"
  | Bool(b, annot) -> Format.sprintf "%b" b
  | Int(n, annot) -> Format.sprintf "%d" n
  | Float(f, annot) -> Format.sprintf "%f" f
  | Not(e, annot) -> "Not"
  | Neg(e, annot) -> "-"
  | IBop(op, e1, e2, annot) -> op (* Format.sprintf "%s" op *)
  | FNeg(e, annot) -> "-."
  | FBop(op, e1, e2, annot) -> op (* Format.sprintf "%s" op *)
  | Rel(op, e1, e2, annot) -> op (* Format.sprintf "%s" op *)
  | If(e1, e2,e3, annot) -> "IfThenElse"
  | Let(id, e1, e2, annot) -> Format.sprintf "Let %s" id
  | Var(id, annot) -> Format.sprintf "%s" id
  | Array(e1, e2, annot) -> "ArrayMake"
  | Get(e1,e2, annot) -> "ArrayGet"
  | Put(e1, e2, e3, annot) -> "ArrayPut"
  | LetRec(f, e, annot) -> Format.sprintf "LetRec %s" (fst f.name)
  | App(e, es, annot) -> "App"
  | Tuple(es, annot) -> "Tuple"
  | LetTuple(bs, e1, e2, annot) -> Format.sprintf "LetTuple%s" (List.fold_left bs ~init:"" ~f:(fun a s -> a ^ " " ^ s))

module Node = struct
   type t = int * (string * ((IncrementalFunAlgorithm.IncrementalReport.node_visit_type ref)))
   let compare = Stdlib.compare
   let hash (h : t) = let (i, (s, _)) = h in [%hash: int * string] (i, s)
   let equal = (=)
end

module G = Graph.Persistent.Digraph.Concrete (Node)

module Dot = Graph.Graphviz.Dot(struct
    include G
    let edge_attributes (a, b) = []
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v =
      (* if List.mem (fst v) (IncrementalFunAlgorithm.IncrementalReport.get_miss_list IncrementalFunAlgorithm.report) then *)
      (match !(snd (snd v)) with
        | IncrementalFunAlgorithm.IncrementalReport.NoVisit -> [`Shape `Box; `Fillcolor 16777215; `Style `Filled]
        | IncrementalFunAlgorithm.IncrementalReport.Orig -> [`Shape `Box; `Fillcolor 65535; `Style `Filled]
        | IncrementalFunAlgorithm.IncrementalReport.Hit -> [`Shape `Box; `Fillcolor 65280; `Style `Filled]
        | IncrementalFunAlgorithm.IncrementalReport.Miss -> [`Shape `Box; `Fillcolor 16711680; `Style `Filled])
    let vertex_name v = fst (snd v)
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let cnt = ref 0

let string_of_node_visit_type t = match t with
  | IncrementalFunAlgorithm.IncrementalReport.NoVisit -> "NoVisit"
  | IncrementalFunAlgorithm.IncrementalReport.Orig -> "Orig"
  | IncrementalFunAlgorithm.IncrementalReport.Hit -> "Hit"
  | IncrementalFunAlgorithm.IncrementalReport.Miss -> "Miss"

let rec mk_graph i e ig =
  let use_cnt () = incr cnt; !cnt in
  let cl = FunSpecification.FunSpecification.get_sorted_children e in
  let ve = (i, (Printf.sprintf "\"(%d) %s\"" (!cnt) (lbl_of_term e), term_getannot e)) in
  let g = G.add_vertex ig ve in
  let g' = List.fold_left
    (List.rev cl) ~init:g ~f:(fun a ec -> (
      let vec =
      (fst ec, (Printf.sprintf "\"(%d) %s\"" (use_cnt ()) (lbl_of_term (snd ec)), term_getannot (snd ec))) in
        G.add_edge (mk_graph (fst ec) (snd ec) a) ve vec
    ))
    in g'

let analyze_expr (file : string) (filem : string) =
    Printf.printf "Analyzing: Orig: %s ... Mod: %s ...\n" file filem; flush stdout;
    let channel, channelm = open_in file, open_in filem in
    Printf.printf "Lexing "; flush stdout;
    let lexbuf, lexbufm = Lexing.from_channel channel, Lexing.from_channel channelm in
    Printf.printf "... done\n"; flush stdout;
    Printf.printf "Parsing "; flush stdout;
    let e, em = (Parser.exp Lexer.token lexbuf), (Parser.exp Lexer.token lexbufm) in
    Printf.printf "... done\n"; flush stdout;
    Printf.printf "Annotating original "; flush stdout;
    (* let e_hf = (Id.counter := 0; OriginalFunAlgorithm.term_map (fun e -> (compute_hash e, compute_fv e)) e) in *)
    let e_hf = (annotate_fv (OriginalFunAlgorithm.term_map (fun e -> compute_hash e) e)) in
    Printf.printf "... - done (root hash: %d)\n" (compute_hash e); flush stdout;
    Printf.printf "Annotating modified "; flush stdout;
    let em_hf = (annotate_fv (OriginalFunAlgorithm.term_map (fun e -> compute_hash e) em)) in
    Printf.printf "... - done (root hash: %d)\n" (compute_hash em); flush stdout;
    Printf.printf "Initial typing environments "; flush stdout;
    let gamma_init, gamma_initm = (FunContext.add_list (initial_gamma_list) (FunContext.get_empty_context ())), (FunContext.add_list (initial_gamma_list) (FunContext.get_empty_context ())) in
    Printf.printf "... done\n"; flush stdout;
    (* Printf.printf "Program: %s\n" (FunSpecification.FunSpecification.string_of_term (fun f x -> ()) e);
    Printf.printf "Program Mod: %s\n" (FunSpecification.FunSpecification.string_of_term (fun f x -> ()) em); *)
    Printf.printf "Building the cache "; flush stdout;
    let cache = IncrementalFunAlgorithm.get_empty_cache () in
    ignore (IncrementalFunAlgorithm.build_cache e_hf gamma_init cache);
    Printf.printf "... done\n"; flush stdout;
    IncrementalFunAlgorithm.IncrementalReport.reset IncrementalFunAlgorithm.report;
    IncrementalFunAlgorithm.IncrementalReport.set_nc (nodecount em_hf) IncrementalFunAlgorithm.report;
    Printf.printf "Original typing "; flush stdout;
    let te = OriginalFunAlgorithm.typing gamma_init e_hf in
    Printf.printf "... done\n"; flush stdout;
    Printf.printf "Incremental typing "; flush stdout;
    let tem = IncrementalFunAlgorithm.typing_w_report cache gamma_initm em_hf in
      Printf.printf "... done\n"; flush stdout;
      Printf.printf "Type: %s - IType: %s\n" (FunSpecification.FunSpecification.string_of_type te) (FunSpecification.FunSpecification.string_of_type tem);
      Printf.printf "%s\n" (IncrementalFunAlgorithm.IncrementalReport.string_of_report IncrementalFunAlgorithm.report);
      (match IncrementalFunAlgorithm.report.annot_t with
        | Some at ->
          let file = Stdlib.open_out_bin "incremental_visual.dot" in
            Printf.printf "Printing tree report ...";
            Dot.output_graph file (mk_graph 0 at G.empty);
            Printf.printf "done\n"
        | _ -> ())

let _ =
    if Array.length Sys.argv = 3 then
        begin
            analyze_expr Sys.argv.(1) Sys.argv.(2);
            exit 0
        end
    else
        Printf.printf "Usage:\n %s file1.ml file2.ml\n" Sys.argv.(0)
