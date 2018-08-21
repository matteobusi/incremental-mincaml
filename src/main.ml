open Annotast
open Typing
open Cache.Cache

(*
   Prove di annotazioni
 *)

(* type hash_annot = H of int
 * let string_of_hash (H(n)) = string_of_int n
 * let string_of_hashtype (H(n),t) = Printf.sprintf "%d - %s" n (Type.string_of_type t) *)
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

(*
let string_of_type (_,t) = Printf.sprintf "%s" (Type.string_of_type t)
 *)
let string_of_type ppf (h,t) = Format.fprintf ppf "@[<2>%d - %a@]" h Type.type_ppf t

let analyzeExpr (file : string) (filem : string) =
  Printf.printf "Analyzing: Orig: %s Mod: %s ...\n" file filem;
  let channel, channelm = open_in file, open_in filem in
  let lexbuf = Lexing.from_channel channel in
  let e = Parser.exp Lexer.token lexbuf in
  Id.counter := 0;
  let lexbufm = Lexing.from_channel channelm in
  let em = Parser.exp Lexer.token lexbufm in
  try
    let gamma_init = (M.add_list external_signatures M.empty) in
      let te = Typing.g gamma_init e in
        let tem = Typing.g gamma_init em in (* tem computed just to compare the results! *)
        let cache = Cache.buildCache te gamma_init in 
          let inctem = Incrementaltc.incremental_tc gamma_init cache em in (*Analyse the modified program *)
            let nc = nodecount em in
              Incrementaltc.IncrementalReport.set_nc nc Incrementaltc.report;
              (* print_string (Annotast.string_of_annotast string_of_type te);
              print_newline ();
              print_string (Annotast.string_of_annotast string_of_type tem);
              print_string "\n========= Cache =========\n";
              Cache.print_cache cache;
              print_string "=========================\n\n"; *)
              Printf.printf "Type: %s - IType: %s\n%s\n" (Type.string_of_type (Typing.extract_type tem)) (Type.string_of_type (fst inctem))(Incrementaltc.IncrementalReport.string_of_report Incrementaltc.report );            
  with Typing.TypeError _ -> print_string "Type error\n";
        exit 1
        (* print_string (Annotast.string_of_annotast (fun _ _ -> ()) e);
        print_newline () *)


let _ =  
(* Typing.extenv := M.add_list external_signatures M.empty; *)
  if Array.length Sys.argv = 3 then
    begin
    analyzeExpr Sys.argv.(1) Sys.argv.(2); 
    exit 0
    (* Array.iteri (fun i exp -> if i > 0 then analyzeExpr exp else ()) Sys.argv *)
    end
  else
    Printf.eprintf "Usage:\n %s file1.ml file2.ml\n" Sys.argv.(0)
