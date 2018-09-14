open Annotast
open Typing
open M
open Incremental
open Varset
open Cache

(* let initial_gamma_list e = (List.map (fun id -> (id, Type.Int)) (VarSet.to_list (Annotast.get_fv e))) *)

let initial_gamma_list = [
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

let analyzeExpr (file : string) (filem : string) =
  Printf.printf "Analyzing: Orig: %s Mod: %s ...\n" file filem;
  let channel, channelm = open_in file, open_in filem in
  let lexbuf = Lexing.from_channel channel in
  let e = Parser.exp Lexer.token lexbuf in
  Id.counter := 0; (* Fix to avoid situations where the same subtree has different hashes *)
  let lexbufm = Lexing.from_channel channelm in
  let em = Parser.exp Lexer.token lexbufm in
  try
    let gamma_init = (M.add_list (initial_gamma_list) M.empty) in (* Just for experimenting, real programs will have empty gamma_init! *)
    let gamma_init_m = (M.add_list (initial_gamma_list) M.empty) in
    let te = Typing.typecheck gamma_init e in
    let tem = Typing.typecheck gamma_init em in (* tem computed just to compare the results! *)
    let cache = Cache.create_empty 100 in 
    Cache.build_cache te gamma_init cache;
    let inctem = IncrementalTyping.typecheck cache gamma_init_m em in (*Analyse the modified program *)
    let nc = (Annotast.node_count em) in
    IncrementalReport.set_nc nc IncrementalTyping.report;
    Printf.printf "Type: %s - IType: %s\n%s\n" (Type.string_of_type (Typing.extract_type tem)) (Type.string_of_type inctem)(IncrementalReport.string_of_report IncrementalTyping.report );            
  with Typing.TypeError _ -> print_string "Type error\n";
    exit 1

let _ =  
  if Array.length Sys.argv = 3 then
    begin
      analyzeExpr Sys.argv.(1) Sys.argv.(2); 
      exit 0
    end
  else
    Printf.eprintf "Usage:\n %s file1.ml file2.ml\n" Sys.argv.(0)
     (* Printf.printf "%s" "Gen aAST..."; flush stdout;
  let e = Generator.gen_ibop_ids_ast (int_of_string Sys.argv.(1)) "+" 10 in
  let em = e in  
  Printf.printf "%s" "Initial gamma..."; flush stdout;
  let gamma_init = (M.add_list (initial_gamma_list e) M.empty ) in
  let gamma_init_m = (M.add_list (initial_gamma_list em) M.empty ) in
   Printf.printf "%s" "Initial typing..."; flush stdout;
  let typed_aast = Typing.typecheck gamma_init e in
   Printf.printf "%s" "Building the cache..."; flush stdout; let cache = Cache.create_empty 100 in Cache.build_cache typed_aast gamma_init cache;
    (* ignore (Typing.typecheck gamma_init e); 
    ignore (Typing.typecheck gamma_init_m em); 
    ignore (IncrementalTyping.typecheck cache gamma_init_m em)  *)
    Printf.printf "%s" "Search the cache..."; flush stdout;  (Cache.extract_cache (fst (Annotast.get_annot em)) cache) *)