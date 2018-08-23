open Annotast
open Typing
open Cache.Cache

(* TODO: HACK: Fill the initial gamma with proper types based on id prefix *)
let initialGammaList e = List.map (
    fun id -> match id.[0] with 
      | 'b' -> (id, Type.Bool)
      | 'n' -> (id, Type.Int)
      | 'f' -> (id, Type.Fun([Type.Int], Type.Int))
      | _ -> failwith "Unexpected variable name!"
  ) 
  (Annotast.free_variables e)


let analyzeExpr (file : string) (filem : string) =
  Printf.printf "Analyzing: Orig: %s Mod: %s ...\n" file filem;
  let channel, channelm = open_in file, open_in filem in
  let lexbuf = Lexing.from_channel channel in
  let e = Parser.exp Lexer.token lexbuf in
  Id.counter := 0; (* Fix to avoid situations where the same subtree has different hashes *)
  let lexbufm = Lexing.from_channel channelm in
  let em = Parser.exp Lexer.token lexbufm in
  try
    let gamma_init = (M.add_list (initialGammaList e) M.empty) in (* Just for experimenting, real programs will have empty gamma_init! *)
    let gamma_init_m = (M.add_list (initialGammaList e) M.empty) in
    let te = Typing.typecheck gamma_init e in
    let tem = Typing.typecheck gamma_init em in (* tem computed just to compare the results! *)
    let cache = Cache.build_cache te gamma_init in 
    let inctem = Incrementaltyping.incremental_tc gamma_init_m cache em in (*Analyse the modified program *)
    let nc = (Annotast.node_count em) in
    Incrementaltyping.IncrementalReport.set_nc nc Incrementaltyping.report;
    Printf.printf "Type: %s - IType: %s\n%s\n" (Type.string_of_type (Typing.extract_type tem)) (Type.string_of_type (fst inctem))(Incrementaltyping.IncrementalReport.string_of_report Incrementaltyping.report );            
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
