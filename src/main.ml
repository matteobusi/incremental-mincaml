open Annotast
open Typing
open Cache.Cache

(*
   Prove di annotazioni
 *)

(* type hash_annot = H of int
 * let string_of_hash (H(n)) = string_of_int n
 * let string_of_hashtype (H(n),t) = Printf.sprintf "%d - %s" n (Type.string_of_type t) *)

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
  let lexbuf, lexbufm = Lexing.from_channel channel, Lexing.from_channel channelm in
  let e, em = Parser.exp Lexer.token lexbuf, Parser.exp Lexer.token lexbufm in
  try
    let gamma_init = (M.add_list external_signatures M.empty) in
      let te, tem = Typing.g gamma_init e, Typing.g gamma_init em in (* tem computed just to compare the results! *)
        let cache = Cache.buildCache te gamma_init in 
          let inctem = Incrementaltc.incremental_tc gamma_init cache em in (*Analyse the modified program *)
        print_string (Annotast.string_of_annotast string_of_type te);
        print_newline ();
        print_string (Annotast.string_of_annotast string_of_type tem);
        print_string "\n========= Cache =========\n";
        Cache.print_cache cache;
        print_string "=========================\n\n";
        Printf.printf "Type: %s - IType: %s - %s\n" (Type.string_of_type (Typing.extract_type tem)) (Type.string_of_type (fst inctem)) (Incrementaltc.IncrementalReport.string_of_report Incrementaltc.report)
        
  with Typing.TypeError _ -> print_string "Type error\n"
        (* print_string (Annotast.string_of_annotast (fun _ _ -> ()) e);
        print_newline () *)


let _ =  
(* Typing.extenv := M.add_list external_signatures M.empty; *)
  if Array.length Sys.argv = 3 then
    analyzeExpr Sys.argv.(1) Sys.argv.(2)
    (* Array.iteri (fun i exp -> if i > 0 then analyzeExpr exp else ()) Sys.argv *)
  else
    Printf.eprintf "Usage:\n %s file1.ml file2.ml\n" Sys.argv.(0)
