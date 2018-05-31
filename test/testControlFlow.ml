open Annotast
open Controlflow

let string_of_hash ppf h = Format.fprintf ppf "@[%d@]" h
let string_of_annot ppf (_,h) = Format.fprintf ppf "@[%d@]" h


let getExpr (file : string) =
  let channel = open_in file in
  let lexbuf = Lexing.from_channel channel in
  Parser.exp Lexer.token lexbuf


let external_signatures = [
    "print_int" ,      "print_int_fun";
    "print_newline" ,  "print_newline_fun";
    "int_of_float",    "int_of_float_fun";
    "float_of_int",    "float_of_int_fun";
    "sin",             "sin_fun";
    "cos",             "cos_fun";
    "sqrt",            "sqrt_fun";
    "abs_float",       "abs_float_fun";
    "truncate",        "truncate_fun";
  ]


let print_fundef ({fundef=fdef; vars=vs}) =
  Printf.printf "fun %s = " (fdef.name |> fst);
  List.iter (fun (id,v) -> Printf.printf "%s : %d " id v) vs;
  Printf.printf "\n"

let print_constr c =
  let open Solver.Constraint in
  match c with
  | IsIn(t,v) -> Printf.printf "%s is %d\n" t v
  | SubSeteq(v1,v2) -> Printf.printf "%d < %d\n" v1 v2
  | Impl(t, v0, v1,v2) -> Printf.printf "%s is %d ==> %d < %d\n" t v0 v1 v2

let () =
  let e = getExpr Sys.argv.(1) in
  Printf.printf "%s\n\n" (Annotast.string_of_annotast string_of_hash e);
  let eWithVars = computeVars e in
  Printf.printf "\nAnnotated AST with variables\n";
  Printf.printf "%s\n\n" (Annotast.string_of_annotast string_of_annot eWithVars);
  let funsAndVar = computeFuns eWithVars in
  Printf.printf "\nFunction declaration\n";
  HashMap.bindings funsAndVar |>
  List.iter (fun (_,f) -> print_fundef f);
  Printf.printf "\n";
  let freeVars = Annotast.free_variables eWithVars in
  let usedExternals = List.filter (fun (name, _) -> List.exists (fun l -> (String.compare name l) = 0) freeVars) external_signatures in
  Printf.printf "Used free variables: ";
  List.iter (fun (v,_) -> Printf.printf "%s " v) usedExternals;
  Printf.printf "\n";
  let (externBindings, externConstraint) = generateExternalConstraint usedExternals in
  let constraints = generateConstraint eWithVars externBindings funsAndVar in
  Printf.printf "Constraints\n";
  List.iter print_constr constraints;
  let sol = Solver.(mkSolver() |>
                    addAll constraints |>
                    addAll externConstraint |>
                    getSolution |>
                    VarMap.bindings
                   ) in
  Printf.printf "Solution\n";
  List.iter (fun (a,b) ->
      Printf.printf "%d -> ["  a;
      Solver.TokSet.iter (fun s -> Printf.printf "%s " s) b;
      Printf.printf "] ";) sol;
  print_newline ()
