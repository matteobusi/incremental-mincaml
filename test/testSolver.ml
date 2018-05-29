open OUnit2

module Solver = CubicSolver.Make(String)(struct
    type t = int
    let compare = Pervasives.compare
  end)

module Constraint = Solver.Constraint

let solveConstraints constraints =
  Solver.(
    mkSolver () |>
    addAll constraints |>
    getSolution |>
    VarMap.bindings
  ) |>
  List.iter ( fun (v, s) ->
      Printf.printf "%s ->" v;
      Solver.TokSet.iter (fun n -> Printf.printf " %d" n) s;
      Printf.printf "\n")


let test1 _ =
  Constraint.( [
      1  @^ "X" ;
      "Y" @< "Z";
      2  @^ "Z" ;
      3  @^ "Y" ;
      "X" @< "Y" ;
      5 @^ "Y" ;
      6 @^ "Z";
      (2 @^ "Y") @~~> ("Y" @< "W") ;
      (1 @^ "X") @~~> ("Z" @< "W")
    ]) |>
  solveConstraints

let test2 _ =
  Constraint.( [
      1 @^ "X" ;
      "Y" @< "X" ;
      "X" @< "Y" ;
      23 @^ "Y"
    ]) |>
  solveConstraints

let test_solver = "Solver Test: Simple constraints">:::
   [

     "Test1">:: test1;
     "Test2">:: test2;

  ]


let _ =
  run_test_tt_main test_solver
