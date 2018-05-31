module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module Make(Var : OrderedType)(Token : OrderedType) =
struct

  module Constraint = struct
    type t =
      | IsIn of Token.t * Var.t
      | SubSeteq of Var.t * Var.t
      | Impl of Token.t * Var.t * Var.t * Var.t


    let (@^) token var =
      IsIn(token, var)

    let (@<) var1 var2 =
      SubSeteq(var1, var2)

    let (@~~>) hyp concl = match (hyp, concl) with
      |(IsIn(t,v1), SubSeteq(v2,v3)) -> Impl(t,v1,v2,v3)
      | _ -> failwith "Malformed constraint"

  end

  module OrderedPair(Ord : OrderedType) =
  struct
    type t = (Ord.t * Ord.t)
    let compare (x0,x1) (y0,y1) =
      match Ord.compare x0 y0 with
      | 0 -> Ord.compare x1 y1
      | n -> n
  end

  module VarSet = Set.Make(Var)
  module VarMap = Map.Make(Var)
  module PairVarSet = Set.Make(OrderedPair(Var))

  module TokSet = Set.Make(Token)
  module TokMap = Map.Make(Token)

  type node = {
    successors      : VarSet.t;
    tokenSol        : TokSet.t;
    vars            : VarSet.t;
    conditionals    : PairVarSet.t TokMap.t
  }

  let mkNode (v : Var.t) =
  {
    successors    = VarSet.empty ;
    tokenSol      = TokSet.empty ;
    vars          = VarSet.singleton v;
    conditionals  = TokMap.empty
  }

  module NodeOrdered = struct
    type t = node
    let compare = Pervasives.compare
  end

  module NodeSet = Set.Make(NodeOrdered)


  type t = { nodes : node VarMap.t }

  let mkSolver () =
    { nodes = VarMap.empty }

  let addNode solver var node =
    { nodes = VarMap.add var node solver.nodes }

  let getOrNewNode (solver : t) (var : Var.t) :  node =
    match (VarMap.find_opt var solver.nodes) with
    | Some(n) ->  n
    | None -> mkNode var

  (* Rewrite very ugly *)
let detectPath fromNode toNode solver =
  let visitedNode = ref NodeSet.empty in
  let rec iter current =
    if (NodeOrdered.compare current toNode) = 0 then
      [current]
    else
      begin
        visitedNode := NodeSet.add current !visitedNode;
        let toReturn = ref [] in
        current.successors |>
        VarSet.elements |>
        List.map (fun n -> VarMap.find n solver.nodes) |>
        List.filter (fun n -> not (NodeSet.mem n !visitedNode)) |>
        List.exists (fun n ->
            let visited = iter n in
            if (visited <> []) then
              (toReturn := current :: visited;
               true)
            else
              false
          ) |>
        ignore;
        !toReturn
      end
  in
  iter fromNode

let collapseCycle cycle solver =
  if cycle <> [] then
    let firstNode = List.hd cycle in
    let otherNodes = List.tl cycle in
    let nSuccessors = List.fold_left (fun s n -> VarSet.union s n.successors)
        firstNode.successors otherNodes in
    let nVars = List.fold_left (fun vars node -> VarSet.union vars node.vars)
        firstNode.vars otherNodes in
    let nTokenSet = List.fold_left (fun tset node -> TokSet.union tset node.tokenSol)
        firstNode.tokenSol otherNodes in
    let nConds = List.fold_left (fun conds node ->
        TokMap.union (fun _ a b -> Some(PairVarSet.union a b)) conds node.conditionals)
        firstNode.conditionals otherNodes in
    let newNode = { successors = nSuccessors;
                    tokenSol = nTokenSet ;
                    vars = nVars;
                    conditionals = nConds }
    in
    let nNodes = VarSet.fold (fun v nMap -> VarMap.add v newNode nMap)
        newNode.vars solver.nodes
    in
    {nodes = nNodes}
  else
    solver

  let rec addAndPropagate solver tokenSet var =
    let  node = getOrNewNode solver var in
    let newTokens = TokSet.union node.tokenSol tokenSet in
    if (TokSet.compare newTokens node.tokenSol) <> 0 then
      let diff = TokSet.diff newTokens node.tokenSol in
      let newNode = {node with tokenSol = newTokens;
                               conditionals = TokSet.fold TokMap.remove diff node.conditionals } in
      let tempSolver = addNode solver var newNode in
      let tempSolver1 = TokSet.fold (fun token solver ->
          match (TokMap.find_opt token newNode.conditionals) with
          | Some(s) -> PairVarSet.fold (fun (v1,v2) solver1 ->
              addSubsetConstraint v1 v2 solver1) s solver
          | None -> solver
        ) diff tempSolver in
      VarSet.fold (fun var solver -> addAndPropagate solver newTokens var) node.successors tempSolver1
    else
      solver

  and addSubsetConstraint v1 v2 solver =
    let node1 = getOrNewNode solver v1 in
    let node2 = getOrNewNode solver v2 in

    if (Pervasives.compare node1 node2) <> 0 then
      let nnode1 = {node1 with successors = VarSet.add v2 node1.successors } in
      let solver1 = addNode solver v1 nnode1 in
      (* Ugly, but it works *)
      let solver11 = addNode solver1 v2 node2 in
      let solver2 = addAndPropagate solver11 nnode1.tokenSol v2 in
      collapseCycle (detectPath node2 nnode1 solver2) solver2
    else
      solver

  and addConstantConstraint token var solver =
    addAndPropagate solver (TokSet.singleton token) var

  and addConditionalConstraint token var1 var2 var3 solver =
    let node = getOrNewNode solver var1 in
    if TokSet.mem token node.tokenSol then
      addSubsetConstraint var2 var3 solver
    else if (Var.compare var2 var3) <> 0 then
      let newConditionals =
        TokMap.union (fun _ a b -> Some(PairVarSet.union a b)) node.conditionals (TokMap.singleton token (PairVarSet.singleton (var2,var3)))
      in
      let newNode = {node with conditionals = newConditionals} in
      addNode solver var1 newNode
    else
      solver

  let addConstraint constr solver =
    let open Constraint in
    match constr with
    | IsIn(token, var) -> addConstantConstraint token var solver
    | SubSeteq(var1, var2) -> addSubsetConstraint var1 var2 solver
    | Impl(token, var1, var2, var3) -> addConditionalConstraint token var1 var2 var3 solver

  let addAll constrs solver =
    List.fold_left (fun s c ->  addConstraint c s) solver constrs

  let getSolution solver =
    VarMap.map (fun n -> n.tokenSol) solver.nodes
end
