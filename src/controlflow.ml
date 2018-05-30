open Annotast

type cvar_t = int
type token_t = int

module CVar = struct
  type t = cvar_t
  let compare = Pervasives.compare
end

(* Per il momento, può cambiare *)
module CToken = struct
  type t = String.t
  let compare = String.compare
end

let newVariable : unit -> cvar_t =
  let counter = ref(-1) in
  fun () -> counter := !counter + 1;
    !counter

let extract_cvar (e : ('a * cvar_t) Annotast.t) : cvar_t =
  Annotast.get_annot e |>
  snd


module HashMap = Cache.Cache

let computeVars (e : 'a Annotast.t) : ('a * cvar_t) Annotast.t =
  Annotast.map_annot (fun a -> (a, newVariable())) e


type 'a fundef_data = { fundef : 'a Annotast.fundef ; vars : (Id.t *cvar_t) list }

let computeFuns (e : 'a Annotast.t) : ('a fundef_data HashMap.t)  =
  let rec iter e varMap = match e with
    | Unit(_)
    | Bool(_, _)
    | Int(_, _)
    | Var(_,_)
    | Float(_, _)  ->    varMap

    | Not(e1, _)
    | Neg(e1, _)
    | FNeg(e1, _)   ->   iter e1 varMap
    | IBop(_, e1, e2, _)
    | FBop(_, e1, e2, _)
    | Rel(_, e1, e2, _)
    | Array(e1, e2, _)
    | LetTuple(_, e1, e2, _)
    | Let(_, e1, e2, _)
    | Get(e1, e2, _)   ->   iter e1 varMap |> iter e2

    | If(e1, e2, e3, _)
    | Put(e1, e2, e3, _) -> iter e1 varMap |>
                            iter e2 |>
                            iter e3
    | App(e1, es, _) ->     List.fold_left (fun varMap e -> iter e varMap) (iter e1 varMap) es

    | Tuple(es, _)   -> List.fold_left (fun varMap e -> iter e varMap) varMap es
    | LetRec({name=(f,_) ; args=formals ; body = e1} as fdef, e2, _) ->
      let vs = (f, newVariable ()) :: (List.map (fun (v,_) -> (v,newVariable())) formals) in
      let payload = {fundef=fdef ; vars=vs } in
      HashMap.add (Hashing.compute_hash fdef) payload varMap |>
      iter e1 |>
      iter e2
  in
  iter e HashMap.empty


module Solver = CubicSolver.Make(CVar)(CToken)
module Constraint = Solver.Constraint

let generateConstraint e extern (varMap : 'a fundef_data HashMap.t) =
  let open Solver.Constraint in
  let rec iter e env clist = match e with
    | Unit(_)
    | Bool(_, _)
    | Int(_, _)
    | Float(_, _)  ->   clist
    | Var(x, (_,var)) -> ((M.find x env) @< var) :: clist


    | Not(e1, _)
    | Neg(e1, _)
    | FNeg(e1, _)   ->  iter e1 env clist

    | IBop(_, e1, e2, _)
    | FBop(_, e1, e2, _)
    | Array(e1, e2, _)
    | Rel(_, e1, e2, _)  -> iter e1 env clist |> iter e2 env
    | Get(e1, e2, (_, var))   -> (extract_cvar e1 @< var) :: clist |>
                                 iter e1 env |>
                                 iter e2 env
    | If(e1, e2, e3, (_,var)) -> (extract_cvar e2 @< var) :: (extract_cvar e3 @< var) :: clist |>
                                 iter e1 env |>
                                 iter e2 env |>
                                 iter e3 env
    | Let(id, e1, e2, (_,var)) ->
      let rho_id = newVariable() in
      (extract_cvar e1 @< rho_id) :: (extract_cvar e2 @< var) :: clist |>
      iter e1 env |>
      iter e2 (M.add id rho_id env)
    | LetTuple(ids, e1, e2, (_,var)) ->
      let rho_ids = List.map (fun _ -> newVariable ()) ids in
      let id_con  = List.map (fun v -> extract_cvar e1 @< v) rho_ids in
      (extract_cvar e2 @< var) :: id_con @ clist |>
      iter e1 env |>
      iter e2 (M.add_list2 ids rho_ids env)
    | Put(e1, e2, e3, _) ->  iter e1 env clist |> (* Da capire se va bene *)
                             iter e2 env |>
                             iter e3 env
    | Tuple(es, _)   ->  List.fold_left (fun a e -> iter e env a) clist es
    | LetRec({name=_ ; args=_ ; body = e1} as fdef, e2, (_,var)) ->
      let {fundef=_;vars=binds} = HashMap.find (Hashing.compute_hash fdef) varMap in
      let (f, rho_f) = List.hd binds in
      let nenv = M.add f rho_f env in
      (f @^ rho_f) :: (extract_cvar e2 @< var) :: clist |>
      iter e1 (M.add_list binds nenv) |>
      iter e2 nenv
    | App(e1, es, (_, var)) ->
      let createFormalsConstr es vs =
        List.combine es vs |>
        List.map (fun (e,v) -> (extract_cvar e) @< v)
      in
      let numArgs = List.length es + 1 in
      let flist = HashMap.bindings varMap |>
                  List.map snd in
      let possibleFdef = List.filter (fun fdef -> (List.length fdef.vars) = numArgs) flist in
      let constrs = List.map (fun {fundef=fdef; vars=vs} ->
          let tokenConstr =  (fst fdef.name) @^ (extract_cvar e1) in
          let formalConstr = vs |>
                             List.map snd |>
                             List.tl |>
                             createFormalsConstr es in
          (tokenConstr @~~> (extract_cvar fdef.body @< var)) ::
          (List.map (fun rhs -> tokenConstr @~~> rhs) formalConstr)
        ) possibleFdef
      in
      let clist1 = (List.concat constrs |>  iter e1 env) @ clist in
      List.fold_left (fun clist e -> iter e env clist) clist1 es
  in
  iter e extern []

let generateExternalConstraint external_signatures =
  let open Solver.Constraint in
  let (bindings, constr) = external_signatures |>
             List.map (fun (name, token) ->
                 let variable = newVariable () in
                 ((name, variable), token @^ variable)) |>
             List.split
  in
  (M.add_list bindings M.empty, constr)
