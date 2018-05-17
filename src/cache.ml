open Annotast

(* Associates our hashes to the corresponding env/type *)
module Cache = Map.Make(struct
    type t = int
    let compare = compare
  end)

module VarSet = Set.Make(struct
    type t = Id.t
    let compare = compare
  end)

let hash_collision k v1 v2 = Some v1

let rec free_variables e = match e with
  | Unit(_)
  | Bool(_)
  | Int(_)
  | Float(_) -> VarSet.empty
  | Var(x, _) -> VarSet.add x VarSet.empty
  | Not(e1, _)
  | Neg(e1, _) 
  | FNeg(e1, _) -> free_variables e1 
  | IBop(_, e1, e2, _)
  | FBop(_, e1, e2, _)
  | Rel(_, e1, e2, _) -> VarSet.union (free_variables e1) (free_variables e2)
  | If(e1,e2,e3, _) -> VarSet.union (free_variables e1) (VarSet.union (free_variables e2) (free_variables e3))
  | Let(x, e1, e2, _) -> VarSet.remove x (VarSet.union (free_variables e1) (free_variables e2))
  | LetRec ({ name = (x, t); args = yts; body = e1 }, e2, _) ->
    List.fold_left (fun fvs el -> VarSet.remove el fvs) (VarSet.union (free_variables e1) (free_variables e2)) (x::(List.map fst yts))
  | App (e, es, _) -> VarSet.union (free_variables e) (List.fold_left (fun fvs el -> VarSet.union el fvs) VarSet.empty (List.map free_variables es))
  | Tuple(es, _) -> List.fold_left (fun fvs el -> VarSet.union el fvs) VarSet.empty (List.map free_variables es)
  | LetTuple(xs, e1, e2, _) -> List.fold_left (fun fvs el -> VarSet.remove el fvs) (VarSet.union (free_variables e1) (free_variables e2)) xs
  | Array(e1, e2, _) -> VarSet.union (free_variables e1) (free_variables e2)
  | Get (e1, e2, _) -> VarSet.union (free_variables e1) (free_variables e2)
  | Put (e1, e2, e3, _) ->  VarSet.union (free_variables e1) (VarSet.union (free_variables e2) (free_variables e3))

let restrict_env gamma fvs = List.fold_left (fun env el -> M.remove el env) gamma (VarSet.elements fvs) 

(* TODO: restringere le gamma con restring env *)
(* buildCache: Type.t Annotast.t -> M.t -> Cache *)
let rec buildCache e gammao = let gamma = (restrict_env gammao (free_variables e)) in match e with
  | Unit((hash, tau)) -> Cache.add hash (gamma, tau) Cache.empty
  | Bool(_, (hash, tau))
  | Int(_,  (hash, tau))
  | Float(_, (hash, tau))
  | Var(_, (hash, tau)) -> Cache.add hash (gamma, tau) Cache.empty
  | Not(e1, (hash, tau))
  | Neg(e1, (hash, tau)) 
  | FNeg(e1, (hash, tau)) -> Cache.add hash (gamma, tau) (buildCache e1 gamma) 
  | IBop(_, e1 , e2, (hash, tau))
  | FBop(_, e1, e2, (hash, tau))
  | Rel(_, e1, e2, (hash, tau)) -> Cache.add hash (gamma, tau)  (Cache.union hash_collision (buildCache e2 gamma) (buildCache e1 gamma)) 
  | If (e1, e2, e3, (hash, tau)) -> 
    let c1 = buildCache e1 gamma in
    let c2 = buildCache e2 gamma in 
    let c3 = buildCache e3 gamma in 
    Cache.add hash (gamma, tau) (
      Cache.union hash_collision c1 (
        Cache.union hash_collision c2 c3))
  | Let (x, e1, e2, (hash, tau)) -> 
    let c1 = buildCache e1 gamma in
    let tau_x = Typing.extract_type e1 in
    let gammap = M.add x tau_x gamma in
    let c2 = buildCache e2 gammap in
    let hash_x = Hashing.compute_hash x (* TODO : dovrebbe funzionare così, ma dipende come funziona il parser. *) in
    Cache.add hash (gamma, tau) (
      Cache.add hash_x (gammap, tau) (
        Cache.union hash_collision c1 c2))
  | LetRec ({ name = (f, t); args = yts; body = e1 }, e2, (hash, tau)) ->
    let gamman = M.add_list ((f, t)::yts) gamma in
    let cache_vars = List.fold_left (fun c (h, g, t) -> Cache.add h (g, t) c) Cache.empty (List.map (fun (v, t) -> (Hashing.compute_hash v, (M.add v t M.empty), t)) ((f, t)::yts)) in
    Cache.add hash (gamma, tau) (Cache.union hash_collision cache_vars (Cache.union hash_collision (buildCache e1 gamman) (buildCache e2 gamman)))
  | App (e1, e2, (hash, tau)) -> Cache.add hash (gamma, tau) (Cache.union hash_collision (buildCache e1 gamma) (List.fold_left (fun fvs el -> Cache.union       hash_collision el fvs) Cache.empty (List.map (fun ei -> buildCache ei gamma) e2)))
  | Tuple(es, (hash, tau)) -> List.fold_left (fun fvs el -> Cache.union hash_collision el fvs) Cache.empty (List.map (fun ei -> buildCache ei gamma) es)
  (* | LetTuple(t, e1, e2, (hash, tau)) -> let tes1 = g env e1 in
       let (Type.Tuple(ts)) = extract_type tes1 in
       (try
         let  zts = List.combine xs ts in
         let nenv = List.fold_left (fun env (x,t) -> M.add x t env) env zts in
         let tes2 =  g nenv e2 in
         LetTuple(xs, tes1, tes2, (annot, extract_type tes2))
       with Invalid_argument _ ->
             failwith "Different arity in pattern matching for tuple")
     let cache_t = List.fold_left (fun c xi -> Cache.add (Hashing.compute_hash xi) () c) Cache.empty t
     | Array (e1, e2, (hash, tau)) -> 
     | Get (e1, e2, (hash, tau)) ->
     | Put (e1, e2, (hash, tau)) -> *)
  | _ -> failwith "Not implemented yet!"
