open Batteries

open Annotast
open M
open Typing

(* Associates each node hash to its corresponding environment and type *)
module Cache = 
  struct
  include Map.Make(struct
      type t = int
      let compare = compare
    end)

  let extract_cache h cache = (let find_opt h cache = (try Some(Map.find h cache) with Not_found -> None) in find_opt h cache)
  let update_cache e gamma tau cache = Map.add (Hashing.extract_simple_hash e) (M.restrict gamma e, tau)

  let add = Map.add
  let union = Map.union
  let find = Map.find
  (* DEBUGGING UTILITY: print the cache on the stdout; TODO: reimplement using ppf *)
  let rec print_cache c = 
    let rec print_gamma gamma = Printf.printf "[";
      M.iter (fun id t -> Printf.printf "%s |> %s, " id (Type.string_of_type t)) gamma;
      Printf.printf "]"
    and print_pairs hash p = match p with
      | (g, t) -> Printf.printf "%d - " hash;
        print_gamma g;
        Printf.printf " - %s \n" (Type.string_of_type t)
    in
    Map.iter print_pairs c

  (* 
    Function to build the cache given an aAST and a typing environment.
  *)
  let rec build_cache e env =
    let gamma = M.restrict env e in 
    match e with
    | Unit((hash, tau)) -> Map.add hash (gamma, tau) Map.empty
    | Bool(_, (hash, tau))
    | Int(_,  (hash, tau))
    | Float(_, (hash, tau)) -> Map.add hash (gamma, tau) Map.empty
    | Var(x, (hash, tau)) when M.mem x env ->
      let t = M.find x env in
      Map.add hash (gamma, t) Map.empty
    | Var(x, _) -> failwith ("Unbound variable: " ^ x)
    | Not(e1, (hash, tau))
    | Neg(e1, (hash, tau)) 
    | FNeg(e1, (hash, tau)) -> Map.add hash (gamma, tau) (build_cache e1 env) 
    | IBop(_, e1 , e2, (hash, tau))
    | FBop(_, e1, e2, (hash, tau))
    | Rel(_, e1, e2, (hash, tau)) -> Map.add hash (gamma, tau) (Map.union (build_cache e2 env) (build_cache e1 env)) 
    | If (e1, e2, e3, (hash, tau)) -> 
      let c1 = build_cache e1 env in
      let c2 = build_cache e2 env in 
      let c3 = build_cache e3 env in 
      Map.add hash (gamma, tau) (
        Map.union c1 (
          Map.union c2 c3))
    | Let (x, e1, e2, (hash, tau)) -> 
      let c1 = build_cache e1 env in
      let taux = Typing.extract_type e1 in
      let gammax = M.add x taux env in
      let c2 = build_cache e2 gammax in
      let hashx = Hashing.compute_hash x in
      Map.add hash (gamma, tau) (
        Map.add hashx (gamma, taux) (
          Map.union c1 c2))
    | LetRec ({ name = (f, t); args = yts; body = e1 }, e2, (hash, tau)) ->
      let gamman = M.add_list ((f, Type.Fun(List.map snd yts, t))::yts) env in
      let cache_vars = List.fold_left 
          (fun c (h, g, t) -> Map.add h (g, t) c)
          Map.empty 
          (
            List.map 
              (fun (v, t) -> (Hashing.compute_hash v, (M.add v t M.empty), t)) 
              ((f, Type.Fun(List.map snd yts, t))::yts)
          ) in
      Map.add 
        hash 
        (gamma, tau) 
        (Map.union
          cache_vars 
          (Map.union 
              (build_cache e1 gamman) 
              (build_cache e2 gamman)
          )
        )
    | App (e1, e2, (hash, tau)) -> 
      Map.add hash 
        (gamma, tau)
        (
          Map.union 
            (build_cache e1 env) 
            (
              List.fold_left 
                (fun fvs el -> Map.union el fvs) 
                Map.empty 
                (
                  List.map 
                    (fun ei -> build_cache ei env) 
                    e2
                )
            )
        )
    | Tuple(es, (hash, tau)) -> List.fold_left (fun fvs el -> Map.union el fvs) Map.empty (List.map (fun ei -> build_cache ei env) es)
    | LetTuple(t, e1, e2, (hash, tau)) -> 
      let (Type.Tuple(e1s)) = Typing.extract_type e1 in
      let gamma_t = M.add_list2 t e1s env in
      let cache_t = List.fold_left
          (fun c xi -> Map.add (Hashing.compute_hash xi) (M.add xi (M.find xi gamma_t) M.empty, (M.find xi gamma_t)) c) (* TODO: restrict env over gamma_t *) 
          (build_cache e1 env)
          t in
      Map.add hash (gamma, tau) (
        Map.union
          cache_t
          (build_cache e2 gamma_t)
      )

    | Array (e1 (* dim *), e2 (* init *) , (hash, tau)) 
    | Get (e1 (* array *) , e2 (* idx *), (hash, tau)) -> Map.add hash (gamma, tau) (Map.union (build_cache e1 env) (build_cache e2 env))
    | Put (e1 (* array *) , e2 (* idx *) , e3 (* ass *), (hash, tau)) -> Map.add hash (gamma, tau) (Map.union (build_cache e1 env) (Map.union (build_cache e2 env) (build_cache e3 env)))
end