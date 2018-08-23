open Annotast

(* Associates each node hash to its corresponding environment and type *)
module Cache = 
  struct 
    include Map.Make(struct
        type t = int
        let compare = compare
      end)

    let c_union c1 c2 = 
      let hash_collision k v1 v2 = Some v1 in
      union hash_collision c1 c2

    let extract_cache h cache = find_opt h cache
    let update_cache e gamma tau cache = add (Hashing.extract_simple_hash e) (M.restrict gamma e, tau)
  end

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
  Cache.iter print_pairs c

(* 
  Function to build the cache given an aAST and a typing environment.
*)
let rec build_cache e gammaf =
  let gamma = M.restrict gammaf e in 
  match e with
  | Unit((hash, tau)) -> Cache.add hash (gamma, tau) Cache.empty
  | Bool(_, (hash, tau))
  | Int(_,  (hash, tau))
  | Float(_, (hash, tau)) -> Cache.add hash (gamma, tau) Cache.empty
  | Var(x, (hash, tau)) when M.mem x gammaf ->
    let t = M.find x gammaf in
    Cache.add hash (gamma, t) Cache.empty
  | Var(x, _) -> failwith ("Unbound variable: " ^ x)
  | Not(e1, (hash, tau))
  | Neg(e1, (hash, tau)) 
  | FNeg(e1, (hash, tau)) -> Cache.add hash (gamma, tau) (build_cache e1 gammaf) 
  | IBop(_, e1 , e2, (hash, tau))
  | FBop(_, e1, e2, (hash, tau))
  | Rel(_, e1, e2, (hash, tau)) -> Cache.add hash (gamma, tau) (Cache.c_union (build_cache e2 gammaf) (build_cache e1 gammaf)) 
  | If (e1, e2, e3, (hash, tau)) -> 
    let c1 = build_cache e1 gammaf in
    let c2 = build_cache e2 gammaf in 
    let c3 = build_cache e3 gammaf in 
    Cache.add hash (gamma, tau) (
      Cache.c_union c1 (
        Cache.c_union c2 c3))
  | Let (x, e1, e2, (hash, tau)) -> 
    let c1 = build_cache e1 gammaf in
    let taux = Typing.extract_type e1 in
    let gammax = M.add x taux gammaf in
    let c2 = build_cache e2 gammax in
    let hashx = Hashing.compute_hash x in
    Cache.add hash (gamma, tau) (
      Cache.add hashx (gamma, taux) (
        Cache.c_union c1 c2))
  | LetRec ({ name = (f, t); args = yts; body = e1 }, e2, (hash, tau)) ->
    let gamman = M.add_list ((f, Type.Fun(List.map snd yts, t))::yts) gammaf in
    let cache_vars = List.fold_left 
        (fun c (h, g, t) -> Cache.add h (g, t) c)
        Cache.empty 
        (
          List.map 
            (fun (v, t) -> (Hashing.compute_hash v, (M.add v t M.empty), t)) 
            ((f, Type.Fun(List.map snd yts, t))::yts)
        ) in
    Cache.add 
      hash 
      (gamma, tau) 
      (Cache.c_union
         cache_vars 
         (Cache.c_union 
            (build_cache e1 gamman) 
            (build_cache e2 gamman)
         )
      )
  | App (e1, e2, (hash, tau)) -> 
    Cache.add hash 
      (gamma, tau)
      (
        Cache.c_union 
          (build_cache e1 gammaf) 
          (
            List.fold_left 
              (fun fvs el -> Cache.c_union el fvs) 
              Cache.empty 
              (
                List.map 
                  (fun ei -> build_cache ei gammaf) 
                  e2
              )
          )
      )
  | Tuple(es, (hash, tau)) -> List.fold_left (fun fvs el -> Cache.c_union el fvs) Cache.empty (List.map (fun ei -> build_cache ei gammaf) es)
  | LetTuple(t, e1, e2, (hash, tau)) -> 
    let (Type.Tuple(e1s)) = Typing.extract_type e1 in
    let gamma_t = M.add_list2 t e1s gammaf in
    let cache_t = List.fold_left
        (fun c xi -> Cache.add (Hashing.compute_hash xi) (M.add xi (M.find xi gamma_t) M.empty, (M.find xi gamma_t)) c) (* TODO: restrict env over gamma_t *) 
        (build_cache e1 gammaf)
        t in
    Cache.add hash (gamma, tau) (
      Cache.c_union
        cache_t
        (build_cache e2 gamma_t)
    )

  | Array (e1 (* dim *), e2 (* init *) , (hash, tau)) 
  | Get (e1 (* array *) , e2 (* idx *), (hash, tau)) -> Cache.add hash (gamma, tau) (Cache.c_union (build_cache e1 gammaf) (build_cache e2 gammaf))
  | Put (e1 (* array *) , e2 (* idx *) , e3 (* ass *), (hash, tau)) -> Cache.add hash (gamma, tau) (Cache.c_union (build_cache e1 gammaf) (Cache.c_union (build_cache e2 gammaf) (build_cache e3 gammaf)))
