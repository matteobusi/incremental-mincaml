open Annotast

(* Associates our hashes to the corresponding env/type *)
module Cache = Map.Make(struct
    type t = int
    let compare = compare
  end)

let hash_collision k v1 v2 = Some v1

(* let environment_restrict gamma e = List.fold_left (fun env el -> M.remove el env) gamma (bound_variables e) *)

let rec string_of_type t = match t with
| Type.Unit -> "unit"
| Type.Bool -> "bool"
| Type.Int -> "int"
| Type.Float -> "float"
| Type.Fun(ts, r) -> (String.concat "*" (List.map string_of_type ts)) ^ " -> " ^ (string_of_type r) 
| Type.Tuple(ts) -> "{" ^ (String.concat ", " (List.map string_of_type ts)) ^ "}"
| Type.Array(r) -> (string_of_type r) ^ "[]" 

let rec print_list l = match l with
| [] -> Printf.printf "[]"
| x::xs -> print_string x; Printf.printf "::"; print_list xs

let print_gamma gamma = Printf.printf "[";
                        M.iter (fun id t -> Printf.printf "%s |> %s, " id (string_of_type t)) gamma;
                        Printf.printf "]"

(* TODO: da rifare, stampa al volo *)
let print_pairs hash p = match p with
| (g, t) -> Printf.printf "%d - " hash;
            print_gamma g;
            Printf.printf " - %s \n" (string_of_type t)

let rec print_cache c = Cache.iter print_pairs c

(* buildCache: Type.t Annotast.t -> M.t -> Cache *)
let rec buildCache e gammaf =
  let gamma = M.environment_restrict gammaf e in 
  (* print_gamma gammaf; Printf.printf " - "; print_gamma gamma; Printf.printf " - "; print_list (free_variables e); Printf.printf "\n"; *)
    match e with
  | Unit((hash, tau)) -> Cache.add hash (gamma, tau) Cache.empty
  | Bool(_, (hash, tau))
  | Int(_,  (hash, tau))
  | Float(_, (hash, tau)) -> Cache.add hash (gamma, tau) Cache.empty
  | Var(x, (hash, tau)) when M.mem x gammaf ->
      let t = M.find x gammaf in
      Cache.add hash (gamma, t) Cache.empty
  (* | Var(x, (hash, tau)) when M.mem x !extgamma ->
      let t = M.find x !extgamma in
      Cache.add hash (environment_restrict (M.union (fun k v1 v2 -> Some v1) !extgamma gamma) e, t) Cache.empty *)
  | Var(x, _) -> failwith ("Unbound variable: " ^ x)
  | Not(e1, (hash, tau))
  | Neg(e1, (hash, tau)) 
  | FNeg(e1, (hash, tau)) -> Cache.add hash (gamma, tau) (buildCache e1 gammaf) 
  | IBop(_, e1 , e2, (hash, tau))
  | FBop(_, e1, e2, (hash, tau))
  | Rel(_, e1, e2, (hash, tau)) -> Cache.add hash (gamma, tau) (Cache.union hash_collision (buildCache e2 gammaf) (buildCache e1 gammaf)) 
  | If (e1, e2, e3, (hash, tau)) -> 
    let c1 = buildCache e1 gammaf in
      let c2 = buildCache e2 gammaf in 
        let c3 = buildCache e3 gammaf in 
          Cache.add hash (gamma, tau) (
            Cache.union hash_collision c1 (
              Cache.union hash_collision c2 c3))
  | Let (x, e1, e2, (hash, tau)) -> 
    let c1 = buildCache e1 gammaf in
      let taux = Typing.extract_type e1 in
        let gammax = M.add x taux gammaf in
          let c2 = buildCache e2 gammax in
            let hashx = Hashing.compute_hash x in
            Cache.add hash (gamma, tau) (
              Cache.add hashx (gamma, taux) (
                Cache.union hash_collision c1 c2))
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
          (Cache.union hash_collision
            cache_vars 
            (Cache.union hash_collision 
              (buildCache e1 gamman) 
              (buildCache e2 gamman)
            )
          )
  | App (e1, e2, (hash, tau)) -> 
    Cache.add hash 
      (gamma, tau)
      (
        Cache.union hash_collision 
          (buildCache e1 gammaf) 
          (
            List.fold_left 
              (fun fvs el -> Cache.union hash_collision el fvs) 
              Cache.empty 
              (
                List.map 
                (fun ei -> buildCache ei gammaf) 
                e2
              )
          )
      )
  | Tuple(es, (hash, tau)) -> List.fold_left (fun fvs el -> Cache.union hash_collision el fvs) Cache.empty (List.map (fun ei -> buildCache ei gammaf) es)
  | LetTuple(t, e1, e2, (hash, tau)) -> 
    let (Type.Tuple(e1s)) = Typing.extract_type e1 in
      let gamma_t = M.add_list2 t e1s gammaf in
        let cache_t = List.fold_left
            (fun c xi -> Cache.add (Hashing.compute_hash xi) (M.add xi (M.find xi gamma_t) M.empty, (M.find xi gamma_t)) c) (* TODO: restrict env over gamma_t *) 
            (buildCache e1 gammaf)
            t in
              Cache.add hash (gamma, tau) (
                Cache.union hash_collision
                  cache_t
                  (buildCache e2 gamma_t)
              )

  | Array (e1 (* dim *), e2 (* init *) , (hash, tau)) 
  | Get (e1 (* array *) , e2 (* idx *), (hash, tau)) -> Cache.add hash (gamma, tau) (Cache.union hash_collision (buildCache e1 gammaf) (buildCache e2 gammaf))
  | Put (e1 (* array *) , e2 (* idx *) , e3 (* ass *), (hash, tau)) -> Cache.add hash (gamma, tau) (Cache.union hash_collision (buildCache e1 gammaf) (Cache.union hash_collision (buildCache e2 gammaf) (buildCache e3 gammaf)))
