open Batteries

open Annotast
open M
open Typing
open Varset

(* Associates each node hash to its corresponding environment and type *)
module Cache = 
struct

include Hashtbl.Make(struct
          type t = int
          let equal i j = i=j
          let hash i = i land max_int
        end)

let create_empty n = create n
let extract_cache h cache = find_option cache h

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
  iter print_pairs c

let rec get_hashed_tree (e : ((int * VarSet.t ) * Type.t) Annotast.t) = 
    match e with
  | Unit(annot) -> Unit((fst (fst annot), snd annot))
  | Bool(v, annot) -> Bool(v, (fst (fst annot), snd annot))
  | Int(v, annot) -> Int(v, (fst (fst annot), snd annot))
  | Float(v, annot) -> Float(v, (fst (fst annot), snd annot))
  | Not(e, annot) -> Not(get_hashed_tree e, (fst (fst annot), snd annot))
  | Var(x, annot) -> Var(x, (fst (fst annot), snd annot))
  | Neg(e, annot) -> Neg(get_hashed_tree e, (fst (fst annot), snd annot))
  | FNeg(e, annot) -> FNeg(get_hashed_tree e, (fst (fst annot), snd annot))
  | IBop(op,e1,e2, annot) -> IBop(op,get_hashed_tree e1, get_hashed_tree e2, (fst (fst annot), snd annot))
  | FBop(op,e1,e2, annot) -> FBop(op,get_hashed_tree e1, get_hashed_tree e2, (fst (fst annot), snd annot))
  | Rel(op,e1,e2, annot) -> Rel(op, get_hashed_tree e1, get_hashed_tree e2, (fst (fst annot), snd annot))
  | If(e1,e2,e3, annot) -> If (get_hashed_tree e1, get_hashed_tree e2, get_hashed_tree e3, (fst (fst annot), snd annot))
  | Let(x,e1,e2, annot) -> Let (x, get_hashed_tree e1, get_hashed_tree e2, (fst (fst annot), snd annot))
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2, annot) -> LetRec({ name = (x, t); args = yts; body = get_hashed_tree e1 }, get_hashed_tree e2, (fst (fst annot), snd annot))
  | App(e1, el, annot) -> App(get_hashed_tree e1, List.map get_hashed_tree el, (fst (fst annot), snd annot))
  | Tuple(el, annot) -> Tuple(List.map get_hashed_tree el, (fst (fst annot), snd annot))
  | LetTuple(x,e1,e2, annot) -> LetTuple(x, get_hashed_tree e1, get_hashed_tree e2, (fst (fst annot), snd annot))
  | Array(e1, e2, annot) -> Array(get_hashed_tree e1, get_hashed_tree e2, (fst (fst annot), snd annot))
  | Get(e1, e2, annot) -> Get(get_hashed_tree e1, get_hashed_tree e2, (fst (fst annot), snd annot))
  | Put(e1, e2, e3, annot) -> Put(get_hashed_tree e1, get_hashed_tree e2, get_hashed_tree e3, (fst (fst annot), snd annot))

(* 
  Function to build the cache given an aAST and a typing environment.
  Cache initially an empty MUTABLE Cache. 
*)
let rec build_cache (e : ((int * VarSet.t ) * Type.t) Annotast.t) (gamma : Type.t M.t) cache =
  let rec build_cache_aux e gamma cache =
    match e with
    | Unit((hash, tau)) -> add cache hash (gamma, tau)
    | Bool(_, (hash, tau))
    | Int(_,  (hash, tau))
    | Float(_, (hash, tau)) -> add cache hash (gamma, tau)
    | Var(x, (hash, tau)) when M.mem x gamma ->
      let t = M.find x gamma in
      add cache hash (gamma, t)
    | Var(x, _) -> failwith ("Unbound variable: " ^ x)
    | Not(e1, (hash, tau))
    | Neg(e1, (hash, tau)) 
    | FNeg(e1, (hash, tau)) -> add cache hash (gamma, tau); (build_cache_aux e1 gamma cache) 
    | IBop(_, e1 , e2, (hash, tau))
    | FBop(_, e1, e2, (hash, tau))
    | Rel(_, e1, e2, (hash, tau)) -> add cache hash (gamma, tau); build_cache_aux e2 gamma cache; build_cache_aux e1 gamma cache
    | If (e1, e2, e3, (hash, tau)) -> 
      add cache hash (gamma, tau); build_cache_aux e1 gamma cache; build_cache_aux e2 gamma cache; build_cache_aux e3 gamma cache
    | Let (x, e1, e2, (hash, tau)) -> 
      build_cache_aux e1 gamma cache; 
      let taux = Typing.extract_type e1 in
      let gammax = M.add x taux gamma in
      build_cache_aux e2 gammax cache;
      let hashx = Hashing.compute_hash x in
      add cache hash (gamma, tau);
      add cache hashx (gamma, taux)
    | LetRec ({ name = (f, t); args = yts; body = e1 }, e2, (hash, tau)) ->
      let gamman = M.add_list ((f, Type.Fun(List.map snd yts, t))::yts) gamma in
      List.iter (fun (v, t) -> add cache (Hashing.compute_hash v) (M.add v t (M.empty ()) , t)) ((f, Type.Fun(List.map snd yts, t))::yts);
      build_cache_aux e1 gamman cache;
      build_cache_aux e2 gamman cache;
      add cache hash (gamma, tau)
    | App (e1, e2, (hash, tau)) -> 
      List.iter (fun ei -> build_cache_aux ei gamma cache) e2;
      add cache hash (gamma, tau);
      build_cache_aux e1 gamma cache
    | Tuple(es, (hash, tau)) -> 
      List.iter (fun ei -> build_cache_aux ei gamma cache) es
    | LetTuple(t, e1, e2, (hash, tau)) -> 
      let (Type.Tuple(e1s)) = Typing.extract_type e1 in
      let gamma_t = M.add_list2 t e1s gamma in
      build_cache_aux e1 gamma cache;
      List.iter (fun xi -> add cache (Hashing.compute_hash xi) (M.add xi (M.find xi gamma_t) (M.empty ()) , (M.find xi gamma_t))) t;
      build_cache_aux e2 gamma_t cache
    | Array (e1 (* dim *), e2 (* init *) , (hash, tau)) 
    | Get (e1 (* array *) , e2 (* idx *), (hash, tau)) -> add cache hash (gamma, tau); build_cache_aux e1 gamma cache; build_cache_aux e2 gamma cache
    | Put (e1 (* array *) , e2 (* idx *) , e3 (* ass *), (hash, tau)) -> add cache hash (gamma, tau); build_cache_aux e1 gamma cache; build_cache_aux e2 gamma cache; build_cache_aux e3 gamma cache
    in build_cache_aux (get_hashed_tree e) gamma cache
end