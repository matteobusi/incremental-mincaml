open Batteries

open Annotast
open M
open Typing
open Varset

(* TODO: hashtbl instead of maps might be better and more efficient *)
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
(* let add h gamma tau cache = add cache h (gamma, tau) *)

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

(* 
  Function to build the cache given an aAST and a typing environment.
  Cache initially an empty MUTABLE Cache. 
*)
let rec build_cache e env cache =
    let gamma = env in (*FIXME:  M.restrict env (Annotast.free_variables e) *)
    match e with
    | Unit((hash, tau)) -> add cache hash (gamma, tau)
    | Bool(_, (hash, tau))
    | Int(_,  (hash, tau))
    | Float(_, (hash, tau)) -> add cache hash (gamma, tau)
    | Var(x, (hash, tau)) when M.mem x env ->
      let t = M.find x env in
      add cache hash (gamma, t)
    | Var(x, _) -> failwith ("Unbound variable: " ^ x)
    | Not(e1, (hash, tau))
    | Neg(e1, (hash, tau)) 
    | FNeg(e1, (hash, tau)) -> add cache hash (gamma, tau); (build_cache e1 env cache) 
    | IBop(_, e1 , e2, (hash, tau))
    | FBop(_, e1, e2, (hash, tau))
    | Rel(_, e1, e2, (hash, tau)) -> add cache hash (gamma, tau); build_cache e2 env cache; build_cache e1 env cache
    | If (e1, e2, e3, (hash, tau)) -> 
      add cache hash (gamma, tau); build_cache e1 env cache; build_cache e2 env cache; build_cache e3 env cache
    | Let (x, e1, e2, (hash, tau)) -> 
      build_cache e1 env cache; 
      let taux = Typing.extract_type e1 in
      let gammax = M.add x taux env in
      build_cache e2 gammax cache;
      let hashx = Hashing.compute_hash x in
      add cache hash (gamma, tau);
      add cache hashx (gamma, taux)
    | LetRec ({ name = (f, t); args = yts; body = e1 }, e2, (hash, tau)) ->
      let gamman = M.add_list ((f, Type.Fun(List.map snd yts, t))::yts) env in
      List.iter (fun (v, t) -> add cache (Hashing.compute_hash v) (M.add v t M.empty, t)) ((f, Type.Fun(List.map snd yts, t))::yts);
      build_cache e1 gamman cache;
      build_cache e2 gamman cache;
      add cache hash (gamma, tau)
    | App (e1, e2, (hash, tau)) -> 
      List.iter (fun ei -> build_cache ei env cache) e2;
      add cache hash (gamma, tau);
      build_cache e1 env cache
    | Tuple(es, (hash, tau)) -> 
      List.iter (fun ei -> build_cache ei env cache) es
    | LetTuple(t, e1, e2, (hash, tau)) -> 
      let (Type.Tuple(e1s)) = Typing.extract_type e1 in
      let gamma_t = M.add_list2 t e1s gamma in
      build_cache e1 env cache;
      List.iter (fun xi -> add cache (Hashing.compute_hash xi) (M.add xi (M.find xi gamma_t) M.empty, (M.find xi gamma_t))) t;
      build_cache e2 gamma_t cache
    | Array (e1 (* dim *), e2 (* init *) , (hash, tau)) 
    | Get (e1 (* array *) , e2 (* idx *), (hash, tau)) -> add cache hash (gamma, tau); build_cache e1 env cache; build_cache e2 env cache
    | Put (e1 (* array *) , e2 (* idx *) , e3 (* ass *), (hash, tau)) -> add cache hash (gamma, tau); build_cache e1 env cache; build_cache e2 env cache; build_cache e3 env cache
end