open Annotast

(* Associates our hashes to the corresponding env/type *)
module Cache = Map.Make(struct
    type t = int
    let compare = compare
end)

let hash_collision k v1 v2 = Some v1

(* buildCache: Type.t Annotast.t -> M.t -> Cache *)
let rec buildCache e gamma = match e with
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
              let hash_x = Hashing.compute_hash x (* TODO : dovrebbe funzionare così, ma perch\`e sa come funziona il parser. *) in
                Cache.add hash (gamma, tau) (
                  Cache.add hash_x (gammap, tau) (
                    Cache.union hash_collision c1 c2))
  | _ -> failwith "Error!"
