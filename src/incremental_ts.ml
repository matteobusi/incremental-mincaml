(* Initial implementation of the incremental type-system*)
open Annotast
open Hashing
open Cache

let extract_cache h cache = Cache.find_opt h cache
let update_cache e gamma tau cache = Cache.add (Hashing.extract_simple_hash e) (environment_restrict gamma e, tau)

(* check_ctx *)
let check_ctx gamma gamma_prime e = true
          
(* incremental_tc : gamma * cache * e -> e_annot * cache_prime *)
let rec incremental_tc gamma cache (e : int Annotast.t) = let cache_res = (extract_cache (Hashing.extract_simple_hash e) cache) in
match cache_res with
(* Found in cache, but not valid *)
| None -> cache_miss_itc gamma cache e
| Some (gamma_cand, tau_cand) when not (check_ctx gamma gamma_cand e) -> cache_miss_itc gamma cache e
(* Found in cache and valid! *)
| Some (gamma_cand, tau_cand) -> begin
                                    match e with
                                    | Unit(hash)
                                    | Bool(_, hash)
                                    | Int(_, hash)
                                    | Float(_, hash)
                                    | Var(_, hash)
                                    | Not( _, hash)
                                    | Neg(_, hash)
                                    | FNeg(_, hash)
                                    | IBop(_, _, _, hash)
                                    | FBop(_, _, _, hash)
                                    | Rel(_, _, _, hash)
                                    | If(_, _, _, hash)
                                    | Let(_, _, _, hash)
                                    | LetRec (_, _, hash)
                                    | App (_, _, hash)
                                    | Tuple(_, hash)
                                    | LetTuple(_, _, _, hash)
                                    | Array(_, _, hash)
                                    | Get (_, _, hash)
                                    | Put (_, _, _, hash) -> (tau_cand, cache)
                                end

and cache_miss_itc gamma cache e = match e with
        | Unit(hash)
        | Bool(_, hash)
        | Int(_, hash)
        | Float(_, hash) -> failwith "IncTC: never happens."
        | Var(x, hash) -> let tau = Typing.extract_type (Typing.g gamma e) in (tau, Cache.add hash (environment_restrict gamma e, tau) cache)
        | Not(e1, hash) -> 
            let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
                Typing.check Type.Bool tau_e1; 
                (Type.Bool, Cache.add hash (M.environment_restrict gamma e, Type.Bool) cache_e1)
        | Neg(e1, hash) ->
            let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
                Typing.check Type.Int tau_e1; 
                (Type.Int, Cache.add hash (M.environment_restrict gamma e, Type.Int) cache_e1)
        | FNeg(e1, hash) ->
            let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
                Typing.check Type.Float tau_e1; 
                (Type.Float, Cache.add hash (M.environment_restrict gamma e, Type.Float) cache_e1)
        | IBop(_, e1, e2, hash) ->
            let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
                let (tau_e2, cache_e2) = incremental_tc gamma cache e2 in 
                    let cache_union = Cache.union hash_collision cache_e1 cache_e2 in
                Typing.check Type.Int tau_e1; 
                Typing.check Type.Int tau_e2; 
                (Type.Bool, Cache.add hash (M.environment_restrict gamma e, Type.Bool) cache_union)
        | FBop(_, e1, e2, hash) ->
            let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
                let (tau_e2, cache_e2) = incremental_tc gamma cache e2 in 
                    let cache_union = Cache.union hash_collision cache_e1 cache_e2 in
                Typing.check Type.Float tau_e1; 
                Typing.check Type.Float tau_e2; 
                (Type.Bool, Cache.add hash (M.environment_restrict gamma e, Type.Bool) cache_union)
        | Rel(_, e1, e2, hash) ->
            let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
                let (tau_e2, cache_e2) = incremental_tc gamma cache e2 in 
                    let cache_union = Cache.union hash_collision cache_e1 cache_e2 in
                Typing.check tau_e1 tau_e2; 
                (Type.Bool, Cache.add hash (M.environment_restrict gamma e, Type.Bool) cache_union)
        | If(e1, e2, e3, hash) ->
            let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
                let (tau_e2, cache_e2) = incremental_tc gamma cache e2 in 
                    let (tau_e3, cache_e3) = incremental_tc gamma cache e3 in 
                        let cache_union = Cache.union hash_collision cache_e1 (Cache.union hash_collision cache_e2 cache_e3) in
                            Typing.check Type.Bool tau_e1;
                            Typing.check tau_e2 tau_e3; 
                            (tau_e2, Cache.add hash (M.environment_restrict gamma e, tau_e2) cache_union)
        | Let(x, e1, e2, hash) -> 
            let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
                let (tau_e2, cache_e2) = incremental_tc (M.add x tau_e1 gamma) cache e2 in 
                        let cache_union = Cache.union hash_collision cache_e1 cache_e2 in
                            (tau_e2, Cache.add hash (M.environment_restrict gamma e, tau_e2) cache_union)
        | LetRec ({ name = (x, t); args = yts; body = e1 }, e2, hash) ->
            let gamma_rec = M.add x (Type.Fun(List.map snd yts, t)) (M.add_list yts gamma) in
                let (tau_e1, cache_e1) = incremental_tc gamma_rec cache e1 in 
                    let (tau_e2, cache_e2) = incremental_tc gamma_rec cache e2 in 
                            let cache_union = Cache.union hash_collision cache_e1 cache_e2 in
                                Typing.check tau_e1 t;
                                (tau_e2, Cache.add hash (M.environment_restrict gamma e, tau_e2) cache_union)
        | App (e1, e2, hash) -> 
            let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
                let itc_e2 = List.map (fun yi -> incremental_tc gamma cache yi) e2 in 
                        let cache_union = (List.fold_left (fun c (t, e) -> Cache.union hash_collision c e) cache_e1 itc_e2) in
                            let Type.Fun(ts,tr) = tau_e1 in
                                Typing.check tau_e1 (Type.Fun(List.map fst itc_e2, tr));
                                (tr, Cache.add hash (M.environment_restrict gamma e, tr) cache_union)
        | Tuple(es, hash) ->
            let itc_es = List.map (fun yi -> incremental_tc gamma cache yi) es in 
                        let cache_union = List.fold_left (fun c (t, e) -> Cache.union hash_collision c e) cache itc_es in
                        let tes = List.map fst itc_es in
                            (Type.Tuple(tes), Cache.add hash (M.environment_restrict gamma e, Type.Tuple(tes)) cache_union)
        | LetTuple(x, e1, e2, hash) ->
            let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
                let (Type.Tuple(ts)) = tau_e1 in
                    if List.length ts <> List.length x then failwith "IncTC: Different arity in let tuple."
                    else
                        let (tau_e2, cache_e2) = incremental_tc (M.add_list2 x ts gamma) cache e2 in 
                            let cache_union = Cache.union hash_collision cache_e1 cache_e2 in
                                (tau_e2, Cache.add hash (M.environment_restrict gamma e, tau_e2) cache_union)
        | Array(e1, e2, hash) ->
            let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
                let (tau_e2, cache_e2) = incremental_tc gamma cache e2 in 
                    let cache_union = Cache.union hash_collision cache_e1 cache_e2 in
                Typing.check Type.Int tau_e1; 
                (Type.Array(tau_e2), Cache.add hash (M.environment_restrict gamma e, Type.Array(tau_e2)) cache_union)
        | Get (e1, e2, hash) ->
            let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
                let (Type.Array(ts)) = tau_e1 in
                    let (tau_e2, cache_e2) = incremental_tc gamma cache e2 in 
                            Typing.check Type.Int tau_e2;
                            let cache_union = Cache.union hash_collision cache_e1 cache_e2 in
                                (ts, Cache.add hash (M.environment_restrict gamma e, ts) cache_union)
        | Put (e1, e2, e3, hash) ->
            let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
                let (tau_e2, cache_e2) = incremental_tc gamma cache e2 in 
                    let (tau_e3, cache_e3) = incremental_tc gamma cache e3 in 
                        let cache_union = Cache.union hash_collision cache_e1 (Cache.union hash_collision cache_e2 cache_e3) in
                            Typing.check (Type.Array(tau_e3)) tau_e1;
                            Typing.check Type.Int tau_e2; 
                            (Type.Unit, Cache.add hash (M.environment_restrict gamma e, Type.Unit) cache_union)