(* Initial implementation of the incremental type-system*)
open Annotast
open Hashing
open Cache

module IncrementalReport = struct
  type report_data = { mutable cache_miss_inc : int; mutable cache_miss_none : int; mutable cache_hit : int; mutable nc : int}
  let create () = { cache_hit = 0; cache_miss_inc = 0; cache_miss_none = 0; nc = 0}
  let reset r = r.cache_hit <- 0; r.cache_miss_inc <- 0; r.cache_miss_none <- 0; r.nc <- 0
  let set_nc nc r = r.nc <- nc
  let register_hit r = r.cache_hit <- r.cache_hit + 1
  let register_miss_incomp r = r.cache_miss_inc <- r.cache_miss_inc + 1
  let register_miss_none r = r.cache_miss_none <- r.cache_miss_none + 1
  let string_of_report r = Printf.sprintf "[Visited: %d/%d] H: %d - M: %d (I) + %d (NF) = %d" (r.cache_miss_inc+r.cache_miss_none+r.cache_hit) r.nc r.cache_hit r.cache_miss_inc r.cache_miss_none (r.cache_miss_inc+r.cache_miss_none)
end

(* Build an empty report to be *)
let report = IncrementalReport.create ()

(* TODO: Incremental type checking implementation, to be refactored *)
let rec incremental_tc gamma cache (e : int Annotast.t) = (* check_ctx *)
  let check_ctx gamma gamma_prime e = let fv = free_variables e in ((List.for_all (fun v -> M.mem v gamma) fv) && 
                                                                    (List.for_all (fun v -> M.mem v gamma_prime) fv) &&
                                                                    (List.for_all (fun v -> (M.find v gamma) = (M.find v gamma_prime)) fv))
  and cache_res = (Cache.extract_cache (Hashing.extract_simple_hash e) cache) in
  match cache_res with
  | None ->  (* Not found in cache *)
    IncrementalReport.register_miss_none report; 
    cache_miss_itc gamma cache e
  | Some (gamma_cand, tau_cand) when not (check_ctx gamma gamma_cand e) -> (* In cache but context is not valid *)
    IncrementalReport.register_miss_incomp report; 
    cache_miss_itc gamma cache e
  | Some (gamma_cand, tau_cand) -> (* Found in cache and valid! *)
    begin
      IncrementalReport.register_hit report;
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
and cache_miss_itc gammao cache e = let gamma = M.restrict gammao e in
  match e with
  | Unit(hash) -> (Type.Unit, cache)
  | Bool(_, hash) -> (Type.Bool, cache)
  | Int(_, hash) -> (Type.Int, cache)
  | Float(_, hash) -> (Type.Float, cache)
  | Var(x, hash) -> let tau = Typing.extract_type (Typing.typecheck gamma e) in (tau, Cache.add hash (gamma, tau) cache)
  | Not(e1, hash) -> 
    let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
    Typing.check Type.Bool tau_e1; 
    (Type.Bool, Cache.add hash (gamma, Type.Bool) cache_e1)
  | Neg(e1, hash) ->
    let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
    Typing.check Type.Int tau_e1; 
    (Type.Int, Cache.add hash (gamma, Type.Int) cache_e1)
  | FNeg(e1, hash) ->
    let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
    Typing.check Type.Float tau_e1; 
    (Type.Float, Cache.add hash (gamma, Type.Float) cache_e1)
  | IBop(_, e1, e2, hash) ->
    let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
    let (tau_e2, cache_e2) = incremental_tc gamma cache e2 in 
    let cache_union = Cache.c_union cache_e1 cache_e2 in
    Typing.check Type.Int tau_e1; 
    Typing.check Type.Int tau_e2; 
    (Type.Int, Cache.add hash (gamma, Type.Int) cache_union)
  | FBop(_, e1, e2, hash) ->
    let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
    let (tau_e2, cache_e2) = incremental_tc gamma cache e2 in 
    let cache_union = Cache.c_union cache_e1 cache_e2 in
    Typing.check Type.Float tau_e1; 
    Typing.check Type.Float tau_e2; 
    (Type.Float, Cache.add hash (gamma, Type.Float) cache_union)
  | Rel(_, e1, e2, hash) ->
    let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
    let (tau_e2, cache_e2) = incremental_tc gamma cache e2 in 
    let cache_union = Cache.c_union cache_e1 cache_e2 in
    Typing.check tau_e1 tau_e2; 
    (Type.Bool, Cache.add hash (gamma, Type.Bool) cache_union)
  | If(e1, e2, e3, hash) ->
    let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
    let (tau_e2, cache_e2) = incremental_tc gamma cache e2 in 
    let (tau_e3, cache_e3) = incremental_tc gamma cache e3 in 
    let cache_union = Cache.c_union cache_e1 (Cache.c_union cache_e2 cache_e3) in
    Typing.check Type.Bool tau_e1;
    Typing.check tau_e2 tau_e3; 
    (tau_e2, Cache.add hash (gamma, tau_e2) cache_union)
  | Let(x, e1, e2, hash) -> 
    let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
    let (tau_e2, cache_e2) = incremental_tc (M.add x tau_e1 gamma) cache e2 in 
    let cache_union = Cache.c_union cache_e1 cache_e2 in
    (tau_e2, Cache.add hash (gamma, tau_e2) cache_union)
  | LetRec ({ name = (x, t); args = yts; body = e1 }, e2, hash) ->
    let gamma_rec = M.add x (Type.Fun(List.map snd yts, t)) (M.add_list yts gamma) in
    let (tau_e1, cache_e1) = incremental_tc gamma_rec cache e1 in 
    let (tau_e2, cache_e2) = incremental_tc gamma_rec cache e2 in 
    let cache_union = Cache.c_union cache_e1 cache_e2 in
    Typing.check tau_e1 t;
    (tau_e2, Cache.add hash (gamma, tau_e2) cache_union)
  | App (e1, e2, hash) -> 
    let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
    let itc_e2 = List.map (fun yi -> incremental_tc gamma cache yi) e2 in 
    let cache_union = (List.fold_left (fun c (t, e) -> Cache.c_union c e) cache_e1 itc_e2) in
    let Type.Fun(ts,tr) = tau_e1 in
    Typing.check tau_e1 (Type.Fun(List.map fst itc_e2, tr));
    (tr, Cache.add hash (gamma, tr) cache_union)
  | Tuple(es, hash) ->
    let itc_es = List.map (fun yi -> incremental_tc gamma cache yi) es in 
    let cache_union = List.fold_left (fun c (t, e) -> Cache.c_union c e) cache itc_es in
    let tes = List.map fst itc_es in
    (Type.Tuple(tes), Cache.add hash (gamma, Type.Tuple(tes)) cache_union)
  | LetTuple(x, e1, e2, hash) ->
    let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
    let (Type.Tuple(ts)) = tau_e1 in
    if List.length ts <> List.length x then failwith "IncTC: Different arity in let tuple."
    else
      let (tau_e2, cache_e2) = incremental_tc (M.add_list2 x ts gamma) cache e2 in 
      let cache_union = Cache.c_union cache_e1 cache_e2 in
      (tau_e2, Cache.add hash (gamma, tau_e2) cache_union)
  | Array(e1, e2, hash) ->
    let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
    let (tau_e2, cache_e2) = incremental_tc gamma cache e2 in 
    let cache_union = Cache.c_union cache_e1 cache_e2 in
    Typing.check Type.Int tau_e1; 
    (Type.Array(tau_e2), Cache.add hash (gamma, Type.Array(tau_e2)) cache_union)
  | Get (e1, e2, hash) ->
    let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
    let (Type.Array(ts)) = tau_e1 in
    let (tau_e2, cache_e2) = incremental_tc gamma cache e2 in 
    Typing.check Type.Int tau_e2;
    let cache_union = Cache.c_union cache_e1 cache_e2 in
    (ts, Cache.add hash (gamma, ts) cache_union)
  | Put (e1, e2, e3, hash) ->
    let (tau_e1, cache_e1) = incremental_tc gamma cache e1 in 
    let (tau_e2, cache_e2) = incremental_tc gamma cache e2 in 
    let (tau_e3, cache_e3) = incremental_tc gamma cache e3 in 
    let cache_union = Cache.c_union cache_e1 (Cache.c_union cache_e2 cache_e3) in
    Typing.check (Type.Array(tau_e3)) tau_e1;
    Typing.check Type.Int tau_e2; 
    (Type.Unit, Cache.add hash (gamma, Type.Unit) cache_union)