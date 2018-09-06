(* Initial implementation of the incremental type-system*)
open Annotast
open Hashing
open M
open Typing
open Varset
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

module IncrementalTyping = struct
(* Build an empty report *)
let report = IncrementalReport.create ()

(* TODO: Incremental type checking implementation, to be refactored *)
let rec typecheck cache env e =
    let compat_env env envp e =
      (* Baseline: *)
      (* let fv = Annotast.free_variables e in VarSet.for_all (fun e -> M.mem e env && M.mem e envp && (M.find e env) = (M.find e envp)) fv *)
      (* 
        compat_env is called on cache hits, i.e. we know that e is the same to the one in cache; 
        Then, we know that we can type it with env given it was in cache; 
        Since env was also minimal, we know that envp must be equal to env to be sure that we can type e. 
      *)
      M.equal (=) env envp
    and cache_res = (Cache.extract_cache (Hashing.extract_simple_hash e) cache) in
    match cache_res with
    | None ->  (* Not found in cache *)
      IncrementalReport.register_miss_none report;
      cache_miss_itc env cache e
    | Some (gamma_cand, tau_cand) when not (compat_env env gamma_cand e) -> (* In cache but context is not valid *)
      IncrementalReport.register_miss_incomp report; 
      cache_miss_itc env cache e
    | Some (gamma_cand, tau_cand) -> IncrementalReport.register_hit report; tau_cand
  and cache_miss_itc gammao cache e = let gamma = M.restrict gammao (free_variables e) in (* Calling this produces a type and MODIFIES cache *)
    match e with
    | Unit(hash) -> Type.Unit
    | Bool(_, hash) -> Type.Bool
    | Int(_, hash) -> Type.Int
    | Float(_, hash) -> Type.Float
    | Var(x, hash) -> let tau = Typing.extract_type (Typing.typecheck gamma e) in Cache.replace cache hash (gamma, tau);  tau
    | Not(e1, hash) -> 
      let tau_e1 = typecheck cache gamma e1 in 
      Typing.check Type.Bool tau_e1; 
      Cache.replace cache hash (gamma, Type.Bool);
      Type.Bool
    | Neg(e1, hash) ->
      let tau_e1 = typecheck cache gamma e1 in 
      Typing.check Type.Int tau_e1; 
      Cache.replace cache hash (gamma, Type.Int);
      Type.Int
    | FNeg(e1, hash) ->
      let tau_e1 = typecheck cache gamma e1 in 
      Typing.check Type.Float tau_e1; 
      Cache.replace cache hash (gamma, Type.Float);
      Type.Float
    | IBop(_, e1, e2, hash) ->
      let tau_e1 = typecheck cache gamma e1 in 
      let tau_e2 = typecheck cache gamma e2 in 
      Typing.check Type.Int tau_e1; 
      Typing.check Type.Int tau_e2; 
      Cache.replace cache hash (gamma, Type.Int);
      Type.Int
    | FBop(_, e1, e2, hash) ->
      let tau_e1 = typecheck cache gamma e1 in 
      let tau_e2 = typecheck cache gamma e2 in 
      Typing.check Type.Float tau_e1; 
      Typing.check Type.Float tau_e2; 
      Cache.replace cache hash (gamma, Type.Float);
      Type.Float
    | Rel(_, e1, e2, hash) ->
      let tau_e1 = typecheck cache gamma e1 in 
      let tau_e2 = typecheck cache gamma e2 in 
      Typing.check tau_e1 tau_e2; 
      Cache.replace cache hash (gamma, Type.Bool);
      Type.Bool
    | If(e1, e2, e3, hash) ->
      let tau_e1 = typecheck cache gamma e1 in 
      let tau_e2 = typecheck cache gamma e2 in 
      let tau_e3 = typecheck cache gamma e3 in 
      Typing.check Type.Bool tau_e1;
      Typing.check tau_e2 tau_e3; 
      Cache.replace cache hash (gamma, tau_e2);
      tau_e2
    | Let(x, e1, e2, hash) -> 
      let tau_e1 = typecheck cache gamma e1 in 
      let tau_e2 = typecheck cache (M.add x tau_e1 gamma) e2 in 
      Cache.replace cache hash (gamma, tau_e2);
      tau_e2
    | LetRec ({ name = (x, t); args = yts; body = e1 }, e2, hash) ->
      let gamma_rec = M.add x (Type.Fun(List.map snd yts, t)) (M.add_list yts gamma) in
      let tau_e1 = typecheck cache gamma_rec e1 in 
      let tau_e2 = typecheck cache gamma_rec e2 in 
      Typing.check tau_e1 t;
      Cache.replace cache hash (gamma, tau_e2);
      tau_e2
    | App (e1, e2, hash) -> 
      let tau_e1 = typecheck cache gamma e1 in 
      let itc_e2 = List.map (fun yi -> typecheck cache gamma yi) e2 in 
      let Type.Fun(ts, tr) = tau_e1 in
      Typing.check tau_e1 (Type.Fun(itc_e2, tr));
      Cache.replace cache hash (gamma, tr);
      tr
    | Tuple(es, hash) ->
      let tes = List.map (fun yi -> typecheck cache gamma yi) es in 
      Cache.replace cache hash (gamma, Type.Tuple(tes));
      Type.Tuple(tes)
    | LetTuple(x, e1, e2, hash) ->
      let tau_e1 = typecheck cache gamma e1 in 
      let Type.Tuple(ts) = tau_e1 in
      if List.length ts <> List.length x 
      then
        failwith "IncTC: Different arity in let tuple."
      else
        let tau_e2 = typecheck cache (M.add_list2 x ts gamma) e2 in 
        Cache.replace cache hash (gamma, tau_e2);
        tau_e2
    | Array(e1, e2, hash) ->
      let tau_e1 = typecheck cache gamma e1 in 
      let tau_e2 = typecheck cache gamma e2 in 
      Typing.check Type.Int tau_e1; 
      Cache.replace cache hash (gamma, Type.Array(tau_e2));
      Type.Array(tau_e2)
    | Get (e1, e2, hash) ->
      let tau_e1 = typecheck cache gamma e1 in 
      let tau_e2 = typecheck cache gamma e2 in 
      let (Type.Array(ts)) = tau_e1 in
      Typing.check Type.Int tau_e2;
      Cache.replace cache hash (gamma, ts);
      ts
    | Put (e1, e2, e3, hash) ->
      let tau_e1 = typecheck cache gamma e1 in 
      let tau_e2 = typecheck cache gamma e2 in 
      let tau_e3 = typecheck cache gamma e3 in 
      Typing.check (Type.Array(tau_e3)) tau_e1;
      Typing.check Type.Int tau_e2; 
      Cache.replace cache hash (gamma, Type.Unit);
      Type.Unit

end