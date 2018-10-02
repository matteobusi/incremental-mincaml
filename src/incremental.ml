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

let compat_env_default env envp (e : (int * VarSet.t) Annotast.t) =
  (* 
    Straightorward implementation from the theory:
  *)
  let fv = snd (Annotast.get_annot e) in
  VarSet.for_all (fun v -> (M.find v env) = (M.find v envp)) fv
  (* 
    Sometimes faster implementation:
  *)
  (* BatEnum.equal (fun (k1,v1) (k2,v2) ->  if (VarSet.mem k2 fv) then v1=v2 else true) (M.enum env) (M.enum envp)   *)

(* This takes an aAST with annotation (Hash.t * VarSet.t) and incrementally typecheks it *)
let rec typecheck ?(compat_env=compat_env_default) cache env (e : (int * VarSet.t) Annotast.t) =
    let extract_hash e = fst (Annotast.get_annot e) in
    let cache_res = (Cache.extract_cache (extract_hash e) cache) in
      match cache_res with
      | None ->  (* Not found in cache *)
        IncrementalReport.register_miss_none report;
        cache_miss_itc compat_env env cache e
      | Some (gamma_cand, tau_cand) when compat_env env gamma_cand e -> (* In cache but context is not valid *)
        IncrementalReport.register_hit report; tau_cand
      | _ ->  IncrementalReport.register_miss_incomp report;  cache_miss_itc compat_env env cache e

    and cache_miss_itc compat_env gamma cache e = 
      match e with
      | Unit((hash, _)) -> Type.Unit
      | Bool(_, (hash, _)) -> Type.Bool
      | Int(_, (hash, _)) -> Type.Int
      | Float(_, (hash, _)) -> Type.Float
      | Var(x, (hash, _)) ->
        let tau = Typing.extract_type (Typing.typecheck gamma e) in 
        Cache.replace cache hash (gamma, tau);  
        tau
      | Not(e1, (hash, _)) -> 
        let tau_e1 = typecheck ~compat_env:(compat_env) cache gamma e1 in 
        Typing.check Type.Bool tau_e1; 
        Cache.replace cache hash (gamma, Type.Bool);
        Type.Bool
      | Neg(e1, (hash, _)) ->
        let tau_e1 = typecheck ~compat_env:(compat_env) cache gamma e1 in 
        Typing.check Type.Int tau_e1; 
        Cache.replace cache hash (gamma, Type.Int);
        Type.Int
      | FNeg(e1, (hash, _)) ->
        let tau_e1 = typecheck ~compat_env:(compat_env) cache gamma e1 in 
        Typing.check Type.Float tau_e1; 
        Cache.replace cache hash (gamma, Type.Float);
        Type.Float
      | IBop(_, e1, e2, (hash, _)) ->
        let tau_e1 = typecheck ~compat_env:(compat_env) cache gamma e1 in 
        let tau_e2 = typecheck ~compat_env:(compat_env) cache gamma e2 in 
        Typing.check Type.Int tau_e1; 
        Typing.check Type.Int tau_e2; 
        Cache.replace cache hash (gamma, Type.Int);
        Type.Int
      | FBop(_, e1, e2, (hash, _)) ->
        let tau_e1 = typecheck ~compat_env:(compat_env) cache gamma e1 in 
        let tau_e2 = typecheck ~compat_env:(compat_env) cache gamma e2 in 
        Typing.check Type.Float tau_e1; 
        Typing.check Type.Float tau_e2; 
        Cache.replace cache hash (gamma, Type.Float);
        Type.Float
      | Rel(_, e1, e2, (hash, _)) ->
        let tau_e1 = typecheck ~compat_env:(compat_env) cache gamma e1 in 
        let tau_e2 = typecheck ~compat_env:(compat_env) cache gamma e2 in 
        Typing.check tau_e1 tau_e2; 
        Cache.replace cache hash (gamma, Type.Bool);
        Type.Bool
      | If(e1, e2, e3, (hash, _)) ->
        let tau_e1 = typecheck ~compat_env:(compat_env) cache gamma e1 in 
        let tau_e2 = typecheck ~compat_env:(compat_env) cache gamma e2 in 
        let tau_e3 = typecheck ~compat_env:(compat_env) cache gamma e3 in 
        Typing.check Type.Bool tau_e1;
        Typing.check tau_e2 tau_e3; 
        Cache.replace cache hash (gamma, tau_e2);
        tau_e2
      | Let(x, e1, e2, (hash, _)) -> 
        let tau_e1 = typecheck ~compat_env:(compat_env) cache gamma e1 in 
        let tau_e2 = typecheck ~compat_env:(compat_env) cache (M.add x tau_e1 gamma) e2 in 
        Cache.replace cache hash (gamma, tau_e2);
        tau_e2
      | LetRec ({ name = (x, t); args = yts; body = e1 }, e2, (hash, _)) ->
        let gamma_rec = M.add x (Type.Fun(List.map snd yts, t)) (M.add_list yts gamma) in
        let tau_e1 = typecheck ~compat_env:(compat_env) cache gamma_rec e1 in 
        let tau_e2 = typecheck ~compat_env:(compat_env) cache gamma_rec e2 in 
        Typing.check tau_e1 t;
        Cache.replace cache hash (gamma, tau_e2);
        tau_e2
      | App (e1, e2, (hash, _)) -> 
        let tau_e1 = typecheck ~compat_env:(compat_env) cache gamma e1 in 
        let itc_e2 = List.map (fun yi -> typecheck ~compat_env:(compat_env) cache gamma yi) e2 in 
        let Type.Fun(ts, tr) = tau_e1 in
        Typing.check tau_e1 (Type.Fun(itc_e2, tr));
        Cache.replace cache hash (gamma, tr);
        tr
      | Tuple(es, (hash, _)) ->
        let tes = List.map (fun yi -> typecheck ~compat_env:(compat_env) cache gamma yi) es in 
        Cache.replace cache hash (gamma, Type.Tuple(tes));
        Type.Tuple(tes)
      | LetTuple(x, e1, e2, (hash, _)) ->
        let tau_e1 = typecheck ~compat_env:(compat_env) cache gamma e1 in 
        let Type.Tuple(ts) = tau_e1 in
        if List.length ts <> List.length x 
        then
          failwith "IncTC: Different arity in let tuple."
        else
          let tau_e2 = typecheck ~compat_env:(compat_env) cache (M.add_list2 x ts gamma) e2 in 
          Cache.replace cache hash (gamma, tau_e2);
          tau_e2
      | Array(e1, e2, (hash, _)) ->
        let tau_e1 = typecheck ~compat_env:(compat_env) cache gamma e1 in 
        let tau_e2 = typecheck ~compat_env:(compat_env) cache gamma e2 in 
        Typing.check Type.Int tau_e1; 
        Cache.replace cache hash (gamma, Type.Array(tau_e2));
        Type.Array(tau_e2)
      | Get (e1, e2, (hash, _)) ->
        let tau_e1 = typecheck ~compat_env:(compat_env) cache gamma e1 in 
        let tau_e2 = typecheck ~compat_env:(compat_env) cache gamma e2 in 
        let (Type.Array(ts)) = tau_e1 in
        Typing.check Type.Int tau_e2;
        Cache.replace cache hash (gamma, ts);
        ts
      | Put (e1, e2, e3, (hash, _)) ->
        let tau_e1 = typecheck ~compat_env:(compat_env) cache gamma e1 in 
        let tau_e2 = typecheck ~compat_env:(compat_env) cache gamma e2 in 
        let tau_e3 = typecheck ~compat_env:(compat_env) cache gamma e3 in 
        Typing.check (Type.Array(tau_e3)) tau_e1;
        Typing.check Type.Int tau_e2; 
        Cache.replace cache hash (gamma, Type.Unit);
        Type.Unit
end