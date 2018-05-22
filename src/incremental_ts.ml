(* Initial implementation of the incremental type-system*)
open Annotast
open Hashing
open Cache

let extract_cache h cache = Cache.find_opt h cache
let update_cache e gamma tau cache = Cache.add (Hashing.extract_simple_hash e) (environment_restrict gamma e, tau) 

(* check_ctx *)
let check_ctx gamma gamma_prime e = true

(* check/join for each abstract construct *)
let check e tau tau_prime = true
let join e tau tau_prime = Some(tau_prime)

(* incremental_tc : gamma * cache * e * tau -> e_annot * cache_prime *)
let incremental_tc gamma cache (e : int Annotast.t) = match e with
(* Even if trivial: invoke the underlying type system *)
  | Unit(hash) 
  | Bool(_, hash)
  | Int(_, hash)
  | Float(_, hash) -> begin
                        match (extract_cache hash cache) with
                            | None -> let tau = Typing.extract_type (Typing.g gamma e) in (tau, Cache.add hash (environment_restrict gamma e, tau) cache)
                            | Some(gamma_cand, tau_cand) when not (check_ctx gamma gamma_cand e) -> (Typing.extract_type (Typing.g gamma e), cache)
                            | Some(gamma_cand, tau_cand) -> (tau_cand, cache)
                        end
  | _ -> failwith "Incremental TC: not implemented!"
  (* | Var(x, _) -> Typing.extract_type Typing
  | Not(e1, _)
  | Neg(e1, _) 
  | FNeg(e1, _) -> free_variables e1 
  | IBop(_, e1, e2, _)
  | FBop(_, e1, e2, _)
  | Rel(_, e1, e2, _) -> List.append (free_variables e1) (free_variables e2)
  | If(e1,e2,e3, _) -> List.append (List.append (free_variables e1) (free_variables e2)) (free_variables e3)
  | Let(x, e1, e2, _) -> list_remove (List.append (free_variables e1) (free_variables e2)) x
  | LetRec ({ name = (x, t); args = yts; body = e1 }, e2, _) -> List.fold_left list_remove (List.append (free_variables e1) (free_variables e2)) (x::(List.map fst yts))
  | App (e1, es, _) -> List.append (free_variables e1) (List.concat (List.map free_variables es))
  | Tuple(es, _) -> List.concat (List.map free_variables es)
  | LetTuple(xs, e1, e2, _) -> List.fold_left list_remove (List.append (free_variables e1) (free_variables e2)) xs
  | Array(e1, e2, _) -> List.append (free_variables e1) (free_variables e2)
  | Get (e1, e2, _) -> List.append (free_variables e1) (free_variables e2)
  | Put (e1, e2, e3, _) ->  List.append (free_variables e1) (List.append (free_variables e2) (free_variables e3)) *)