(* type checking *)

open Annotast

exception TypeError of Type.t * Type.t

let extenv = ref M.empty

let check t1 t2 =
  let rec check_iter t1 t2 =
  try
    match (t1, t2) with
  | (Type.Unit, Type.Unit)   -> Type.Unit
  | (Type.Int, Type.Int)     -> Type.Int
  | (Type.Float, Type.Float) -> Type.Float
  | (Type.Bool, Type.Bool)   -> Type.Bool
  | (Type.Array(t1), Type.Array(t2)) -> Type.Array(check_iter t1 t2)
  | (Type.Tuple(ts1), Type.Tuple(ts2)) ->
     let args = List.map (fun (a,b) -> check_iter a b) (List.combine ts1 ts2) in
     Type.Tuple(args)
  | (Type.Fun(ts1, tr1),Type.Fun(ts2, tr2)) ->
     let args = List.map (fun (a,b) -> check_iter a b) (List.combine ts1 ts2) in
     Type.Fun(args, check_iter tr1 tr2)
  | _ ->
     Printf.eprintf "error: %s vs %s\n" (Type.string_of_type t1) (Type.string_of_type t2);
     raise (TypeError(t1, t2))
  with Invalid_argument(_) -> raise (TypeError(t1, t2))
  in
  ignore( check_iter t1 t2);
  ()

let extract_type e : Type.t =
  match e with
  | Unit(_,type_t)
  | Bool(_,(_,type_t))
  | Int(_, (_, type_t))
  | Float(_,(_,type_t))
  | Not(_, (_, type_t))
  | Var(_, (_, type_t))
  | Neg(_, (_, type_t))
  | Add(_,_,(_,type_t))
  | Sub(_,_,(_,type_t))
  | FNeg(_,(_,type_t))
  | FAdd(_,_,(_,type_t))
  | FSub(_,_,(_,type_t))
  | FMul(_,_,(_,type_t))
  | FDiv(_,_,(_,type_t))
  | Eq(_,_,(_,type_t))
  | LE(_,_,(_,type_t))
  | If(_,_,_, (_,type_t))
  | Let(_,_,_, (_,type_t))
  | LetRec(_,_, (_,type_t))
  | App(_,_,(_,type_t))
  | Tuple(_,(_,type_t))
  | LetTuple(_,_,_,(_,type_t))
  | Array(_,_, (_,type_t))
  | Get(_,_, (_,type_t))
  | Put(_, _,_, (_,type_t))-> type_t


let rec g env e = (* *)
    match e with
    | Unit(annot) -> Unit(annot, Type.Unit)
    | Bool(b, annot) -> Bool(b, (annot, Type.Bool))
    | Int(i, annot) -> Int(i, (annot, Type.Int))
    | Float(f, annot) -> Float(f, (annot, Type.Float))
    | Not(e, annot) ->
       let te = g env e in
       check Type.Bool (extract_type te);
       Not(te, (annot, Type.Bool))
    | Neg(e, annot) ->
       let te = g env e in
       check Type.Int (extract_type te);
       Neg(te, (annot, Type.Int))
    | Add(e1, e2, annot) ->
       let te1 = (g env e1) in
       let te2 = (g env e2) in
       check Type.Int (extract_type te1);
       check Type.Int (extract_type te2);
       Add(te1, te2, (annot, Type.Int))
    | Sub(e1, e2, annot) ->
       let te1 = (g env e1) in
       let te2 = (g env e2) in
       check Type.Int (extract_type te1);
       check Type.Int (extract_type te2);
       Sub(te1, te2, (annot, Type.Int))
    | FNeg(e, annot) ->
       let te = g env e in
       check Type.Float (extract_type te);
       FNeg(te, (annot, Type.Float))
    | FAdd(e1, e2, annot)  ->
       let te1 = g env e1 in
       let te2 = g env e2 in
       check Type.Float (extract_type te1);
       check Type.Float (extract_type te2);
       FAdd(te1,te2, (annot,Type.Float))
    | FSub(e1, e2, annot) ->
       let te1 = g env e1 in
       let te2 = g env e2 in
       check Type.Float (extract_type te1);
       check Type.Float (extract_type te2);
       FSub(te1,te2, (annot,Type.Float))
    | FMul(e1, e2, annot) ->
       let te1 = g env e1 in
       let te2 = g env e2 in
       check Type.Float (extract_type te1);
       check Type.Float (extract_type te2);
       FMul(te1,te2, (annot,Type.Float))
    | FDiv(e1, e2, annot) ->
       let te1 = g env e1 in
       let te2 = g env e2 in
       check Type.Float (extract_type te1);
       check Type.Float (extract_type te2);
       FDiv(te1,te2, (annot, Type.Float))
    | Eq(e1, e2, annot) ->
       let te1 = g env e1 in
       let te2 = g env e2 in
       check  (extract_type te1) (extract_type te2);
       Eq(te1, te2, (annot, Type.Bool))
    | LE(e1, e2, annot) ->
       let te1 = g env e1 in
       let te2 = g env e2 in
       check (extract_type te1) (extract_type te2);
       LE(te1, te2, (annot, Type.Bool))
    | If(e1, e2, e3, annot) ->
       let te1 = g env e1 in
       check (extract_type te1) Type.Bool;
       let te2 = g env e2 in
       let te3 = g env e3 in
       let t = extract_type te2 in
       check t (extract_type te3);
       If(te1, te2, te2, (annot, t))
    | Let((x, t), e1, e2, annot) ->
       let te1 = g env e1 in
       check  t (extract_type te1);
       let te2 = g (M.add x t env) e2 in
       Let((x,t), te1, te2, (annot, extract_type te2))
    | Var(x, annot) when M.mem x env ->
       let t = M.find x env in
       Var(x, (annot, t))
    | Var(x, annot) when M.mem x !extenv ->
       let t = M.find x !extenv in
       Var(x, (annot, t))
    | Var(x, _) -> failwith "Unbound variable"
    | LetRec({ name = (x, t); args = yts; body = e1 }, e2, annot) ->
       let env = M.add x t env in
       let tes1 = g (M.add_list yts env) e1 in
       check t (Type.Fun(List.map snd yts, (extract_type tes1)));
       let tes2 =  g env e2 in
       LetRec({name = (x,t); args = yts; body = tes1}, tes2, (annot, extract_type tes2))
    | App(e, es, annot) ->
       (* TODO: improve that *)
       let tes = g env e in
       let tess = List.map (g env) es in
       let (Type.Fun(ts,tr) as t) = extract_type tes in
       check t (Type.Fun(List.map extract_type tess, tr));
       App(tes, tess, (annot, t))
    | Tuple(es, annot) ->
       let tes = List.map (g env) es in
       let ts = List.map extract_type tes in
       Tuple(tes, (annot, Type.Tuple(ts)))
    | LetTuple(xts, e1, e2, annot) ->
       let tes1 = g env e1 in
       check (Type.Tuple(List.map snd xts)) (extract_type tes1);
       let tes2 =  g (M.add_list xts env) e2 in
       LetTuple(xts, tes1, tes2, (annot, extract_type tes2))
    | Array(e1, e2, annot) ->
       let tes1 = g env e1 in
       let tes2 = g env e2 in
       check (extract_type tes1) Type.Int;
       Array(tes1, tes2, (annot, Type.Array(extract_type tes2)))
    | Get(e1, e2, annot) ->
             (* TODO: improve that *)
       let tes1 = g env e1 in
       let tes2 = g env e2 in
       let (Type.Array(t)) =  extract_type tes1 in
       check Type.Int (extract_type tes2);
       Get(tes1, tes2, (annot, t))
    | Put(e1, e2, e3, annot) ->
       let tes1 = g env e1 in
       let tes2 = g env e2 in
       let tes3 = g env e3 in
       check (Type.Array(extract_type tes3)) (extract_type tes1);
       check Type.Int (extract_type tes2);
       Put(tes1,tes2,tes3, (annot,  Type.Unit))


(* let rec g env e = (\* *\)
 *     match e with
 *     | Unit -> Type.Unit
 *     | Bool(_) -> Type.Bool
 *     | Int(_) -> Type.Int
 *     | Float(_) -> Type.Float
 *     | Not(e) ->
 *         check e  Type.Bool (g env e);
 *         Type.Bool
 *     | Neg(e) ->
 *         check e  Type.Int (g env e);
 *         Type.Int
 *     | Add(e1, e2) | Sub(e1, e2) ->
 *         check e  Type.Int (g env e1);
 *         check e  Type.Int (g env e2);
 *         Type.Int
 *     | FNeg(e) ->
 *         check e  Type.Float (g env e);
 *         Type.Float
 *     | FAdd(e1, e2) | FSub(e1, e2) | FMul(e1, e2) | FDiv(e1, e2) ->
 *         check e  Type.Float (g env e1);
 *         check e  Type.Float (g env e2);
 *         Type.Float
 *     | Eq(e1, e2) | LE(e1, e2) ->
 *         check e  (g env e1) (g env e2);
 *         Type.Bool
 *     | If(e1, e2, e3) ->
 *         check e  (g env e1) Type.Bool;
 *         let t2 = g env e2 in
 *         let t3 = g env e3 in
 *         check e  t2 t3;
 *         t2
 *     | Let((x, t), e1, e2) ->
 *         check e  t (g env e1);
 *         g (M.add x t env) e2
 *     | Var(x) when M.mem x env -> M.find x env
 *     | Var(x) when M.mem x !extenv -> M.find x !extenv
 *     | Var(x) -> failwith "Unbound variable"
 * (\*
 *         Format.eprintf "free variable %s assumed as external@." x;
 *         let t = Type.gentyp () in
 *         extenv := M.add x t !extenv;
 *         t
 *  *\)
 *     | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
 *         let env = M.add x t env in
 *         check e  t (Type.Fun(List.map snd yts, g (M.add_list yts env) e1));
 *         g env e2
 *     | App(e, es) ->
 *        (\* TODO: improve that *\)
 *         let (Type.Fun(ts,tr) as t) = g env e in
 *         check e t (Type.Fun(List.map (g env) es, tr));
 *         t
 *     | Tuple(es) -> Type.Tuple(List.map (g env) es)
 *     | LetTuple(xts, e1, e2) ->
 *         check e  (Type.Tuple(List.map snd xts)) (g env e1);
 *         g (M.add_list xts env) e2
 *     | Array(e1, e2) ->
 *         check e  (g env e1) Type.Int;
 *         Type.Array(g env e2)
 *     | Get(e1, e2) ->
 *         (\* TODO: improve that *\)
 *        let (Type.Array(t)) = g env e1 in
 *        check e  Type.Int (g env e2);
 *        t
 *     | Put(e1, e2, e3) ->
 *         let t = g env e3 in
 *         check e  (Type.Array(t)) (g env e1);
 *         check e  Type.Int (g env e2);
 *         Type.Unit *)



let f e =
  extenv := M.empty;
  g M.empty e
(*
  (match deref_typ (g M.empty e) with
  | Type.Unit -> ()
  | _ -> Format.eprintf "warning: final result does not have type unit@.");

  (try unify Type.Unit (g M.empty e)
  with Unify _ -> failwith "top level does not have type unit");
  extenv := M.map deref_typ !extenv;
  deref_term e
*)
