(* Non incremental type checker *)
open Batteries

open Annotast
open M

module Typing = struct
  exception TypeError of Type.t * Type.t

  let check t1 t2 =
    let rec check_iter t1 t2 =
    try
      match (t1, t2) with
    | (Type.Unit, Type.Unit) -> Type.Unit
    | (Type.Int, Type.Int) -> Type.Int
    | (Type.Float, Type.Float) -> Type.Float
    | (Type.Bool, Type.Bool) -> Type.Bool
    | (Type.Array(t1), Type.Array(t2)) -> Type.Array(check_iter t1 t2)
    | (Type.Tuple(ts1), Type.Tuple(ts2)) ->
        let args = List.map (fun (a,b) -> check_iter a b) (List.combine ts1 ts2) in Type.Tuple(args)
    | (Type.Fun(ts1, tr1),Type.Fun(ts2, tr2)) ->
        let args = List.map (fun (a,b) -> check_iter a b) (List.combine ts1 ts2) in Type.Fun(args, check_iter tr1 tr2)
    | _ ->
      Printf.eprintf "error: %s vs %s\n" (Type.string_of_type t1) (Type.string_of_type t2);
      raise (TypeError(t1, t2))
    with Invalid_argument(_) -> raise (TypeError(t1, t2))
    in ignore(check_iter t1 t2)

  let extract_type e = snd (Annotast.get_annot e)

  let rec typecheck env e = (* *)
      match e with
      | Unit(annot) -> Unit(annot, Type.Unit)
      | Bool(b, annot) -> Bool(b, (annot, Type.Bool))
      | Int(i, annot) -> Int(i, (annot, Type.Int))
      | Float(f, annot) -> Float(f, (annot, Type.Float))
      | Not(e, annot) ->
        let te = typecheck env e in
        check Type.Bool (extract_type te);
        Not(te, (annot, Type.Bool))
      | Neg(e, annot) ->
        let te = typecheck env e in
        check Type.Int (extract_type te);
        Neg(te, (annot, Type.Int))
      | IBop(op,e1, e2, annot) ->
        let te1 = (typecheck env e1) in
        let te2 = (typecheck env e2) in
        check Type.Int (extract_type te1);
        check Type.Int (extract_type te2);
        IBop(op,te1, te2, (annot, Type.Int))
      | FNeg(e, annot) ->
        let te = typecheck env e in
        check Type.Float (extract_type te);
        FNeg(te, (annot, Type.Float))
      | FBop(op,e1, e2, annot)  ->
        let te1 = typecheck env e1 in
        let te2 = typecheck env e2 in
        check Type.Float (extract_type te1);
        check Type.Float (extract_type te2);
        FBop(op,te1,te2, (annot,Type.Float))
      | Rel(op,e1, e2, annot) ->
        let te1 = typecheck env e1 in
        let te2 = typecheck env e2 in
        check  (extract_type te1) (extract_type te2);
        Rel(op,te1, te2, (annot, Type.Bool))
      | If(e1, e2, e3, annot) ->
        let te1 = typecheck env e1 in
        check (extract_type te1) Type.Bool;
        let te2 = typecheck env e2 in
        let te3 = typecheck env e3 in
        let t = extract_type te2 in
        check t (extract_type te3);
        If(te1, te2, te3, (annot, t))
      | Let(x, e1, e2, annot) ->
        let te1 = typecheck env e1 in
        (* check  t (extract_type te1); *)
        let te2 = typecheck (M.add x (extract_type te1) env) e2 in
        Let(x, te1, te2, (annot, extract_type te2))
      | Var(x, annot) when M.mem x env ->
        let t = M.find x env in
        Var(x, (annot, t))
      | Var(x, _) -> failwith ("Unbound variable: " ^ x)
      | LetRec({ name = (x, t); args = yts; body = e1 }, e2, annot) ->
        let funtype = Type.Fun(List.map snd yts, t) in
        let env = M.add x funtype env in
        let tes1 = typecheck (M.add_list yts env) e1 in
        check t (extract_type tes1);
        let tes2 =  typecheck env e2 in
        LetRec({name = (x,t); args = yts; body = tes1}, tes2, (annot, extract_type tes2))
      | App(e, es, annot) ->
        (* TODO: improve that *)
        let tes = typecheck env e in
        let tess = List.map (typecheck env) es in
        let (Type.Fun(ts,tr) as t) = extract_type tes in
        check t (Type.Fun(List.map extract_type tess, tr));
        App(tes, tess, (annot, tr))
      | Tuple(es, annot) ->
        let tes = List.map (typecheck env) es in
        let ts = List.map extract_type tes in
        Tuple(tes, (annot, Type.Tuple(ts)))
      | LetTuple(xs, e1, e2, annot) ->
        let tes1 = typecheck env e1 in
        (* check (Type.Tuple(List.map snd xts)) (extract_type tes1);
            fare una funzione che fa lo zip tra xs e xts e le aggiunge
            al contesto per tipare e2
          *)
        let (Type.Tuple(ts)) = extract_type tes1 in
        (try
          let  zts = List.combine xs ts in
          let nenv = List.fold_left (fun env (x,t) -> M.add x t env) env zts in
          let tes2 =  typecheck nenv e2 in
          LetTuple(xs, tes1, tes2, (annot, extract_type tes2))
        with Invalid_argument _ ->
              failwith "Different arity in pattern matching for tuple")
      | Array(e1, e2, annot) ->
        let tes1 = typecheck env e1 in
        let tes2 = typecheck env e2 in
        check (extract_type tes1) Type.Int;
        Array(tes1, tes2, (annot, Type.Array(extract_type tes2)))
      | Get(e1, e2, annot) ->
        (* TODO: improve this one *)
        let tes1 = typecheck env e1 in
        let tes2 = typecheck env e2 in
        let (Type.Array(t)) =  extract_type tes1 in
        check Type.Int (extract_type tes2);
        Get(tes1, tes2, (annot, t))
      | Put(e1, e2, e3, annot) ->
        let tes1 = typecheck env e1 in
        let tes2 = typecheck env e2 in
        let tes3 = typecheck env e3 in
        check (Type.Array(extract_type tes3)) (extract_type tes1);
        check Type.Int (extract_type tes2);
        Put(tes1,tes2,tes3, (annot,  Type.Unit))
end