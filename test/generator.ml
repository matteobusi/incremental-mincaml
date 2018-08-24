open Annotast

(*
  Generate a balanced aAST of the specified depth (0 means just the root) whose nodes are ibop and leaves are all identical to id
*)
let rec gen_ibop_id_ast h ibop id = 
  match h with
  |  0 -> Annotast.Var(id, Hashing.compute_hash id)
  |  _ ->
    let laast = gen_ibop_id_ast (h-1) ibop id in 
    let raast = gen_ibop_id_ast (h-1) ibop id in
    let hash = Hashing.combine_hashes [ Hashing.compute_hash ibop; Hashing.extract_simple_hash laast; Hashing.extract_simple_hash raast] in
    Annotast.IBop(ibop, laast, raast, hash)

(*
  Generate a balanced aAST of the specified depth (0 means just the root) whose nodes are ibop and leaves are taken from ids
*)
let gen_ibop_ids_ast h ibop id = 
  let counter = ref 0 in
  let rec aux h ibop id = 
    match h with
    |  1 -> incr counter; let curr_id = id ^ (string_of_int !counter) in Annotast.Var(curr_id, Hashing.compute_hash id)
    |  _ ->
      let laast = aux (h-1) ibop id in 
      let raast = aux (h-1) ibop id in
      let hash = Hashing.combine_hashes [ Hashing.compute_hash ibop; Hashing.extract_simple_hash laast; Hashing.extract_simple_hash raast] in
      Annotast.IBop(ibop, laast, raast, hash) in 
  aux h ibop id