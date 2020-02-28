open Batteries
open LanguageSpecification

module TypeAlgorithm (L : LanguageSpecification) =
struct
  exception TypeError of ((int * VarSet.t) L.term)

  let rec typing gamma t =
    let ts = L.get_rev_children t in
    let rs = List.fold_left (fun rs tch -> let (i, ti) = tch in (typing (L.tr ti t gamma rs) ti)::rs) [] ts in
      match L.checkjoin t gamma rs with
      | None -> raise (TypeError t)
      | Some final_res -> final_res
end
