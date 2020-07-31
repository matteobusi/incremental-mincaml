open Batteries
open LanguageSpecification

module TypeAlgorithm (L : LanguageSpecification) =
struct
  let rec typing gamma t =
    let ts = L.get_sorted_children t in
    let rs = List.fold_left (fun rs (i, ti) -> rs@[typing (L.tr i ti t gamma rs) ti]) [] ts in
    (match L.checkjoin t gamma rs with
      | None ->
        Printf.printf "\ntyping failed at: %s\n" (L.string_of_term (fun _ _ -> ()) t);
        List.iter (fun t -> Printf.printf "child: %s\n" (L.string_of_term (fun _ _ -> ()) (snd t))) ts;
        List.iter (fun r -> Printf.printf "partial res: %s\n" (L.string_of_type r)) rs;
        failwith "Original: CheckJoin failed!"
      | Some final_res -> final_res)


  let rec term_map (fn : 'a L.term -> 'b) (t : 'a L.term) : 'b L.term =
    let ts = List.map snd (L.get_sorted_children t) in
    let ts' = List.map (fun ti -> term_map fn ti) ts in
      L.term_edit t ts' (fn t)
end
