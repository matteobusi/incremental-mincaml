open Core
open LanguageSpecification

module TypeAlgorithm (L : LanguageSpecification) =
struct
  let rec typing (gamma : L.context) (t : 'a L.term) : L.res =
    let ts = L.get_sorted_children t in
    let rs = List.fold_left ts ~init:[] ~f:(fun rs (i, ti) -> rs@[typing (L.tr i ti t gamma rs) ti]) in
      (match L.checkjoin t gamma rs with
        | None ->
            Printf.printf "\ntyping failed at: %s\n" (L.string_of_term (fun _ _ -> ()) t);
            List.iter ts ~f:(fun t -> Printf.printf "child: %s\n" (L.string_of_term (fun _ _ -> ()) (snd t)));
            List.iter rs ~f:(fun r -> Printf.printf "partial res: %s\n" (L.string_of_type r));
            failwith "(original.ml) Error: CheckJoin failed!"
        | Some final_res -> final_res)


  let rec term_map (fn : 'a L.term -> 'b) (t : 'a L.term) : 'b L.term =
    let ts = List.map (L.get_sorted_children t) ~f:snd in
    let ts' = List.map ts ~f:(fun ti -> term_map fn ti) in
      L.term_edit t ts' (fn t)
end
