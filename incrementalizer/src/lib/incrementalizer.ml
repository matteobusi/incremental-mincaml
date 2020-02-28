open Batteries

open LanguageSpecification
open Original

(* This is just a POC to show that the process is indeed mechanizable, with no focus on performance! *)
module TypeAlgorithm (L : LanguageSpecification) =
struct
    let get_empty_cache = Hashtbl.create 100

    let rec build_cache at gamma c =
        let ((hash, fvs), res) = L.get_annot at in
        let child_cache = List.iter (fun ti -> build_cache ti gamma c) (L.get_rev_children at) in
            Hashtbl.add child_cache hash (gamma, res)

    let rec incremental_typing c gamma at =
        let (hash, fvs) = L.get_annot at in
        let ti_list = L.get_rev_children at in
            match ti_list with
            | [] -> (* Call the original algorithm and update the cache *)
                let ta_typed = Original.TypeAlgorithm(L).typing gamma at in
                let (_, res) = L.term.get_annot ta_typed in
                    add c hash (gamma, res); ta_typed
            | _ -> raise "Error" (* Make a copy of c to be faithful to the formal description!
                let ti_res_list = fold_right (fun ti res_list -> (incremental_typing c (L.tr ta ti gamma res_list) ti)::res_list) [] ti_list in
                    L.checkjoin at gamma ti_res_list *)
end
