open Batteries

(* This is just a POC to show that the process is indeed mechanizable, with no focus on performance! *)
module IncrementalTypingAlgorithm (A : TypingAlgorithm) =
struct
    type cache = (int, (A.env * A.res)) Hashtbl.t

    let get_empty_cache = Hashtbl.create 100

    let rec build_cache at gamma c =
        let ((hash, fvs), res) = A.term.get_annot at in
        let child_cache = List.iter (fun ti -> build_cache ti gamma c) (A.term.get_ordered_children at) in
            add child_cache hash (gamma, res)

    let rec incremental_typing c gamma at =
        let (hash, fvs) = A.term.get_annot at in
        let ti_list = A.term.get_ordered_children at in
            match ti_list with
            | [] ->
                (* Call the original algorithm and update the cache *)
                let ta_typed = A.typing gamma at in
                let (_, res) = A.term.get_annot ta_typed in
                    add c hash (gamma, res); ta_typed
            | _ -> (* Make a copy of c to be faithful to the formal description! *)
                let ti_res_list = fold_right (fun ti res_list -> (incremental_typing c (A.tr ta ti gamma res_list) ti)::res_list) [] ti_list in
                    A.checkjoin at gamma ti_res_list

end
