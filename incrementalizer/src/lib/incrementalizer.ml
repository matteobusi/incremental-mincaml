open Batteries

open LanguageSpecification
open Original

(* This is just a POC to show that the process is indeed mechanizable *)
module TypeAlgorithm (L : LanguageSpecification) =
struct
    module OriginalFunAlgorithm = Original.TypeAlgorithm(L)

    let get_empty_cache = Hashtbl.create 100

    let miss c t gamma = None

    let rec build_cache at gamma c =
        let ((hash, fvs), res) = L.get_annot at in
        List.iter (fun (i, ti) -> build_cache ti gamma c) (L.get_sorted_children at);
        Hashtbl.add c hash (gamma, res)

    let rec typing c gamma at =
        let (hash, fvs) = L.get_annot at in
        let ts = L.get_sorted_children at in
            match ts with
            | [] -> (* Call the original algorithm and update the cache *)
                let r = OriginalFunAlgorithm.typing gamma at in
                    Hashtbl.replace c hash (gamma, r); r
            | _ -> (* This is the inductive case, either a hit or a miss *)
                match miss c at gamma with
                | None -> (* This is a miss *)
                    begin
                    let rs = List.fold_left (fun rs (i, ti) -> rs@(typing c (L.tr i ti at gamma rs) ti)) [] ts in
                        (match L.checkjoin at gamma rs with
                        | None ->
                            Printf.printf "\nincr. typing failed at: %s\n" (L.string_of_term (fun _ _ -> ()) t);
                            List.iter (fun t -> Printf.printf "child: %s\n" (L.string_of_term (fun _ _ -> ()) (snd t))) ts;
                            List.iter (fun r -> Printf.printf "partial res: %s\n" (L.string_of_type r)) rs;
                            failwith "CheckJoin failed!"
                        | Some r -> Hashtbl.replace c hash (gamma, r); r)
                    end
                | Some res -> res
end
