open Batteries

open LanguageSpecification
open Original

(* This is just a POC to show that the process is indeed mechanizable *)
module TypeAlgorithm (L : LanguageSpecification) =
struct
    module OriginalFunAlgorithm = Original.TypeAlgorithm(L)

    let get_empty_cache : (int, L.context*L.res) Hashtbl.t = Hashtbl.create 100

    let build_cache t gamma cache =
        (* Same Original.TypeAlgorithm.typing but returns an aAST *)
        let rec compute_aast gamma t =
            let ts = L.get_sorted_children t in
            let ats = List.fold_left (fun rs (i, ti) -> rs@[compute_aast (L.tr i ti t gamma (List.map (fst%L.term_getannot) rs)) ti]) [] ts in
            (match L.checkjoin t gamma (List.map (fst%L.term_getannot) ats) with
            | None ->
                Printf.printf "\ntyping failed at: %s\n" (L.string_of_term (fun _ _ -> ()) t);
                List.iter (fun t -> Printf.printf "child: %s\n" (L.string_of_term (fun _ _ -> ()) (snd t))) ts;
                failwith "Buildcache: CheckJoin failed!"
            | Some r -> L.term_edit t ats (r, L.term_getannot t)) in
        let rec _build_cache (t : (L.res * (int * VarSet.t)) L.term) (t_hf : (int * VarSet.t) L.term) gamma cache =
            let (res, (hash, fvs)) = L.term_getannot t in
            let ts = (L.get_sorted_children t) in
            let ts_hf = (L.get_sorted_children t_hf) in
            let rs = List.map (fst%L.term_getannot%snd) ts in
                Hashtbl.add cache hash (gamma, res);
                List.iter (fun ((i, ti), (_, ti_hf)) -> _build_cache ti ti_hf (L.tr i ti_hf t_hf gamma (List.take i rs)) cache) (List.combine ts ts_hf) in
        _build_cache (compute_aast gamma t) t gamma cache

    let rec typing c gamma t =
        let miss c t gamma =
            let (hash, fc) = L.term_getannot t in
            match Hashtbl.find_option c hash with
            | None -> None
            | Some (gamma', res') -> if L.compat gamma gamma' t then Some res' else None in
        let (hash, fvs) = L.term_getannot t in
        let ts = L.get_sorted_children t in
            match ts with
            | [] -> (* Call the original algorithm and update the cache *)
                let r = OriginalFunAlgorithm.typing gamma t in
                    Hashtbl.replace c hash (gamma, r); r
            | _ -> (* This is the inductive case, either a hit or a miss *)
                match miss c t gamma with
                | None -> (* This is a miss *)
                    begin
                    (* TODO: copy the cache and update it only in the end! *)
                    let rs = List.fold_left (fun rs (i, ti) -> rs@[typing c (L.tr i ti t gamma rs) ti]) [] ts in
                        (match L.checkjoin t gamma rs with
                        | None ->
                            Printf.printf "\nincr. typing failed at: %s\n" (L.string_of_term (fun _ _ -> ()) t);
                            List.iter (fun t -> Printf.printf "child: %s\n" (L.string_of_term (fun _ _ -> ()) (snd t))) ts;
                            List.iter (fun r -> Printf.printf "partial res: %s\n" (L.string_of_type r)) rs;
                            failwith "Incremental CheckJoin failed!"
                        | Some res -> Hashtbl.replace c hash (gamma, res); res)
                    end
                | Some res -> res
end
