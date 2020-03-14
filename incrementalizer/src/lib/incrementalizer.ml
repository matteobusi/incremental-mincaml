open Batteries

open LanguageSpecification
open Original

(* This is just a POC to show that the process is indeed mechanizable *)
(* @@inline never *)
module TypeAlgorithm (L : LanguageSpecification) =
struct
    module OriginalFunAlgorithm = Original.TypeAlgorithm(L)
    module Cache = Hashtbl.Make(struct
            type t = int
            let equal i j = i=j
            let hash i = i land max_int
          end)

    (* Reporting facilities, this is unneeded in a real-world incrementalizer *)
    module IncrementalReport = struct
        type report_data = { mutable cache_miss_inc : int; mutable cache_miss_none : int; mutable cache_hit : int; mutable nc : int}

        let create () = { cache_hit = 0; cache_miss_inc = 0; cache_miss_none = 0; nc = 0}
        let reset r = r.cache_hit <- 0; r.cache_miss_inc <- 0; r.cache_miss_none <- 0; r.nc <- 0
        let set_nc nc r = r.nc <- nc
        let register_hit r = r.cache_hit <- r.cache_hit + 1
        let register_miss_incomp r = r.cache_miss_inc <- r.cache_miss_inc + 1
        let register_miss_none r = r.cache_miss_none <- r.cache_miss_none + 1
        let string_of_report r = Printf.sprintf "[Visited: %d/%d] H: %d - M: %d (I) + %d (NF) = %d" (r.cache_miss_inc+r.cache_miss_none+r.cache_hit) r.nc r.cache_hit r.cache_miss_inc r.cache_miss_none (r.cache_miss_inc+r.cache_miss_none)
    end
    let report = IncrementalReport.create ()

    let get_empty_cache sz : (int, L.context*L.res) Cache.t = Cache.create sz

    let build_cache t gamma cache =
        (* Same Original.TypeAlgorithm.typing but returns an aAST *)
        let rec compute_aast gamma t =
            let ts = L.get_sorted_children t in
            let ats = List.fold_left (fun rs (i, ti) -> rs@[compute_aast (L.tr i ti t gamma (List.map (fst%L.term_getannot) rs)) ti]) [] ts in
            (match L.checkjoin t gamma (List.map (fst%L.term_getannot) ats) with
            | None ->
                Printf.printf "\nFailed typing at: %s\n" (L.string_of_term (fun _ _ -> ()) t);
                List.iter (fun t -> Printf.printf "child %d: %s\n" (fst t) (L.string_of_term (fun _ _ -> ()) (snd t))) ts;
                Printf.printf "context: %s \n\n" (L.string_of_context gamma);
                failwith "Buildcache: CheckJoin failed!"
            | Some r -> L.term_edit t ats (r, L.term_getannot t)) in
        let rec _build_cache (t : (L.res * (int * VarSet.t)) L.term) (t_hf : (int * VarSet.t) L.term) gamma cache =
            let (res, (hash, fvs)) = L.term_getannot t in
            let ts = (L.get_sorted_children t) in
            let ts_hf = (L.get_sorted_children t_hf) in
            let rs = List.map (fst%L.term_getannot%snd) ts in
                Cache.add cache hash (gamma, res);
                List.iter (fun ((i, ti), (_, ti_hf)) -> _build_cache ti ti_hf (L.tr i ti_hf t_hf gamma (List.take i rs)) cache) (List.combine ts ts_hf) in
        let aast = compute_aast gamma t in
            _build_cache aast t gamma cache; aast

    let rec typing c gamma t ?(wcache=c)=
        let miss c hash gamma =
            (match Cache.find_option c hash with
            | None -> IncrementalReport.register_miss_none report; None (* This is a miss, w/o any corresponding element in cache *)
            | Some (gamma', res') ->
                if L.compat gamma gamma' t then
                    (IncrementalReport.register_hit report; Some res') (* This is a hit! *)
                else
                    (IncrementalReport.register_miss_incomp report; None)) in
        let (hash, fvs) = L.term_getannot t in
        let ts = L.get_sorted_children t in
            match ts with
            | [] -> (* Call the original algorithm and update the cache *)
                (let r = OriginalFunAlgorithm.typing gamma t in
                    Cache.replace wcache hash (gamma, r); r)
            | _ -> (* This is the inductive case, either a hit or a miss *)
                (match miss c hash gamma with
                | None -> (* This is a miss *)
                    (
                    (*
                        TODO: copy the cache and update it only in the end to be faithful to the paper!
                    *)
                    let rs = List.fold_left (fun rs (i, ti) -> rs@[typing c (L.tr i ti t gamma rs) ti ~wcache:wcache]) [] ts in
                        (match L.checkjoin t gamma rs with
                        | None ->
                            Printf.printf "\nincr. typing failed at: %s\n" (L.string_of_term (fun _ _ -> ()) t);
                            List.iter (fun t -> Printf.printf "child (%d) - %s \n" (fst t) (L.string_of_term (fun _ _ -> ()) (snd t))) ts;
                            List.iter (fun r -> Printf.printf "child res: %s\n" (L.string_of_type r)) rs;
                            failwith "Incremental CheckJoin failed!"
                        | Some res -> Cache.replace wcache hash (gamma, res); res))
                | Some res -> res)
end
