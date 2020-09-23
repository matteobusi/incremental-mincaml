open Core

open LanguageSpecification
open Original

(* This is just a POC to show that the process is indeed mechanizable *)
module TypeAlgorithm (L : LanguageSpecification) =
struct
    module OriginalFunAlgorithm = Original.TypeAlgorithm(L)
    module Cache = Hashtbl.Make(struct
            type t = int [@@deriving compare,hash,sexp]
          end)

    (* Reporting facilities, this is unneeded in a real-world incrementalizer *)
    module IncrementalReport = struct
        type node_visit_type = Hit | Orig | Miss | NoVisit

        type report_data = {
            mutable cache_miss_inc : int;
            mutable cache_miss_none : int;
            mutable cache_hit : int;
            mutable nc : int;
            mutable orig_call : int;
            mutable annot_t : ((node_visit_type ref) L.term) option}

        let create = {
            cache_hit = 0;
            cache_miss_inc = 0;
            cache_miss_none = 0;
            nc = 0;
            orig_call = 0;
            annot_t = None }

        let reset r =
            r.cache_hit <- 0;
            r.cache_miss_inc <- 0;
            r.cache_miss_none <- 0;
            r.nc <- 0;
            r.orig_call <- 0;
            r.annot_t <- None

        let set_nc nc r = r.nc <- nc

        let register_hit r = r.cache_hit <- r.cache_hit + 1

        let register_miss_incomp r = r.cache_miss_inc <- r.cache_miss_inc + 1

        let register_miss_none r = r.cache_miss_none <- r.cache_miss_none + 1

        let register_orig_call r = r.orig_call <- r.orig_call + 1

        let string_of_report r = Printf.sprintf "[Visited: %d/%d] O: %d - H: %d - M: %d (I) + %d (NF) = %d" (r.cache_miss_inc+r.cache_miss_none+r.cache_hit+r.orig_call) r.nc r.orig_call r.cache_hit r.cache_miss_inc r.cache_miss_none (r.cache_miss_inc+r.cache_miss_none)
    end

    let report = IncrementalReport.create

    let get_empty_cache sz : (L.context ref * L.res) Cache.t = Cache.create sz

    let build_cache t gamma (cache : (L.context ref * L.res) Cache.t) =
        (* Same Original.TypeAlgorithm.typing but returns an aAST *)
        let rec compute_aast gamma t =
            let ts = L.get_sorted_children t in
            let ats = List.fold_left ts ~init:[] ~f:(fun rs (i, ti) -> rs@[compute_aast (L.tr i ti t gamma (List.map rs ~f:(Fn.compose fst L.term_getannot))) ti]) in
            (match L.checkjoin t gamma (List.map ats ~f:(Fn.compose fst L.term_getannot)) with
            | None ->
                Printf.printf "\nFailed typing at: %s\n" (L.string_of_term (fun _ _ -> ()) t);
                List.iter ts ~f:(fun t -> Printf.printf "child %d: %s\n" (fst t) (L.string_of_term (fun _ _ -> ()) (snd t)));
                Printf.printf "context: %s \n\n" (L.string_of_context gamma);
                failwith "(incrementalizer.ml, l.43) buildcache: CheckJoin failed!"
            | Some r -> L.term_edit t ats (r, L.term_getannot t)) in
        let rec _build_cache (t : (L.res * (int * VarSet.t)) L.term) (t_hf : (int * VarSet.t) L.term) gamma  (cache : (L.context ref * L.res) Cache.t) =
            let (res, (hash, fvs)) = L.term_getannot t in
            let ts = (L.get_sorted_children t) in
            let ts_hf = (L.get_sorted_children t_hf) in
            let rs = List.map ts ~f:(Fn.compose fst (Fn.compose L.term_getannot snd)) in
                Cache.set cache hash (ref gamma, res);
                List.iter (List.zip_exn ts ts_hf) ~f:(fun ((i, ti), (_, ti_hf)) -> _build_cache ti ti_hf (L.tr i ti_hf t_hf gamma (List.take rs i)) cache) in
        let aast = compute_aast gamma t in
            _build_cache aast t gamma cache; aast

    let typing ?(threshold=Int.max_value) (cache : (L.context ref * L.res) Cache.t) gamma t =
        let incompat_cnt = ref 0 in
        let rec _typing (cache : (L.context ref * L.res) Cache.t) gamma t =
            (let miss (cache : (L.context ref * L.res) Cache.t) hash gamma =
                (match Cache.find cache hash with
                | None -> None (* This is a miss, w/o any corresponding element in cache *)
                | Some (gamma', res') ->
                    incr incompat_cnt;
                    (* TODO: possible optimization: check whether gamma and gamma' refer to the same location -- this requires changing cache update implementation *)
                    if L.compat !gamma !gamma' t then
                        Some res' (* This is a hit! *)
                    else
                        None
                ) in
            let (hash, fvs) = L.term_getannot t in
            let orig_call () = (let r = OriginalFunAlgorithm.typing gamma t in Cache.set cache hash (ref gamma, r); r) in
            if !incompat_cnt > threshold then orig_call ()
            else
                (let ts = L.get_sorted_children t in
                    match ts with
                    | [] -> orig_call () (* Call the original algorithm and update the cache *)
                    | _ -> (* This is the inductive case, either a hit or a miss *)
                        (match miss cache hash (ref gamma) with
                        | None -> (* This is a miss *)
                            (
                            (* Here's the difference w. paper:
                                copy the cache and update it only in the end to be faithful to the paper! *)
                            let rs = List.fold_left ts ~init:[] ~f:(fun rs (i, ti) -> rs@[_typing cache (L.tr i ti t gamma rs) ti]) in
                                (match L.checkjoin t gamma rs with
                                | None ->
                                    List.iter ts ~f:(fun t -> Printf.printf "child (%d) - %s \n" (fst t) (L.string_of_term (fun _ _ -> ()) (snd t)));
                                    List.iter rs ~f:(fun r -> Printf.printf "child res: %s\n" (L.string_of_type r));
                                    failwith "Incremental CheckJoin failed!"
                                | Some res -> Cache.set cache hash (ref gamma, res); res))
                        | Some res -> res)))
        in _typing cache gamma t

    let typing_w_report ?(threshold=Int.max_value) nc (cache : (L.context ref * L.res) Cache.t) gamma t =
        let rec _typing (cache : (L.context ref * L.res) Cache.t) gamma (t : (int * VarSet.t) L.term) (at : (IncrementalReport.node_visit_type ref) L.term) =
            let miss (cache : (L.context ref * L.res) Cache.t) hash gamma =
                (match Cache.find cache hash with
                | None ->
                    IncrementalReport.register_miss_none report; None (* This is a miss, w/o any corresponding element in cache *)
                | Some (gamma', res') ->
                    if L.compat !gamma !gamma' t then
                        (IncrementalReport.register_hit report; Some res') (* This is a hit! *)
                    else
                        (IncrementalReport.register_miss_incomp report; None)) in
            let ts = L.get_sorted_children t in
            let (hash, fvs) = L.term_getannot t in
            let orig_call () =
                (let r = OriginalFunAlgorithm.typing gamma t in
                    IncrementalReport.register_orig_call report;
                    (L.term_getannot at) := IncrementalReport.Orig;
                    Cache.set cache hash (ref gamma, r); r)
                in
                if report.cache_miss_inc + report.cache_miss_none > threshold then (Printf.eprintf "orig: %d %d\n" report.cache_miss_none report.cache_miss_inc; flush stderr; orig_call ())
                else
                    (match ts with
                    | [] -> orig_call () (* Call the original algorithm and update the cache *)
                    | _ -> (* This is the inductive case, either a hit or a miss *)
                        (match miss cache hash (ref gamma) with
                        | None -> (* This is a miss *)
                            (
                            (* Here's the difference w. paper:
                                copy the cache and update it only in the end to be faithful to the paper! *)
                            let ats = L.get_sorted_children at in
                            let cats = List.zip_exn ts ats in
                            L.term_getannot at := IncrementalReport.Miss;
                            let rs = List.fold_left cats ~init:[] ~f:(fun rs ((i, ti), (_, ati)) -> rs@[_typing cache (L.tr i ti t gamma rs) ti ati]) in
                                (match L.checkjoin t gamma rs with
                                | None ->
                                    List.iter ts ~f:(fun t -> Printf.printf "child (%d) - %s \n" (fst t) (L.string_of_term (fun _ _ -> ()) (snd t)));
                                    List.iter rs ~f:(fun r -> Printf.printf "child res: %s\n" (L.string_of_type r));
                                    failwith "Incremental CheckJoin failed!"
                                | Some res -> Cache.set cache hash (ref gamma, res); res))
                        | Some res ->
                            L.term_getannot at := IncrementalReport.Hit;
                            res))
        in
            IncrementalReport.reset report;
            IncrementalReport.set_nc nc report;
            let annot_t = OriginalFunAlgorithm.term_map (fun ct -> ref IncrementalReport.NoVisit) t in
            let res = _typing cache gamma t annot_t in
                report.annot_t <- Some annot_t; res
end
