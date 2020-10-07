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
        type node_visit_type = Hit | Orig | MissNone | MissIncompatible | NoVisit

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

        let string_of_report r = Printf.sprintf
            "[Visited: %d/%d] O: %d - H: %d - M: %d (I) + %d (NF) = %d"
                (r.cache_miss_inc+r.cache_miss_none+r.cache_hit+r.orig_call)
                r.nc
                r.orig_call
                r.cache_hit
                r.cache_miss_inc
                r.cache_miss_none
                (r.cache_miss_inc+r.cache_miss_none)
    end


    let report = IncrementalReport.create


    let get_empty_cache ?(size=4096) () : (L.context ref * L.res) Cache.t = Cache.create ~growth_allowed:false ~size:size ()


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


    let typing ?threshold (cache : (L.context ref * L.res) Cache.t) gamma t =
        let compatcall_cnt = ref 0 in
        let rec _typing (threshold_fn : (unit -> bool) -> (unit -> L.res) -> (unit -> L.res) -> L.res) (cache : (L.context ref * L.res) Cache.t) gamma t =
            let miss (cache : (L.context ref * L.res) Cache.t) hash gamma =
                (match Cache.find cache hash with
                | None -> None (* This is a miss, w/o any corresponding element in cache *)
                | Some (gamma', res') ->
                    incr compatcall_cnt;
                    if L.compat !gamma !gamma' t then
                        Some res' (* This is a hit! *)
                    else
                        None) in
            let (hash, fvs) = L.term_getannot t in
            let orig_call () = (let r = OriginalFunAlgorithm.typing gamma t in Cache.set cache hash (ref gamma, r); r) in
            let inc_call () =
                (let ts = L.get_sorted_children t in
                    match ts with
                    | [] -> orig_call () (* Call the original algorithm and update the cache *)
                    | _ -> (* This is the inductive case, either a hit or a miss *)
                        (match miss cache hash (ref gamma) with
                        | None -> (* This is a miss *)
                            (
                            (* Here's the difference w. paper:
                                copy the cache and update it only in the end to be faithful to the paper! *)
                            let rs = List.fold_left ts ~init:[] ~f:(fun rs (i, ti) -> rs@[_typing threshold_fn cache (L.tr i ti t gamma rs) ti]) in
                                (match L.checkjoin t gamma rs with
                                | None ->
                                    List.iter ts ~f:(fun t -> Printf.printf "child (%d) - %s \n" (fst t) (L.string_of_term (fun _ _ -> ()) (snd t)));
                                    List.iter rs ~f:(fun r -> Printf.printf "child res: %s\n" (L.string_of_type r));
                                    failwith "(incrementalizer.ml) Error: incremental CheckJoin failed!"
                                | Some res ->
                                    Cache.set cache hash (ref gamma, res); res))
                        | Some res -> res)) in
            threshold_fn (fun () -> !compatcall_cnt >= Option.value_exn threshold) orig_call inc_call
        in
        match threshold with
            | Some thresh -> _typing (fun c e1 e2 -> if c () then e1 () else e2 ()) cache gamma t
            | None -> _typing (fun _ _ e2 -> e2 ()) cache gamma t

    let typing_w_report nc ?threshold (cache : (L.context ref * L.res) Cache.t) gamma t =
        let rec _typing (threshold_fn : (unit -> bool) -> (unit -> L.res) -> (unit -> L.res) -> L.res) (cache : (L.context ref * L.res) Cache.t) gamma (t : (int * VarSet.t) L.term) (at : (IncrementalReport.node_visit_type ref) L.term) =
            let miss (cache : (L.context ref * L.res) Cache.t) hash gamma annot_at =
                (match Cache.find cache hash with
                | None ->
                    annot_at := IncrementalReport.MissNone; IncrementalReport.register_miss_none report; None (* This is a miss, w/o any corresponding element in cache *)
                | Some (gamma', res') ->
                    if L.compat !gamma !gamma' t then
                        (annot_at := IncrementalReport.Hit; IncrementalReport.register_hit report; Some res') (* This is a hit! *)
                    else
                        (annot_at := IncrementalReport.MissIncompatible; IncrementalReport.register_miss_incomp report; None)) in
            let ts = L.get_sorted_children t in
            let (hash, fvs) = L.term_getannot t in
            let orig_call () = (let r = OriginalFunAlgorithm.typing gamma t in
                    IncrementalReport.register_orig_call report;
                    (L.term_getannot at) := IncrementalReport.Orig;
                    (if hash = 119809083 then
                        let s = L.string_of_term (fun _ _ -> ()) at in
                        Printf.eprintf "AO: %s\n" (String.slice s 0 (Int.min 80 (String.length s)));
                        Out_channel.flush stderr
                    else ());
                    Cache.set cache hash (ref gamma, r); r)
                in
            let inc_call () =
                    (match ts with
                    | [] -> orig_call () (* Call the original algorithm and update the cache *)
                    | _ -> (* This is the inductive case, either a hit or a miss *)
                        (match miss cache hash (ref gamma) (L.term_getannot at) with
                        | None -> (* This is a miss *)
                            (
                            (* Here's the difference w. paper:
                                copy the cache and update it only in the end to be faithful to the paper! *)
                            let ats = L.get_sorted_children at in
                            let cats = List.zip_exn ts ats in
                            let rs = List.fold_left cats ~init:[] ~f:(fun rs ((i, ti), (_, ati)) -> rs@[_typing threshold_fn cache (L.tr i ti t gamma rs) ti ati]) in
                                (match L.checkjoin t gamma rs with
                                | None ->
                                    List.iter ts ~f:(fun t -> Printf.printf "child (%d) - %s \n" (fst t) (L.string_of_term (fun _ _ -> ()) (snd t)));
                                    List.iter rs ~f:(fun r -> Printf.printf "child res: %s\n" (L.string_of_type r));
                                    failwith "Incremental CheckJoin failed!"
                                | Some res ->
                                    (if hash = 119809083 then
                                        let s = L.string_of_term (fun _ _ -> ()) at in
                                        Printf.eprintf "ACJ: %s\n" (String.slice s 0 (Int.min 80 (String.length s)));
                                        Out_channel.flush stderr
                                    else ());
                                    Cache.set cache hash (ref gamma, res); res))
                        | Some res ->
                            res)) in
            threshold_fn
                (fun () -> report.cache_miss_inc + report.cache_hit >= Option.value_exn threshold)
                orig_call
                inc_call
        in
            IncrementalReport.reset report;
            IncrementalReport.set_nc nc report;
            let annot_t = OriginalFunAlgorithm.term_map (fun ct -> ref IncrementalReport.NoVisit) t in
            let res = (match threshold with
                | Some thresh -> _typing (fun c e1 e2 -> if c () then e1 () else e2 ()) cache gamma t annot_t
                | None -> _typing (fun _ _ e2 -> e2 ()) cache gamma t annot_t) in
                report.annot_t <- Some annot_t;
                Printf.eprintf "Threshold: %d - Compat calls: %d\n" (Option.value ~default:(-1) threshold) (report.cache_miss_inc + report.cache_hit);
                Out_channel.flush stderr;
                res
end
