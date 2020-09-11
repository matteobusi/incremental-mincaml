type event = Hit | Orig | MissNone | MissIncompatible

module type Report = sig
    type 'a term

    val create : 'a term -> Report
    val register : event -> ('a term) list -> Report

    (* Pretty printing utilities *)
    val string_of_report : Report -> string
end
