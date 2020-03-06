module type LanguageSpecification = sig
    (* Define the syntax of the language and some meta-functions *)
    type 'a term

    val term_getannot : 'a term -> 'a
    val term_edit : 'a term -> ('b term) list -> 'b -> 'b term
    val compute_fv : 'a term -> VarSet.t
    val compute_hash : 'a term -> int
    val get_sorted_children : 'a term -> (int * 'a term) list

    (* Then, the basic ingredients for the typing *)
    type context
    type res

    val compat      : context -> context -> (int * VarSet.t) term -> bool
    val tr          : int -> (int * VarSet.t) term -> (int * VarSet.t) term -> context -> res list -> context
    val checkjoin   : (int * VarSet.t) term -> context -> res list -> res option

    (* Pretty printing utilities *)
    val string_of_term : (Format.formatter -> 'a -> unit) -> 'a term -> string
    val string_of_type : res -> string
end
