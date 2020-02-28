module type LanguageSpecification = sig
    (* Define the syntax of the language and some meta-functions *)
    type 'a term

    val get_annot : 'a term -> 'a
    val compute_fv : 'a term -> VarSet.t
    val compute_hash : 'a term -> int


    (* Then, the basic ingredients for the typing *)
    type context
    type res

    val get_rev_children : 'a term -> (int * 'a term) list
    val compat      : context -> context -> (int * VarSet.t) term -> bool
    val tr          : int -> (int * VarSet.t) term -> (int * VarSet.t) term -> context -> res list -> context
    val checkjoin   : (int * VarSet.t) term -> context -> res list -> res option
end
