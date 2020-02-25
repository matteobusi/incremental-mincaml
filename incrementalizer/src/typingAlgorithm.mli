type term = (int * VarSet.t) Term.t
type context
type res

(* Required information for the context *)
val compat      : context -> context -> term -> bool
val tr          : term -> term -> context -> res list -> context
val checkjoin   : term -> context -> res list -> res

(* Just for simplicity, this **must** use the three functions above! *)
val typing   : context -> term -> res
