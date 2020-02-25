type 'a t (* This defines the aAST of the language in use *)

val map : 'a t -> ('a -> 'b) -> 'b t

(* Must be in **REVERSED** order for typing *)
val get_ordered_children : 'a t -> ('a t) list

val get_annot : 'a t -> 'a
val compute_fv : 'a t -> VarSet.t
val compute_hash : 'a t -> int
