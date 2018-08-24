(* Sets of variables *)
open Batteries

module VarSet =
struct
  include Set.Make 
      (struct
        type t = Id.t
        let compare = compare
      end)
end