(* Sets of variables *)
open Batteries

module VarSet =
struct
  include Set.Make 
      (struct
        type t = Id.t
        let compare = String.compare
      end)
end