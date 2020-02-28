(* Sets of variables *)
open Batteries

include Set.Make
    (struct
      type t = string
      let compare = String.compare
    end)
