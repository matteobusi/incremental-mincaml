(* Sets of variables *)
open Core

include Set.Make
    (struct
      type t = string
      [@@deriving compare,sexp]
    end)
