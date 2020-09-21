(** A module internal to [Core_bench]. Please look at {!Bench}.

    A [Test.t] represents a user specified benchmark. *)
open Core

module L = FunSpecification.FunSpecification

module OriginalFunAlgorithm = Original.TypeAlgorithm(L)
module IncrementalFunAlgorithm = Incrementalizer.TypeAlgorithm(L)

module Id : Unique_id.Id = Unique_id.Int()
module Basic_test = struct
  type packed_f = T : ([`init] -> (((L.context ref * L.res) IncrementalFunAlgorithm.Cache.t) -> 'b)) -> packed_f
  type setup_t = S : (unit -> ((L.context ref * L.res) IncrementalFunAlgorithm.Cache.t)) -> setup_t
  type t = {
    test_id     : Id.t;
    name        : string;
    test_name   : string;
    file_name   : string;
    module_name : string;
    key         : int;
    arg         : int option;
    group_key   : int option;
    f           : packed_f;
    setup       : setup_t;
  } [@@deriving fields]

  let create_with_initialization ~name ?(test_name="") ?(file_name="") ?(module_name="") ?(group_key=None) ?(arg=None) ~key setup f =
    { name; test_name; module_name; file_name; f = T f; setup = S setup; key; group_key; arg; test_id = Id.create () }

  let make_filename t =
    let name = String.tr ~target:' ' ~replacement:'-' t.name in
    name ^ ".txt"

end

type t = {
  name        : string;
  test_name   : string;
  file_name   : string;
  module_name : string;
  tests       : Basic_test.t list
} [@@deriving fields]

let create_with_initialization ~name ?(test_name="") ?(file_name="") ?(module_name="") ?(key=0) setup bm = {
  name;
  test_name;
  module_name;
  file_name;
  tests = [Basic_test.create_with_initialization ~name ~test_name ~module_name ~file_name ~key setup bm];
}

let create ~name ?test_name ?file_name ?module_name ?key setup bm =
  create_with_initialization ~name ?test_name ?file_name ?module_name ?key setup (fun `init -> bm)

let create_indexed ~name ?(test_name="") ?(file_name="") ?(module_name="") ~args ?(key=0) setup bm = {
  name;
  test_name;
  module_name;
  file_name;
  tests = List.map args ~f:(fun n ->
    let individual_key = Hashtbl.hash (key + n) in
    let name = name ^ ":" ^ (Int.to_string n) in
    { Basic_test.
      name;
      test_name;
      module_name;
      file_name;
      arg = Some n;
      key = individual_key;
      group_key = Some key;
      f = T (fun `init -> Staged.unstage (bm n));
      setup = S setup;
      test_id = Id.create ()
    }
  )
}

let expand ts =
  List.concat (List.map ~f:tests ts)

let create_group ~name ?(test_name="") ?(file_name="") ?(module_name="") ts =
  let ts = expand ts in
  {
    name;
    test_name;
    module_name;
    file_name;
    tests = List.map ts ~f:(fun test ->
      let name = name ^ "/" ^ test.Basic_test.name in
      { test with Basic_test.name = name });
  }
