open Containers

(** This module defines the {! located} type which represents data wrapped
with a source location.
It also has utilities for manipulating {! located} records.

Note that some of the functions here are automatically derived using the
Visitors package among others. As such these are not documented.
These include functions like {! show_located}, {! equal_located} and {! fold}
which function like their Haskell counterparts.
*)

type source_loc = Lexing.position * Lexing.position
(** This datatype is used to wrap our ASTs with source location information. *)

type 'a located = {
  data : 'a;
  source_loc : source_loc [@opaque] [@equal Utils.always_true];
}
(* Derive useful functions like Haskell *)
[@@deriving show, fields, eq, visitors {variety="fold"}]

let locate ?(source_loc = (Lexing.dummy_pos, Lexing.dummy_pos)) data = 
  {data; source_loc}
(** [locate ?source_loc data] wraps [data] in a {! located} record using
[source_loc].

If [source_loc] is not provided, a dummy location is used.
*)

let update_data ({data = old_data; _} as located) f =
  { located with data = f old_data}
(*** [update_data located f] updates the data in [located] using [f]. 
*)

let set_data located new_data =
  update_data located @@ Fun.const new_data
(** [set_data located new_data] sets the data in [located] to [new_data], ignoring
the old data. *)