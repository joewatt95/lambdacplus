open Containers

(* This datatype is used to wrap our ASTs with source location information. *)
type source_loc = Lexing.position * Lexing.position

type 'a located = {
  data : 'a;
  source_loc : source_loc [@opaque] [@equal Utils.always_true];
} 
[@@deriving show, fields, eq, visitors {variety="fold"}]

(* Given some data and a source location, wrap them up in the located type. *)
let locate ?(source_loc = (Lexing.dummy_pos, Lexing.dummy_pos)) data = 
  {data; source_loc}

(* Update the data in a located type using a function f which takes as input the
   old data and outputs the new one.*)
let update_data ({data = old_data; _} as located) f =
  { located with data = f old_data}

(* Set the data in a located object to new_data *)
let set_data located new_data =
  update_data located @@ Fun.const new_data