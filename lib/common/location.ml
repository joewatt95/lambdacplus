open Containers

(* This datatype is used to wrap our ASTs with source location information.
*)
type source_loc = Lexing.position * Lexing.position

type 'a located = {
  data : 'a;
  source_loc : source_loc [@opaque] [@equal fun _ _ -> true];
} [@@deriving show, fields, eq]

let locate ?(source_loc = (Lexing.dummy_pos, Lexing.dummy_pos)) data = 
  {data; source_loc}

let update_data ({data = old_data; _} as located) f =
  { located with data = f old_data}

let set_data located new_data =
  update_data located @@ Fun.const new_data