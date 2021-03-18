open Containers

(* This datatype is used to wrap our ASTs with source location information.
*)
type source_loc = Lexing.position * Lexing.position

let show_source_loc ((startpos, endpos) : source_loc) =
  let startchar = startpos.pos_cnum - startpos.pos_bol in
  let endchar = endpos.pos_cnum - endpos.pos_bol in
  let s = if startchar = endchar 
                 then "line " ^ string_of_int endpos.pos_lnum ^ ", char"
                 else "char "
  in
  "line " ^ (string_of_int startpos.pos_lnum) ^ ", char " ^
  (string_of_int startchar) ^ " to " ^ s ^ (string_of_int endchar) 

type 'a located = {
  data : 'a;
  source_loc : source_loc [@opaque];
} [@@deriving show, fields]

let locate ?(source_loc = (Lexing.dummy_pos, Lexing.dummy_pos)) data = 
  {data; source_loc}

let update_data ({data = old_data; _} as located) f =
  { located with data = f old_data}

let set_data located new_data =
  update_data located @@ Fun.const new_data