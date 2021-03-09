open Containers

type 'a located = {
  data: 'a;
  source_loc : Lexing.position * Lexing.position [@printer fun _ _ -> ()];
} [@@deriving show]

let locate data source_loc = {data; source_loc}

let update_data ({data = old_data; _} as located) f =
  { located with data = f old_data}

let set_data located new_data =
  update_data located @@ Fun.const new_data
