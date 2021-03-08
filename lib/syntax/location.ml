type 'a located = {
  data: 'a;
  source_loc : Lexing.position * Lexing.position [@printer fun _ _ -> ()];
} [@@deriving show]

let locate data source_loc = {data; source_loc}

let update_data_with_f ({data = old_data; _} as located) f =
  { located with data = f old_data}

let update_data located new_data =
  let open Containers in
  update_data_with_f located @@ Fun.const new_data
