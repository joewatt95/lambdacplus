(* Boilerplate taken from
   https://stackoverflow.com/questions/52474076/using-ocamlyacc-with-sedlex
*)
open Containers

let parse lexbuf =
  let revised_parser =
    MenhirLib.Convert.Simplified.traditional2revised Grammar.main
  in
  let lexer () =
    let token = Lexer.tokenize lexbuf in
    let startpos, endpos = Sedlexing.lexing_positions lexbuf in
    token, startpos, endpos
  in
  try
    revised_parser lexer
  with
  | Grammar.Error -> Lexer.raise_syntax_err lexbuf 

(* Ugly hack to work around Sedlex not incrementing the line numbers properly.

https://github.com/ocaml-community/sedlex/issues/96 
https://github.com/Deducteam/lambdapi/pull/552/files
*)
let parse_string str =
  str
  |> Sedlexing.Utf8.from_string
  |> Fun.tap @@ Fun.flip Sedlexing.set_position
                  {pos_fname=""; pos_lnum=1; pos_bol=0; pos_cnum=0} 
  |> parse

(* let parse_channel = Sedlexing.Utf8.from_channel %> parse *)

let parse_channel channel =
  let raw_string = IO.read_all channel in
  let parsed = parse_string raw_string in
  raw_string, parsed