(* Boilerplate taken from
   https://stackoverflow.com/questions/52474076/using-ocamlyacc-with-sedlex
*)

open CCFun

let parse lexbuf =
  let revised_parser =
    MenhirLib.Convert.Simplified.traditional2revised Grammar.main
  in
  let lexer () =
    let token = Lexer.tokenize lexbuf in
    let startpos, endpos = Sedlexing.lexing_positions lexbuf in
    token, startpos, endpos
  in
  revised_parser lexer

let parse_string = Sedlexing.Utf8.from_string %> parse

let parse_channel = Sedlexing.Utf8.from_channel %> parse