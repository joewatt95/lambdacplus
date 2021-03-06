(* Boilerplate taken from
   https://log.h11.io/2020/0528/
*)

let parse (lexbuf : Sedlexing.lexbuf) =
  let revised_parser =
    MenhirLib.Convert.Simplified.traditional2revised Grammar.main
  in
  let lexer () =
    let token = Lexer.tokenize lexbuf in
    let startpos, endpos = Sedlexing.lexing_positions lexbuf in
    token, startpos, endpos
  in
  revised_parser lexer

let parse_string (src : string) = parse (Sedlexing.Utf8.from_string src)

let parse_channel (src : in_channel) = parse (Sedlexing.Utf8.from_channel src)
