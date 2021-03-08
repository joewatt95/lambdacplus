(*
https://github.com/andrejbauer/spartan-type-theory/blob/master/src/lexer.ml
https://github.com/amblafont/sedlex-menhir/blob/master/sedlex-menhir/lexer.ml
*)

let whitesp = [%sedlex.regexp? ' ' | '\t']
let newline = [%sedlex.regexp? ('\r' | '\n' | "\r\n")]
let var_name = [%sedlex.regexp? (alphabetic |  '_'), Star (alphabetic | '_' | '0' .. '9')]

module G = Grammar

let rec tokenize (lexbuf : Sedlexing.lexbuf) =
  match%sedlex lexbuf with
  | eof -> G.EOF
  | "fun" -> G.FUN
  | "Pi" -> G.PI
  | "Type" -> G.TYPE
  | '(' -> G.LPAREN
  | ')' -> G.RPAREN
  | ',' -> G.COMMA
  | ':' -> G.COLON
  | ":=" -> G.COLON_EQ
  | "=>" -> G.DOUBLE_ARROW
  | "def" -> G.DEF
  | "axiom" -> G.AXIOM
  | "check" -> G.CHECK
  | "eval" -> G.EVAL
  | var_name -> G.VAR_NAME (Sedlexing.Latin1.lexeme lexbuf)
  | Plus whitesp -> tokenize lexbuf
  | newline -> Sedlexing.new_line lexbuf; tokenize lexbuf
  | _ -> assert false