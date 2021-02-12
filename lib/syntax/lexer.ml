(*
https://github.com/andrejbauer/spartan-type-theory/blob/master/src/lexer.ml
https://github.com/amblafont/sedlex-menhir/blob/master/sedlex-menhir/lexer.ml
*)

let whitesp = [%sedlex.regexp? ' ' | '\t']
let newline = [%sedlex.regexp? ('\r' | '\n' | "\r\n")]
let var_name = [%sedlex.regexp? (alphabetic |  '_'), Star (alphabetic | '_' | '0' .. '9')]

module P = Parser

let rec tokenize (lexbuf : Sedlexing.lexbuf) =
  match%sedlex lexbuf with
  | eof -> P.EOF
  | "fun" -> P.FUN
  | "Pi" -> P.PI
  | "Type" -> P.TYPE
  | '(' -> P.LPAREN
  | ')' -> P.RPAREN
  | ',' -> P.COMMA
  | ':' -> P.COLON
  | ":=" -> P.COLON_EQ
  | "=>" -> P.DOUBLE_ARROW
  | "def" -> P.DEF
  | "axiom" -> P.AXIOM
  | "check" -> P.CHECK
  | "eval" -> P.EVAL
  | var_name -> P.VAR_NAME (Sedlexing.Latin1.lexeme lexbuf)
  | Plus whitesp -> tokenize lexbuf
  | newline -> Sedlexing.new_line lexbuf; tokenize lexbuf
  | _ -> assert false
