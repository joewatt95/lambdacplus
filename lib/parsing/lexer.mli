(* This indicates that a syntax error occured while parsing `lexeme` at 
   `source_loc`. *)
exception Syntax_error of {
  lexeme : string;
  source_loc : Common.Location.source_loc;
}

(* Tokenize a lexer buffer. *)
val tokenize : Sedlexing.lexbuf -> Grammar.token

(* This constructs a Syntax_error using the given lexbuf and raises it. *)
val raise_syntax_err : Sedlexing.lexbuf -> 'a