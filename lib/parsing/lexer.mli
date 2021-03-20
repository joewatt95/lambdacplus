(* Indicates that an error was encountered while lexing the lexeme at 
   `source_loc`. *)
exception Lexing_err of {
  lexeme : string;
  source_loc : Common.Location.source_loc;
}

val tokenize : Sedlexing.lexbuf -> Grammar.token