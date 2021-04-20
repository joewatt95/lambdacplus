(** Implementation of the lexer using Sedlex. *)

exception Syntax_error of {
  lexeme : string; (** The lexeme that caused the error. *)
  source_loc : Common.Location.source_loc; (** The location of the lexeme. *)
}
(** Indicates that a syntax error occured while parsing. *)

val tokenize : Sedlexing.lexbuf -> Grammar.token
(** [Tokenize lexbuf] converts a lexbuf into a token. *)

val raise_syntax_err : Sedlexing.lexbuf -> 'a
(** [raise_syntax_err lexbuf] constructs a {! Syntax_error} using [lexbuf] and 
raises it. *)