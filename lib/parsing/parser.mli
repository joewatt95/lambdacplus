(** This module contains utilities and boilerplate code for gluing Menhir and
Sedlex together.
*)

val parse : Sedlexing.lexbuf -> string Common.Ast.list_of_stmts
(** [parse lexbuf] parses [lexbuf] into a list of statements.
The resulting AST is close to the concrete syntax, with only some desugaring
occuring. Variables are not converted to de bruijn indices here.
*)

val parse_string : string -> string Common.Ast.list_of_stmts
(** [parse_string str] parses [str] into a list of statements.

This uses {! parse} under the hood.
*)

val parse_channel : in_channel -> string * string Common.Ast.list_of_stmts
(** [parse_channel chan] parses [chan] into a pair of [(str, stmts)], where
- [str] is the original string obtained from [chan]
- [stmts] is the resulting list of statements.

It does so by first reading [str] from [chan] and then calling
{! parse_string} on [str].
*)