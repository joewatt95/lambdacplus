(** This module glues all our modules into a single function, namely
{! parse_and_eval} which is responsible for parsing and evaluating strings input
by users.
*)

(** The current state of the naming and evaluation contexts. 
*)
type contexts = {
  naming_ctx : Kernel.Context.t;
  (** This is used to parse the string to the parser's AST and then convert it
  to the de bruijn one, where all variables are identified by de bruijn indices.
  *)

  eval_ctx : Kernel.Context.t
  (** This is used for typechecking and evluating expressions in the de bruijn
  AST. All types and bindings stored here have indices for variable names, rather
  than strings.
  *)
}

let initial_contexts = let open Kernel.Context in
  {naming_ctx=empty; eval_ctx=empty}
(** Initial (ie empty) naming and evaluation contexts.
*)

(** [parse_and_eval str {naming_ctx; eval_ctx}] parses and evalutes a string.

Note that we actually use 2 contexts to parse and evaluate user inputs:
- {! naming_ctx} is first used to parse the input string into the de bruijn AST.
- {! eval_ctx} is then used for typechecking and evaluating.

The output is a [result] record (which is like an Either monad) that is either
- [Ok {naming_ctx, eval_ctx}, result]
  where
  {ul
  {- {! naming_ctx} and {! eval_ctx} are the new naming and evaluation contexts respectively }
  {- [result] is the string of the expression resulting from evaluating [str] }
}

- [Error err_str, err_loc] where
  {ul
  {- [err_str] is a string representation of the error that occurred }
  {- [err_loc] is a {!type:Error_reporting.err_loc},
    ie a record containing the location of the error }
  }
*)
let parse_and_eval str {naming_ctx; eval_ctx} =
  try
    (* Parse the string *)
    let stmts, naming_ctx =
      str
      |> Parsing.Parser.parse_string
      |> Fun.flip Ast_conv.parser_to_internal_stmts naming_ctx
    in
    try
      let stmts' , eval_ctx =
        Fun.flip Kernel.Eval_statements.eval_stmts eval_ctx stmts
      in
      Ok ({naming_ctx; eval_ctx},
      Pretty_print.unparse_internal_expr naming_ctx stmts')
    with exn ->
      Error (Error_reporting.format_typing_error str exn)
  with exn ->
    Error (Error_reporting.format_parse_error exn)