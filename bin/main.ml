open Containers

open Parsing
module PAst = Ast
module Loc = Common.Location

module KAst = Kernel.Ast
open Cs4215_dependent_types

let () =
  print_endline "Enter input:";
  try
    let stmts, ctx = 
      stdin 
      |> Parser.parse_channel
      |> Fun.flip Ast_conv.parser_to_internal_stmts 
         Kernel.Context.empty 
    in
    stmts
    |> Fun.flip Kernel.Eval_statements.eval_stmts ctx
    |> fun (expr, _) -> expr
    |> Ast_conv.internal_to_parser_expr ctx
    |> PAst.unparse
    (* |> Parsing.Ast.show_expr *)
    |> fun str -> print_endline @@ "\nHere's the output:\n" ^ str;
    flush stdout
  with exc ->
    Error_handling.pretty_print_err exc