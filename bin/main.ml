open Containers

open Parsing
module PAst = Ast
module Loc = Common.Location

module KAst = Kernel.Ast
open Cs4215_dependent_types

let () =
  (* For debugging *)
  Printexc.record_backtrace true;
  print_endline "Enter input:";
  let stmts, naming_ctx = 
    try
      stdin
      |> Parser.parse_channel
      |> Fun.flip Ast_conv.parser_to_internal_stmts 
         Kernel.Context.empty 
    with exc ->
      print_endline @@ Error_reporting.fmt_parse_err_str exc;
      exit 1
  in
  try
    stmts
    |> Fun.flip Kernel.Eval_statements.eval_stmts naming_ctx
    |> fun (expr, _) -> expr
    |> Pretty_print.unparse_internal_expr naming_ctx
    (* |> Parsing.Ast.show_expr *)
    |> fun str -> print_endline @@ "\nHere's the output:\n" ^ str;
    flush stdout
  with exc ->
    print_endline @@ Error_reporting.fmt_eval_err_str naming_ctx exc