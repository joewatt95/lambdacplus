open Containers

let () =
  print_endline "Enter input:";
  let stmts, ctx = 
    stdin 
    |> Parsing.Parser.parse_channel 
    |> Fun.flip Kernel.Ast_conversion.parser_to_internal_stmts 
       Kernel.Context.empty 
  in
  stmts
  |> Fun.flip Kernel.Eval_statements.eval_stmts ctx
  |> fun (expr, _) -> expr
  |> Fun.flip Kernel.Ast_conversion.internal_to_parser_expr ctx
  |> Parsing.Ast.unparse
  (* |> Parsing.Ast.show_expr *)
  |> fun str -> print_endline @@ "\nHere's the output:\n" ^ str;
  flush stdout