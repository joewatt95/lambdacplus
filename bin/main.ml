open Containers

open Kernel.Ast
open Kernel.Ast_conversion
open Kernel

let internal_stmt_to_parser_expr ctx ({data=stmt; _} : stmt) =
  match stmt with
  | Def {binding; _} -> internal_to_parser_expr binding ctx
  | Axiom {var_type; _} -> internal_to_parser_expr var_type ctx
  | Check expr
  | Eval expr -> internal_to_parser_expr expr ctx

let () =
  print_endline "Enter input for parsing:";
  let stmts = Parsing.Parser.parse_channel stdin in
  let stmt = stmts |> List.hd in
  match stmt.data with
  | Eval expr ->
    print_endline "\nNormalizing input...";
    (* print_endline @@ Parsing.Ast.show_expr expr; *)
    expr |> Fun.flip parser_to_internal_expr Context.empty
         |> Normalization.normalize Context.empty
         |> Fun.flip internal_to_parser_expr Context.empty
         |> Parsing.Ast.show_expr
         |> print_endline; 
  | Check expr ->
    print_endline "\nType checking input...";
    expr |> Fun.flip parser_to_internal_expr Context.empty
         |> Typing.infer Context.empty
         |> Fun.flip internal_to_parser_expr Context.empty
         |> Parsing.Ast.show_expr
         |> print_endline;
    flush stdout
  | _ -> assert false

  (* print_endline "\nParser AST: ";
  print_endline @@ Parsing.Ast.show_list_of_stmts stmts;
  let stmts, ctx = parser_to_internal_stmts stmts Context.empty in
  print_endline "\nDe Bruijn AST: ";
  print_endline @@ show_list_of_stmts stmts;
  print_endline "\nFinal context: ";
  print_endline @@ Context.show ctx;
  print_endline "\nNormalizing last expr back to parser's AST:";
  stmts
  |> List.rev
  |> List.hd
  |> internal_stmt_to_parser_expr ctx
  |> Parsing.Ast.show_expr
  |> print_endline; *)
