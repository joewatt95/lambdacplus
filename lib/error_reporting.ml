open Containers

module Loc = Common.Location
module PAst = Parsing.Ast
module KAst = Kernel.Ast

let fmt_err_encountered_str ((start_pos, end_pos) : Loc.source_loc) =
  let get_col (pos : Lexing.position) = pos.pos_cnum - pos.pos_bol + 1 in
  let start_char = get_col start_pos in
  let end_char = get_col end_pos in
  let start_line = start_pos.pos_lnum in
  let end_line = end_pos.pos_lnum in
  let s = if start_line = end_line
          then ""
          else Printf.sprintf "line %d, col " end_line
  in
  Printf.sprintf "\nError encountered at line %d, col %d to %s%d.\n%s" 
    start_line start_char s end_char

let fmt_parse_err_str = function
  | Parsing.Lexer.Lexing_err {lexeme; source_loc} ->
    lexeme
    |> Printf.sprintf "Invalid token '%s'"
    |> fmt_err_encountered_str source_loc

  | Ast_conv.Unknown_var_name {data=PAst.Var var_name; source_loc} ->
    var_name
    |> Printf.sprintf "Unknown variable name '%s'."
    |> fmt_err_encountered_str source_loc

  | Ast_conv.Underscore_var_name source_loc ->
    fmt_err_encountered_str source_loc @@ "Underscore can only be used in binders."
  
  | exc -> raise exc

let fmt_eval_err_str naming_ctx exc =
  let unparse = Pretty_print.unparse_internal_expr naming_ctx in
  let unparse_raw = Fun.(Loc.locate %> unparse) in
  match exc with
  | Kernel.Typing.Pi_expected 
    {app={data=app_data; source_loc=app_source_loc}; 
      fn={data=fn_data; source_loc=_}; inferred_type} -> 
    (app_data, fn_data, inferred_type) 
    |> fun (x, y, z) -> (unparse_raw x, unparse_raw y, unparse z)
    |> fun (x, y, z) -> Printf.sprintf
    {|Unable to typecheck the function application '%s'.
The expression '%s' was inferred to have type '%s' but it should be a Pi.|} x y z
    |> fmt_err_encountered_str app_source_loc

  | Kernel.Typing.Cannot_infer_type_of_fn {data; source_loc} ->
    data
    |> unparse_raw
    |> Printf.sprintf {|Unable to infer the type of the function '%s'.
Please either annotate the inputs or ascribe a type to the whole function.|}
    |> fmt_err_encountered_str source_loc

  | Kernel.Typing.Cannot_infer_type_of_kind source_loc ->
    fmt_err_encountered_str source_loc @@ "'Kind' does not have a type."

  | Kernel.Typing.Ill_formed_type {expr={data; source_loc}; inferred_type} ->
    (data, inferred_type)
    |> fun (x, y) -> (unparse_raw x, unparse y)
    |> Fun.uncurry @@ Printf.sprintf 
      {|'%s' is not a valid type or type constructor.
It should have type 'Type' or 'Kind' but we inferred the type to be '%s' instead.|}
  |> fmt_err_encountered_str source_loc  

  | Kernel.Typing.Type_mismatch 
    {outer_expr={data=outer_expr_data; source_loc};
     expr={data=expr_data; _}; inferred_type; expected_type} ->
    (outer_expr_data, expr_data, inferred_type, expected_type)
    |> fun (w, x, y, z) -> (unparse_raw w, unparse_raw x, unparse y, unparse z)
    |> fun (w, x, y, z) -> Printf.sprintf
    {|While typechecking '%s', '%s' was inferred to have type '%s' but this doesn't
match the expected type of '%s'.|} w x y z
    |> fmt_err_encountered_str source_loc

| exc -> raise exc