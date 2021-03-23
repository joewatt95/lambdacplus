open Containers
open Common

let fmt_err_encountered_str ((start_pos, end_pos) : Location.source_loc) =
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
  | Parsing.Lexer.Syntax_error {lexeme; source_loc} ->
    lexeme
    |> Printf.sprintf "Syntax error: '%s'"
    |> fmt_err_encountered_str source_loc

  | Ast_conv.Unknown_var_name {data=Ast.Var var_name; source_loc} ->
    var_name
    |> Printf.sprintf "Unknown variable name '%s'."
    |> fmt_err_encountered_str source_loc

  | Ast_conv.Underscore_var_name source_loc ->
    fmt_err_encountered_str source_loc @@ "Underscore can only be used in binders."
  
  | exc -> raise exc

let fmt_eval_err_str naming_ctx exc =
  let unparse = Pretty_print.unparse_internal_expr naming_ctx in
  let unparse_raw = Fun.(Location.locate %> unparse) in
  let unparse_expected_type = function
    | Kernel.Typing.Exact expr -> unparse expr
    | Kernel.Typing.Family str -> str
  in
  match exc with
  | Kernel.Typing.Cannot_infer_type {data; source_loc} ->
    let data_str = unparse_raw data in
    data
    |> begin 
        function
        | Ast.Kind -> "'Kind' does not have a type."
        | Ast.Fun _ -> Printf.sprintf
          {|Unable to infer the type of the function '%s'.
Please either annotate the inputs or ascribe a type to the whole function.|}
          data_str
        | Ast.Pair _ -> Printf.sprintf
          {|Unable to infer the type of the pair '%s'.contents
Please ascribe a type to the pair constructor. |}
          data_str
        | _ -> assert false
        end
    |> fmt_err_encountered_str source_loc 

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
    print_endline @@ Ast.show_expr Format.pp_print_int inferred_type;
    print_endline @@ Kernel.Context.show naming_ctx;
    (outer_expr_data, expr_data, inferred_type, expected_type)
    |> fun (w, x, y, z) -> (unparse_raw w, unparse_raw x, unparse y, unparse_expected_type z)
    |> fun (w, x, y, z) -> Printf.sprintf
    {|While typechecking '%s', '%s' was inferred to have type '%s'.
However, the expected type is '%s'.|} w x y z
    |> fmt_err_encountered_str source_loc

  | exc -> raise exc