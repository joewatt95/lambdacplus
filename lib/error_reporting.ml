open Containers
open Common

type err_loc = {
    start_row : int; end_row : int;
    start_col : int; end_col : int;
}

let fmt_err_encountered_str (start_pos, end_pos) err_str =
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
    start_line start_char s end_char err_str,
    { start_row = start_line - 1; end_row = end_line - 1; 
      start_col = start_char; end_col = end_char }

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

let unparse Kernel.Typing.{expr; ctx} = 
  Pretty_print.unparse_internal_expr ctx expr

(* Return the substring of `source_str` corresponding the source locations given
by the `(start_pos, end_pos)`
Note that `source_str` is an Ocaml string encoded in UTF8 and so some
conversion needs to be done when slicing it.
*)
let get_from_source_str source_str 
  Lexing.({pos_cnum=start_pos; _}, {pos_cnum=end_pos; _}) =
   let open CCUtf8_string in
   source_str
   (* Convert the string to a utf8 bytestring *)
   |> unsafe_of_string
   (* Convert to a seq, starting from the index given by `start_pos` *)
   |> to_seq ~idx:start_pos
   (* Slice the seq until `end_pos` *)
   |> Seq.take @@ end_pos - start_pos
   (* Convert the string back to a utf8 bytestring *)
   |> of_seq
   (* Convert the string back to a normal Ocaml string *)
   |> to_string

let fmt_eval_err_str source_str =
  let get_from_source_str = get_from_source_str source_str in
  function
  | Kernel.Typing.Cannot_infer_type {data; source_loc} ->
    (* let data_str = unparse_raw data in *)
    data
    |> begin function
       | Ast.Kind -> "'Kind' does not have a type."
       | Ast.Fun _ -> Printf.sprintf
         {|Unable to infer the type of the function '%s'.
Please either annotate the inputs or ascribe a type to the whole function.|}
        (get_from_source_str source_loc)
       | Ast.Pair _ -> Printf.sprintf
         {|Unable to infer the type of the pair '%s'.
Please ascribe a type to the it. |}
        (get_from_source_str source_loc)
       | Ast.Inl _  | Ast.Inr _ -> Printf.sprintf
         {|Unable to infer the type of the sum constructor '%s'.
Please ascribe a type to it. |}
        (get_from_source_str source_loc)
       | _ -> assert false
       end
    |> fmt_err_encountered_str source_loc 

  | Kernel.Typing.Ill_formed_type {expr={source_loc; _}; inferred_type} ->
    let inferred_type = unparse inferred_type in
    source_loc
    |> get_from_source_str
    |> fun str -> Printf.sprintf 
      {|'%s' is not a valid type or type constructor.
Its type was inferred to be '%s' but it should be 'Type' or 'Kind'.|}
      str inferred_type
  |> fmt_err_encountered_str source_loc

  | Kernel.Typing.Type_mismatch 
    { outer_expr={source_loc=outer_source_loc; _};
      expr={source_loc=inner_source_loc; _};
      expected_type; inferred_type} -> 
    (* let outer_expr = get_from_source_str outer_source_loc in *)
    let inner_expr = get_from_source_str inner_source_loc in
    let inferred_type = unparse inferred_type in
    let expected_type =
      begin match expected_type with
      | Family str -> str
      | Exact {source_loc; _} -> get_from_source_str source_loc
      end
    in
    Printf.sprintf
    {|Type mismatch! While typechecking the underlined expression, 
'%s' was inferred to have type '%s'. 
It was expected to be '%s'.|}
    inner_expr inferred_type expected_type
    |> fmt_err_encountered_str outer_source_loc

    | Kernel.Typing.Type_mismatch_in_match
      { outer_expr={source_loc; _};
        inl_type; inr_type} ->
      Printf.sprintf
      {|Type mismatch! While typechecking the underlined match expression, 
the left and right branches were found to have different types.
Type of left branch: %s
Type of right branch: %s.|}
      (unparse inl_type) (unparse inr_type)
    |> fmt_err_encountered_str source_loc
    
  | Kernel.Typing.Type_contains_free_var
    { outer_expr={source_loc; _}; free_var; inferred_type } -> 
    Printf.sprintf
    {|While typechecking the underlined expression, the body was inferred to 
have type '%s' which contains '%s' free.
The variable used for the witness must be discharged before you can conclude a
proof by existential elimination!
|}
    (unparse inferred_type) free_var
    |> fmt_err_encountered_str source_loc

  | exn -> raise exn