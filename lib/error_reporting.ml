open Containers

module Loc = Common.Location
module PAst = Parsing.Ast
module KAst = Kernel.Ast
let (%>) = Fun.(%>)
let fmt_err_encountered_str ((startpos, endpos) : Loc.source_loc) =
  let start_char = startpos.pos_cnum - startpos.pos_bol + 1 in
  let end_char = endpos.pos_cnum - endpos.pos_bol in
  let start_line = startpos.pos_lnum in
  let end_line = endpos.pos_lnum in
  let s = if start_line = end_line
          then ""
          else Printf.sprintf "line %d, char " end_line
  in
  Printf.sprintf "\nError encountered at line %d, char %d to %s%d\n%s" 
    start_line start_char s end_char

let fmt_parsing_err_str = function
  | Ast_conv.Unknown_var_name {data=PAst.Var var_name; source_loc} ->
    fmt_err_encountered_str source_loc @@ "Unknown var name: " ^ var_name

  | Ast_conv.Underscore_var_name {source_loc; _} ->
    fmt_err_encountered_str source_loc @@ "Underscore can only be used in binders."
  
  | exc -> raise exc

  let fmt_eval_err_str naming_ctx = 
    let unparse = Pretty_print.unparse_internal_expr naming_ctx in
    let unparse_raw = Loc.locate %> unparse in
    function
  | Kernel.Typing.Pi_expected 
      {app={data=app_data; source_loc=app_source_loc}; 
       fn={data=fn_data; source_loc=_}; inferred_type} -> 
    let fmt_str = Printf.sprintf
      {|Unable to typecheck the function application: %s
The expression %s was inferred to have type %s but it should be a Pi.|}
    in 
    fmt_err_encountered_str app_source_loc @@ 
      fmt_str (unparse_raw app_data) (unparse_raw fn_data) (unparse inferred_type)

  | _ -> assert false