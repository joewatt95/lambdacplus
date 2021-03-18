open Containers

module Loc = Common.Location
module PAst = Parsing.Ast
module KAst = Kernel.Ast
let (%>) = Fun.(%>)

let print_err source_loc =
      print_endline @@ "\nError encountered at " ^ Loc.show_source_loc source_loc 

let handle_parsing_error = function
  | Ast_conv.Unknown_var_name {data=PAst.Var var_name; source_loc} ->
    print_err source_loc;
    print_endline @@ "Unknown var name: " ^ var_name

  | Ast_conv.Underscore_var_name {source_loc; _} ->
    print_err source_loc;
    print_endline "Underscore can only be used in binders."
  
  | exc -> raise exc

  let handle_eval_error naming_ctx = 
    let unparse = Ast_conv.unparse_internal_expr naming_ctx in
    let unparse_raw = Loc.locate %> unparse in
    function
  | Kernel.Typing.Pi_expected 
      {app={data=app_data; source_loc=app_source_loc}; 
       fn={data=fn_data; source_loc=_}; inferred_type} -> 
    print_err app_source_loc;
    print_endline @@ "Unable to typecheck the function application: " ^ 
      (unparse_raw app_data);
    print_endline @@ "The expression " ^ (unparse_raw fn_data) ^ 
    " was inferred to have type " ^ (unparse inferred_type) ^
    " but it should be a Pi."

  | _ -> assert false