module Loc = Common.Location
module PAst = Parsing.Ast
module KAst = Kernel.Ast
let print_err source_loc =
      print_endline @@ "\nError encountered at " ^ Loc.show_source_loc source_loc 
let pretty_print_err = function
  | Ast_conv.Unknown_var_name {data=PAst.Var var_name; source_loc} ->
    print_err source_loc;
    print_endline @@ "Unknown var name: " ^ var_name

  | Ast_conv.Underscore_var_name {source_loc; _} ->
    print_err source_loc;
    print_endline "Underscore can only be used in binders."

  | exc -> raise exc 