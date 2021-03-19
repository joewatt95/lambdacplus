module PAst = Parsing.Ast
module Loc = Common.Location

 let unparse =
  let v = 
    object
      inherit [_] PAst.fold as super
      method build_Var _ var_name = var_name
      method build_Type _ = "Type"
      method build_Kind _ = "Kind"

      method build_Pi _ input_var input_type output_type =
        Printf.sprintf "∏ (%s : %s), %s" input_var input_type output_type

      method build_Fun _ input_var _ body = 
        Printf.sprintf "λ %s ⇒ %s" input_var body

      method build_App _ fn arg =
        Printf.sprintf "(%s %s)" fn arg

      method! visit_expr env {data; _} = super#visit_raw_expr env data

      method build_Let _ _ _ _ = ""
      method build_Ascription _ _ _ = ""
      method build_located _ _ _ _ = ""
      method visit_'a _ _ = ""
    end in v#visit_expr @@ Loc.locate PAst.Kind

let unparse_internal_expr naming_ctx expr =
  expr
  |> Ast_conv.internal_to_parser_expr naming_ctx
  |> unparse 