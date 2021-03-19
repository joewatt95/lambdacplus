module PAst = Parsing.Ast
module Loc = Common.Location

 let unparse =
  let v = 
    object
      inherit [_] PAst.fold as super
      method build_Var _ var_name = var_name
      method build_Type _ = "Type"
      method build_Kind _ = "Kind"

      method build_Pi _ =
        Printf.sprintf "(∏ (%s : %s), %s)"

      method build_Fun env input_var input_type body = 
        match input_type with
        | Some input_type ->
          input_type
          |> super#visit_expr env
          |> fun x -> Printf.sprintf "(λ (%s : %s) ⇒ %s)" input_var x body
        | None ->
          Printf.sprintf "(λ %s ⇒ %s)" input_var body

      method build_App _ = Printf.sprintf "(%s %s)"

      method build_Let _ = Printf.sprintf "(let %s := %s in %s)"

      method build_Ascription _ = Printf.sprintf "(%s : %s)"

      method! visit_expr env {data; _} = super#visit_raw_expr env data

      (* These last 2 methods aren't used. *)
      method build_located _ _ _ _ = ""
      method visit_'a _ _ = ""
    end in v#visit_expr @@ Loc.locate 0

let unparse_internal_expr naming_ctx expr =
  expr
  |> Ast_conv.internal_to_parser_expr naming_ctx
  |> unparse 