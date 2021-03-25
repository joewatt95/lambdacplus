open Containers
open Common

 let unparse =
  let uncurry3_sprintf = Fun.(Printf.sprintf %> Common.Utils.uncurry3) in
  let v = 
    object
      inherit [_] Ast.ast_folder as super
      (* To unparse variables, simply return the variable name associated with
         them. *)
      method build_Var _ = Fun.id
      method build_Type _ = "Type"
      method build_Kind _ = "Kind"

      method build_abstraction _ var_name expr body = (var_name, expr, body) 

      method build_Pi _ = uncurry3_sprintf "(∏ (%s : %s), %s)"

      method build_Sigma _ = uncurry3_sprintf "(Σ (%s : %s), %s)"

      method build_Fun env input_var input_type body = 
        match input_type with
        | Some input_type ->
          input_type
          |> super#visit_expr env
          |> fun input_type -> 
              Printf.sprintf "(λ (%s : %s) ⇒ %s)" input_var input_type body
        | None ->
          Printf.sprintf "(λ %s ⇒ %s)" input_var body

      method build_App _ = Fun.uncurry @@ Printf.sprintf "(%s %s)"

      method build_Let _ = uncurry3_sprintf "(let %s := %s in %s)"

      method build_Ascription _ = Printf.sprintf "(%s : %s)"

      method build_Pair _ = Fun.uncurry @@ Printf.sprintf "(%s, %s)"
      method build_Fst _ = Printf.sprintf "(fst %s)"
      method build_Snd _ = Printf.sprintf "(snd %s)"

      method build_Match _ =
        Printf.sprintf 
          {|(match %s with
 | %s
 | %s
 end)|}

      method build_match_binding _ = Printf.sprintf "%s -> %s"
      method build_Sum _ = Fun.uncurry @@ Printf.sprintf "(%s + %s)"
      method build_Inl _ = Printf.sprintf "(inl %s)"
      method build_Inr _ = Printf.sprintf "(inr %s)" 

      method build_pair _ left right = (left, right)

      (* method build_Let_pair _ = Printf.sprintf "let (%s, %s) := %s in %s" *)

      (* These last 2 methods aren't used. *)
      method build_located _ _ _ _ = ""
      method visit_'a _ _ = ""
    end in v#visit_expr @@ Location.locate 0

let unparse_internal_expr naming_ctx expr =
  expr
  |> Ast_conv.internal_to_parser_expr naming_ctx
  |> unparse 