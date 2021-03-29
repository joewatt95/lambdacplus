open Containers
open Common

 let unparse =
  let v = 
    object (self)
      inherit [_] Ast.ast_folder as super
      (* To unparse variables, simply return the variable name associated with
         them. *)
      method build_Var _ = Fun.id
      method build_Type _ = "Type"
      method build_Kind _ = "Kind"

      method build_abstraction _ var_name expr body = (var_name, expr, body) 

      method build_pi_sigma fmt1 fmt2 (var_name, input_type, output_type) =
         match var_name with
        | "_" -> Printf.sprintf fmt1 input_type output_type
        | _ -> Printf.sprintf fmt2 var_name input_type output_type

      method build_Pi _ =
        self#build_pi_sigma "(%s → %s)" "(∏ (%s : %s), %s)"

      method build_Sigma _ =
        self#build_pi_sigma "(%s ∨ %s)" "(Σ (%s : %s), %s)"

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

      method build_Let _ (var_name, binding, body) = 
        Printf.sprintf "(let %s := %s in %s)" var_name binding body

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