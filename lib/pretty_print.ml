module PAst = Parsing.Ast

(* Unparses an expression back to a string for printing. *)
let rec unparse (expr : PAst.expr) =
 match expr.data with
 | Type -> "Type"
 | Kind -> "Kind"
 | Var var_name -> var_name 
 | Pi {input_var; input_type; output_type} ->
  Printf.sprintf "∏ (%s : %s), %s" input_var (unparse input_type) (unparse output_type)
 | Fun {input_var; body; _} ->
  "λ " ^ input_var ^ " ⇒ " ^ unparse body
 | App {fn; arg} ->
  "(" ^ unparse fn ^ " " ^ unparse arg ^")"
(* No other cases are possible since all expressions are type checked and then
  eagerly normalized to one of the above forms. *)
 | _ -> assert false

let unparse_internal_expr naming_ctx expr =
  expr
  |> Ast_conv.internal_to_parser_expr naming_ctx
  |> unparse 