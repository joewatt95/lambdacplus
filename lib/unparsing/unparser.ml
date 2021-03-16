module PAst = Parsing.Ast

let rec unparse (expr : PAst.expr) =
 match expr.data with
 | PAst.Type -> "Type"
 | PAst.Kind -> "Kind"
 | PAst.Var var_name -> var_name 
 | PAst.Pi {input_var; input_type; output_type} ->
  "∏ (" ^ input_var ^ " : " ^ unparse input_type ^ "), " ^ unparse output_type
 | PAst.Fun {input_var; body; _} ->
  "λ " ^ input_var ^ " => " ^ unparse body
 | PAst.App {fn; arg} ->
  "(" ^ unparse fn ^ " " ^ unparse arg ^")"
 | _ -> assert false