 (*
https://github.com/andrejbauer/spartan-type-theory/blob/master/src/typecheck.ml
 *)

module Loc = Parsing.Location
module Norm = Normalization

let rec infer ctx (expr : Ast.expr) =
 match expr.data with
 | Ast.Type -> expr
 | Ast.Var index -> Context.get_type index ctx

 | Ast.Ascription {expr; expr_type} ->
   check_type ctx expr_type;
   let expr_type = Norm.normalize ctx expr_type in
   check ctx expr expr_type;
   expr_type

 | Ast.Pi {input_var; input_type; output_type} ->
  check_type ctx input_type;
  let input_type = Norm.normalize ctx input_type in
  let ctx = Context.add_binding input_var ~var_type:input_type ctx in
  check_type ctx output_type;
  Ast.located_type

 | Ast.App {fn; arg} ->
  let fn_type = infer ctx fn in
  begin
    match fn_type.data with
    | Ast.Pi {input_type; output_type; _} ->
      check ctx arg input_type;
      Norm.beta_reduce output_type arg 
    | _ -> assert false
  end 

 | Ast.Fun {input_var; input_type=Some input_ty; body} ->
    check_type ctx input_ty;
    let input_ty = Norm.normalize ctx input_ty in
    let ctx = Context.add_binding input_var ~var_type:input_ty ctx in
    let output_type = infer ctx body in
    (* What source location info should be used here? *)
    Loc.locate @@ Ast.Pi {input_var; input_type=input_ty; output_type}

 | Ast.Let {var_name; binding; body} ->
   let var_type = infer ctx binding in
   let ctx = Context.add_binding var_name ~var_type:var_type ctx in
   body |> infer ctx
        |> Fun.flip Norm.beta_reduce binding
        |> Norm.normalize ctx

 | _ -> assert false

and check ctx expr expr_type =
  match expr.data, expr_type.data with
  | Ast.Fun {input_var; input_type=None; body}, 
    Ast.Pi {input_type; output_type; _} ->
    ctx |> Context.add_binding input_var ~var_type:input_type
        |> fun ctx -> check ctx body output_type
    (* print_string "Context: ";
    print_endline @@ Context.show ctx;
    print_string "Expr: ";
    print_endline @@ Ast.show_expr expr;
    print_string "Checking against: ";
    print_endline @@ Ast.show_expr expr_type; *)
  | _, _ -> 
    let inferred_expr_type = infer ctx expr in
    (* Here we need to check if the inferred type and expr_type are equal *)
    if Ast.equal inferred_expr_type expr_type
    then ()
    else assert false

and check_type ctx expr = check ctx expr Ast.located_type