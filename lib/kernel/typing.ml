 (*
https://github.com/andrejbauer/spartan-type-theory/blob/master/src/typecheck.ml
 *)

open Parsing.Location
open Normalization

let located_type = locate Ast.Type

let rec infer ctx expr =
 match expr.data with
 | Ast.Type -> expr
 | Ast.Var index -> Context.get_type index ctx

 | Ast.Ascription {expr; expr_type} ->
   check_type ctx expr_type;
   let expr_type = normalize ctx expr_type in
   check ctx expr expr_type;
   expr_type

 | Ast.Pi {input_type; output_type; _} ->
  check_type ctx input_type;
  let input_type = normalize ctx input_type in
  let ctx = Context.add_binding "dummy" ~var_type:input_type ctx in
  check_type ctx output_type;
  located_type

 | Ast.App {fn; arg} ->
  let fn_type = infer ctx fn in
  begin
    match fn_type.data with
    | Ast.Pi {input_type; output_type; _} ->
      check ctx arg input_type;
      beta_reduce output_type arg 
    | _ -> assert false
  end 
 | _ -> assert false

and check ctx expr expr_type =
  match expr.data, expr_type.data with
  | Ast.Fun {input_var; body}, 
    Ast.Pi {input_type; output_type; _} ->
    let ctx = Context.add_binding input_var ~var_type:input_type ctx in 
    (* print_endline @@ Context.show ctx;
    print_endline @@ Ast.show_expr expr;
    print_endline @@ Ast.show_expr expr_type; *)
    check ctx body output_type
  | _, _ -> 
    let inferred_expr_type = infer ctx expr in
    (* Here we need to check if the inferred type and expr_type are equal *)
    if Equality.check_beta_equality inferred_expr_type expr_type
    then ()
    else assert false

and check_type ctx expr = check ctx expr located_type