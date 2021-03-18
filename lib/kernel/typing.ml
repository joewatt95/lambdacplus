open Containers

module Loc = Common.Location
module Norm = Normalization

exception Pi_expected of {
  app : Ast.expr; 
  fn: Ast.expr; 
  inferred_type : Ast.expr
}

(* let () = Printexc.register_printer @@
  function
  | Pi_expected {fn={source_loc; _}; _} ->
    Some ("Error typechecking expression at " ^ 
          (Loc.show_source_loc source_loc) ^ ".\n" ^
           "Its type should be a Pi.")
  | _ -> None *)

let rec infer ctx (expr : Ast.expr) =
 match expr.data with
 | Ast.Type -> Loc.set_data expr Ast.Kind 
 | Ast.Var index -> Context.get_type ctx index

 (* Do we want to allow users to assert that the type of Type, and type
    constructors is Kind? *)
 | Ast.Ascription {expr; expr_type} ->
  begin
    match expr_type.data with
    | Ast.Kind -> check ctx expr Ast.located_kind; expr_type
    | _ ->
      check_well_formed_type ctx expr_type;
      let expr_type = Norm.normalize ctx expr_type in
      check ctx expr expr_type;
      expr_type
  end

 | Ast.Pi {input_var; input_type; output_type} ->
  check_well_formed_type ctx input_type;
  let input_type = Norm.normalize ctx input_type in
  let ctx = Context.add_binding input_var ~var_type:input_type ctx in
  get_well_formed_type ctx output_type

 | Ast.App {fn; arg} ->
  let inferred_type = infer ctx fn in
  begin
    match inferred_type.data with
    | Ast.Pi {input_type; output_type; _} ->
      check ctx arg input_type;
      Norm.beta_reduce output_type arg 
    | _ -> raise @@ Pi_expected {app=expr; fn; inferred_type}
  end 

 | Ast.Fun {input_var; input_type=Some input_ty; body} ->
  check_well_formed_type ctx input_ty;
  let input_ty = Norm.normalize ctx input_ty in
  let ctx = Context.add_binding input_var ~var_type:input_ty ctx in
  let output_type = infer ctx body in
  (* What source location info should be used here? *)
  Loc.set_data expr @@ Ast.Pi {input_var; input_type=input_ty; output_type}

 | Ast.Let {var_name; binding; body} ->
  let var_type = infer ctx binding in
  let ctx = Context.add_binding var_name ~var_type:var_type ctx in
  body 
  |> infer ctx
  |> Fun.flip Norm.beta_reduce binding
  |> Norm.normalize ctx

 | Ast.Kind -> print_endline "Kind doesn't have a type!"; assert false
 | _ -> assert false

and check ctx expr expr_type =
  match expr.data, expr_type.data with
  | Ast.Fun {input_var; input_type=None; body}, 
    Ast.Pi {input_type; output_type; _} ->
    ctx 
    |> Context.add_binding input_var ~var_type:input_type
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
    if not @@ Ast.equal inferred_expr_type expr_type
    then assert false

and get_well_formed_type ctx expr = 
  let inferred_type = infer ctx expr in
  match inferred_type.data with
  | Ast.Type | Ast.Kind -> inferred_type
  | _ -> assert false

and check_well_formed_type ctx expr = 
  ignore @@ get_well_formed_type ctx expr 
  (* match expr.data with
  | Ast.Kind -> ()
  | _ -> ignore @@ get_well_formed_type ctx expr *)