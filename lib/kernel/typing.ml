open Containers

module Loc = Common.Location
module Norm = Normalization

exception Pi_expected of {
  app : Ast.expr; 
  fn : Ast.expr; 
  inferred_type : Ast.expr
}

exception Cannot_infer_type_of_fn of Ast.expr
exception Cannot_infer_type_of_pair of Ast.expr
exception Cannot_infer_type_of_kind of Loc.source_loc

exception Ill_formed_type of {
  expr : Ast.expr;
  inferred_type : Ast.expr;
}

exception Type_mismatch of {
  outer_expr : Ast.expr;
  expr : Ast.expr;
  inferred_type : Ast.expr;
  expected_type : Ast.expr
}

let rec infer ctx (expr : Ast.expr) =
 match expr.data with
 | Ast.Type -> Loc.set_data expr Ast.Kind
 | Ast.Var index -> Context.get_type ctx index

 (* Do we want to allow users to assert that the type of Type, and type
    constructors is Kind? *)
 | Ast.Ascription {expr=expr'; ascribed_type} ->
  begin
    match ascribed_type.data with
    | Ast.Kind -> 
      check ~outer_expr:expr ctx expr' Ast.located_kind; 
      ascribed_type
    | _ ->
      check_well_formed_type ctx ascribed_type;
      let ascribed_type = Norm.normalize ctx ascribed_type in
      check ~outer_expr:expr ctx expr' ascribed_type;
      ascribed_type
  end

 (* | Ast.Pi {var_name=input_var; expr=input_type; body=output_type} ->
  check_well_formed_type ctx input_type;
  let input_type = Norm.normalize ctx input_type in
  let ctx = Context.add_binding input_var ~var_type:input_type ctx in
  get_well_formed_type ctx output_type *)

 | Ast.App {fn; arg} ->
  let inferred_type = infer ctx fn in
  begin
    match inferred_type.data with
    | Ast.Pi {expr=input_type; body=output_type; _} ->
      check ~outer_expr:expr ctx arg input_type;
      Norm.beta_reduce output_type arg
    | _ -> raise @@ Pi_expected {app=expr; fn; inferred_type}
  end

 | Ast.Fun {input_var; input_type=Some input_ty; body} ->
  check_well_formed_type ctx input_ty;
  let input_ty = Norm.normalize ctx input_ty in
  let ctx = Context.add_binding input_var ~var_type:input_ty ctx in
  let output_type = infer ctx body in
  (* What source location info should be used here? *)
  Loc.set_data expr @@ Ast.Pi {var_name=input_var; expr=input_ty; body=output_type}

 | Ast.Let {var_name; expr=binding; body} ->
  let var_type = infer ctx binding in
  let ctx = Context.add_binding var_name ~var_type:var_type ctx in
  body 
  |> infer ctx
  |> Fun.flip Norm.beta_reduce binding
  |> Norm.normalize ctx

 | Ast.Pi abstraction
 | Ast.Sigma abstraction-> infer_pi_sigma ctx abstraction

 | Ast.Fst expr ->
  let inferred_type = infer ctx expr in
  begin
    match inferred_type.data with
    | Sigma {expr=expr1_type; _} -> expr1_type
    | _ -> assert false
  end
 | Ast.Snd expr ->
  let inferred_type = infer ctx expr in
  begin
    match inferred_type.data with
    | Sigma {expr=expr1_type; body=expr2_type; _} ->
      expr2_type
      |> Fun.flip Norm.beta_reduce expr1_type
      |> Norm.normalize ctx
    | _ -> assert false
  end

 | Ast.Kind -> raise @@ Cannot_infer_type_of_kind expr.source_loc
 | Ast.Pair _ -> raise @@ Cannot_infer_type_of_pair expr
 | Ast.Fun {input_type=None; _} -> raise @@ Cannot_infer_type_of_fn expr

and infer_pi_sigma ctx ({var_name; expr=type1; body=type2} : Ast.abstraction) =
  check_well_formed_type ctx type1;
  let type1 = Norm.normalize ctx type1 in
  let ctx = Context.add_binding var_name ~var_type:type1 ctx in
  get_well_formed_type ctx type2
  
and check ~outer_expr ctx expr expected_type =
  match expr.data, expected_type.data with
  | Ast.Fun {input_var; input_type=None; body},
    Ast.Pi {expr=input_type; body=output_type; _} ->
    ctx 
    |> Context.add_binding input_var ~var_type:input_type
    |> fun ctx -> check ~outer_expr ctx body output_type

  | Ast.Pair {expr1; expr2},  
    Ast.Sigma {expr=expr1_type; body=expr2_type; _} ->
    check ctx ~outer_expr expr1 expr1_type;
    check ctx ~outer_expr expr2 @@ Norm.beta_reduce expr2_type expr1

  | _, _ ->
    let inferred_type = infer ctx expr in
    (* Here we need to check if the inferred type and expr_type are equal *)
    if not @@ Ast.equal_expr inferred_type expected_type
    then raise @@ Type_mismatch {expr; inferred_type; expected_type; outer_expr}

(* Check if expr is a well-formed type wrt ctx. In other words, this checks
  if the type of expr is Type or Kind. If so, we return it. Otherwise, we throw
  an exception. *)
and get_well_formed_type ctx expr = 
  let inferred_type = infer ctx expr in
  match inferred_type.data with
  | Ast.Type | Ast.Kind -> inferred_type
  | _ -> raise @@ Ill_formed_type {expr; inferred_type}

and check_well_formed_type ctx expr = 
  ignore @@ get_well_formed_type ctx expr 
  (* match expr.data with
  | Ast.Kind -> ()
  | _ -> ignore @@ get_well_formed_type ctx expr *)