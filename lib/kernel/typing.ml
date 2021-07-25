open Containers
open Common

(* This module implements the typing judgments. *)
module Norm = Normalization

type expr_with_ctx = {expr : int Ast.expr; ctx : Context.t}

exception Cannot_infer_type of int Ast.expr

exception Ill_formed_type of {
  expr : int Ast.expr;
  inferred_type : expr_with_ctx;
}

type expected_type =
  | Exact of int Ast.expr
  | Family of string

exception Type_mismatch of {
  outer_expr : int Ast.expr;
  expr : int Ast.expr;
  inferred_type : expr_with_ctx;
  expected_type : expected_type;
}

exception Type_mismatch_in_match of {
  outer_expr : int Common.Ast.expr;
  inl_type : expr_with_ctx;
  inr_type : expr_with_ctx;
}

exception Type_contains_free_var of {
  outer_expr : int Common.Ast.expr;
  inferred_type : expr_with_ctx;
  free_var : string
}

(* Compare syntactic equality of de bruijn ASTs. *)
let equal_expr = Ast.equal_expr Stdlib.(=)

let rec infer ctx (expr : int Ast.expr) =
 match expr.data with
 | Ast.Type -> Location.set_data expr Ast.Kind
 | Ast.Var index -> Context.get_type ctx index

 | Ast.Ascription {expr=inner_expr; ascribed_type} ->
  infer_annotation ~outer_expr:expr ctx inner_expr @@ Some ascribed_type

 | Ast.App {left=fn; right=arg} ->
  let inferred_type = infer ctx fn in
  begin match inferred_type.data with
  | Ast.Pi {expr=input_type; body=output_type; _} ->
    check ~outer_expr:expr ctx arg input_type;
    arg
    |> Norm.beta_reduce output_type
    |> Norm.normalize ctx
  | _ -> 
    raise @@ Type_mismatch
      {expr=fn; outer_expr=expr; inferred_type={expr=inferred_type; ctx}; 
       expected_type=Family "Pi"}
  end

 | Ast.Fun {input_var; input_type=Some input_ty; body} ->
  check_well_formed_type ctx input_ty;
  let input_ty = Norm.normalize ctx input_ty in
  let ctx = Context.add_binding input_var ~var_type:input_ty ctx in
  let output_type = infer ctx body in
  (* What source location info should be used here? *)
  Location.set_data expr @@ Ast.Pi 
    {var_name=input_var; expr=input_ty; body=output_type}

 | Ast.Let {abstraction={var_name; expr=binding; body}; ascribed_type} ->
  let var_type = infer_annotation ~outer_expr:expr ctx binding ascribed_type in 
  let binding = Norm.normalize ctx binding in
  let ctx = Context.add_binding var_name ~var_type:var_type ~binding ctx in
  body 
  |> infer ctx
  |> Fun.flip Norm.beta_reduce binding

  (* Pi/Sigma/Exists type formation *)
 | Ast.Pi abstraction | Ast.Sigma abstraction | Ast.Exists abstraction ->
  infer_pi_sigma_exists ctx abstraction

 | Ast.Fst _ | Ast.Snd _ ->
  expr
  |> infer_sigma_exn ctx
  |> begin function
     | ({expr=left_type; body=right_type; _} : int Ast.abstraction) ->
       match expr.data with
       | Ast.Fst _ -> left_type
       | Ast.Snd expr ->
         expr
         |> fun expr -> Ast.Fst expr
         |> Location.locate
         |> Norm.beta_reduce right_type
         |> Norm.normalize ctx

       (* No other cases are possible since expr came from the outer match
       where we already guaranteed that it will be a Ast.Fst or 
       Ast.Snd. *)
       | _ -> assert false
     end

 | Ast.Sum {left; right} ->
  let check_type expr = check ~outer_expr:expr ctx expr @@ Location.locate Ast.Type in
  check_type left; check_type right;
  Location.set_data expr Ast.Type

 | Ast.Match {expr=expr'; inl; inr} ->
  let inferred_type = infer ctx expr' in
  begin match inferred_type.data with
  | Sum {left; right} ->
    let infer_match_binding ({match_var; match_body} : int Ast.match_binding) ty =
      ctx
      |> Context.add_binding match_var ~var_type:ty
      |> Fun.flip infer match_body
      (* It's important to shift the indices by -1 here to avoid an off-by-one
      bug. This is because we added a binding to the context when inferring the
      type of the body. *)
      |> Ast.shift (-1)
    in
    (* Infer the return type in both branches. *)
    let inl_type = infer_match_binding inl left in
    let inr_type = infer_match_binding inr right in
    if equal_expr inl_type inr_type
    then inl_type
    else 
      let inl_type = {expr=inl_type; ctx} in
      let inr_type = {expr=inr_type; ctx} in
      raise @@ Type_mismatch_in_match { outer_expr=expr; inl_type; inr_type }
  | _ -> raise @@ Type_mismatch
          {expr=expr'; outer_expr=expr; inferred_type={expr=inferred_type; ctx}; 
           expected_type=Family "Sum"}
  end

  | Ast.Exists_elim {expr=expr'; witness_var; witness_cert; body; _} ->
    let inferred_type = infer ctx expr' in
    begin match inferred_type.data with
    | Ast.Exists {expr=witness_type; body=witness_prop; _} ->
      let ctx = ctx
                |> Context.add_binding witness_var ~var_type:witness_type
                |> Context.add_binding witness_cert ~var_type:witness_prop
      in 
      let body_type = infer ctx body in
      let raise_err_if_index_found index = 
        let free_var = Context.index_to_var_name ctx index in
        let raise_free_var_err _ = raise @@ Type_contains_free_var
          {outer_expr=expr; free_var; inferred_type={expr=body_type; ctx}}  
        in
        raise_free_var_err
        |> Ast.do_if_index_present index
        |> Fun.tap
      in
      body_type
      |> raise_err_if_index_found 1
      |> raise_err_if_index_found 0
      (* Decrement the indices by -2 since we added 2 things to the context
      when inferring the type of the body. *)
      |> Ast.shift (-2)
    | _ -> raise @@ Type_mismatch
            { expr=expr'; outer_expr=expr; inferred_type={expr=inferred_type; ctx}; 
              expected_type=Family "Sum" }
    end

 | Ast.Kind | Ast.Pair _ | Ast.Exists_pair _ | Ast.Fun {input_type=None; _}
 | Ast.Inl _ | Ast.Inr _ -> raise @@ Cannot_infer_type expr

(* This used to ensure that the expression that Fst and Snd are applied to is
really a Sigma. *)
and infer_sigma_exn ctx (outer_expr : int Ast.expr) =
  match outer_expr.data with
  | Ast.Fst expr | Ast.Snd expr ->
    let inferred_type = infer ctx expr in
    begin match inferred_type.data with
    |  Sigma abstraction -> abstraction
    | _ -> 
      raise @@ Type_mismatch 
        {outer_expr; expr; inferred_type={expr=inferred_type; ctx}; 
         expected_type=Family "Sigma"}
    end

  (* This case should never occur since this function is only used to infer
  the type of Fst and Snd. *)
  | _ -> assert false

(* Check if a Pi/Sigma/Exists is well formed. If it is, we return the sort of
  the type. If it's not, an exception is thrown. *)
and infer_pi_sigma_exists ctx ({var_name; expr=type1; body=type2} : int Ast.abstraction) =
  check_well_formed_type ctx type1;
  let type1 = Norm.normalize ctx type1 in
  let ctx = Context.add_binding var_name ~var_type:type1 ctx in
  get_well_formed_type ctx type2

and infer_annotation ~outer_expr ctx expr ascribed_type =
(* If `ascribed_type` is Some, check that it's well formed and that `expr` has
that type. Otherwise, try to infer the type of `expr` from `ctx` directly. *)
  ascribed_type
  |> Option.map @@ 
    begin fun (ascribed_type : int Ast.expr) ->
      match ascribed_type.data with
      (* Here we allow for Kind to be in annotations. *)
      | Ast.Kind ->
        check ~outer_expr ctx expr Ast.located_kind; 
        ascribed_type
      | _ ->
        ascribed_type
        |> Fun.tap @@ check_well_formed_type ctx
        |> Norm.normalize ctx
        |> Fun.tap @@ check ~outer_expr:expr ctx expr
    end
  |> Option.get_lazy @@ fun _ -> infer ctx expr
  
and check ~outer_expr ctx expr expected_type =
  match expr.data, expected_type.data with
  | Ast.Fun {input_var; input_type=None; body},
    Ast.Pi {expr=input_type; body=output_type; _} ->
    ctx 
    |> Context.add_binding input_var ~var_type:input_type
    |> fun ctx -> check ~outer_expr ctx body output_type

  | Ast.Pair {left; right}, Ast.Sigma abstraction
  | Ast.Exists_pair {left; right}, Ast.Exists abstraction ->
    begin match abstraction with {expr=left_type; body=right_type; _} ->
      check ctx ~outer_expr left left_type;
      right_type
      |> Fun.flip Norm.beta_reduce left
      |> Norm.normalize ctx
      |> check ctx ~outer_expr right 
      (* check ctx ~outer_expr expr2 @@ Norm.normalize ctx @@ Norm.beta_reduce expr2_type expr1 *)
    end

  | Ast.Inl expr, Sum {left=expected_type; _}
  | Ast.Inr expr, Sum {right=expected_type; _} -> 
    check ctx ~outer_expr expr expected_type;

  | _, _ ->
    let inferred_type = infer ctx expr in
    (* Here we need to check if the inferred type and expr_type are equal *)
    if not @@ equal_expr inferred_type expected_type
    then
      let expected_type = Exact expected_type in
      raise @@ Type_mismatch
        {expr; inferred_type={expr=inferred_type; ctx}; expected_type; outer_expr}

(* Check if `expr` is a well-formed type wrt `ctx`. In other words, this checks
  if the type of expr is Type or Kind. If so, we return it. Otherwise, we throw
  an exception. *)
and get_well_formed_type ctx expr =
  let inferred_type = infer ctx expr in
  match inferred_type.data with
  | Ast.Type | Ast.Kind -> inferred_type
  | _ -> raise @@ Ill_formed_type {expr; inferred_type={expr=inferred_type; ctx}}

and check_well_formed_type ctx expr = 
  ignore @@ get_well_formed_type ctx expr