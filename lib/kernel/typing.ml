open Containers
open Common

module Norm = Normalization

exception Cannot_infer_type of int Ast.expr

exception Ill_formed_type of {
  expr : int Ast.expr;
  inferred_type : int Ast.expr;
}

type expected_type =
  | Exact of int Ast.expr
  | Family of string

exception Type_mismatch of {
  outer_expr : int Ast.expr;
  expr : int Ast.expr;
  inferred_type : int Ast.expr;
  expected_type : expected_type;
}

(* Compare syntactic equality of de bruijn ASTs. *)
let equal_expr = Ast.equal_expr Stdlib.(=)

let rec infer ctx (expr : int Ast.expr) =
 match expr.data with
 | Ast.Type -> Location.set_data expr Ast.Kind
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

 | Ast.App {left=fn; right=arg} ->
  let inferred_type = infer ctx fn in
  begin
    match inferred_type.data with
    | Ast.Pi {expr=input_type; body=output_type; _} ->
      check ~outer_expr:expr ctx arg input_type;
      Norm.beta_reduce output_type arg
    | _ -> 
      raise @@ Type_mismatch
        {expr=fn; outer_expr=expr; inferred_type; expected_type=Family "Pi"}
  end

 | Ast.Fun {input_var; input_type=Some input_ty; body} ->
  check_well_formed_type ctx input_ty;
  let input_ty = Norm.normalize ctx input_ty in
  let ctx = Context.add_binding input_var ~var_type:input_ty ctx in
  let output_type = infer ctx body in
  (* What source location info should be used here? *)
  Location.set_data expr @@ Ast.Pi {var_name=input_var; expr=input_ty; body=output_type}

 | Ast.Let {var_name; expr=binding; body} ->
  let var_type = infer ctx binding in
  let ctx = Context.add_binding var_name ~var_type:var_type ctx in
  body 
  |> infer ctx
  |> Fun.flip Norm.beta_reduce binding
  |> Norm.normalize ctx

 | Ast.Pi abstraction | Ast.Sigma abstraction-> infer_pi_sigma ctx abstraction

 | Ast.Fst _ | Ast.Snd _ ->
  expr
  |> infer_sigma_exn ctx
  |> begin
      function
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

 | Ast.Match {expr; inl; inr} ->
  let inferred_type = infer ctx expr in
  begin
    match inferred_type.data with
    | Sum {left; right} ->
      let infer_match_binding ({match_var; match_body} : int Ast.match_binding) ty =
        ctx
        |> Context.add_binding match_var ~var_type:ty
        |> Fun.flip infer match_body
      in
      let inl_type = infer_match_binding inl left in
      let inr_type = infer_match_binding inr right in
      if equal_expr inl_type inr_type 
        (* It's important to shift the indices by -1 here to avoid an off-by-one
           bug. *)
      then Ast.shift (-1) inl_type
      else assert false
    | _ -> raise @@ Type_mismatch
            {expr; outer_expr=expr; inferred_type; expected_type=Family "Sum"}
  end

  (* | Ast.Let_pair {left_var; right_var; binding; body} ->
    expr
    |> infer_sigma_exn ctx 
    |> begin 
        function
        | {expr=left_type; body=right_type; _} ->
          let right_type = binding
                           |> fun expr -> Ast.Fst expr
                           |> Location.locate
                           |> Norm.beta_reduce right_type
                           |> Norm.normalize ctx 
          in
          let ctx = ctx
                    |> Context.add_binding left_var ~var_type:left_type
                    |> Context.add_binding right_var ~var_type:right_type          
          in body
             |> infer ctx
             |> fun x ->
                print_endline @@ Ast.show_expr Format.pp_print_int x;
                print_endline @@ Context.show ctx;
                x
             |> Ast.shift (-2)
       end
  *)

 | Ast.Kind | Ast.Pair _ | Ast.Fun {input_type=None; _} | Ast.Inl _ | Ast.Inr _ -> 
  raise @@ Cannot_infer_type expr

and infer_sigma_exn ctx (outer_expr : int Ast.expr) =
  match outer_expr.data with
  | Ast.Fst expr | Ast.Snd expr ->
    let inferred_type = infer ctx expr in
    begin
      match inferred_type.data with
      |  Sigma abstraction -> abstraction
      | _ -> 
        raise @@ Type_mismatch 
          {outer_expr; expr; inferred_type; expected_type=Family "Sigma"}
    end
  | _ -> assert false

and infer_pi_sigma ctx ({var_name; expr=type1; body=type2} : int Ast.abstraction) =
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

  | Ast.Pair {left; right},  
    Ast.Sigma {expr=left_type; body=right_type; _} ->
    check ctx ~outer_expr left left_type;
    right_type
    |> Fun.flip Norm.beta_reduce left
    |> Norm.normalize ctx
    |> check ctx ~outer_expr right 
    (* check ctx ~outer_expr expr2 @@ Norm.normalize ctx @@ Norm.beta_reduce expr2_type expr1 *)

  | Ast.Inl expr, Sum {left=expected_type; _}
  | Ast.Inr expr, Sum {right=expected_type; _} -> 
    check ctx ~outer_expr expr expected_type;

  | _, _ ->
    let inferred_type = infer ctx expr in
    (* Here we need to check if the inferred type and expr_type are equal *)
    if not @@ equal_expr inferred_type expected_type
    then 
      let expected_type = Exact expected_type in
      raise @@ Type_mismatch {expr; inferred_type; expected_type; outer_expr}

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