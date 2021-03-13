open Containers

(* Implementation of shift, subst and normalize for expressions.
   These closely follow those found in the Types and Programming Languages book,
   with adjustments made to handle the dependent Pi type.

   The key idea is that shifting need only be done whenever we go underneath a
   binder, so we don't shift when we handle the input_type in Pi.
*)

module Loc = Parsing.Location

let shift shift_by (expr : Ast.expr) =
  let rec shift_raw_expr cutoff raw_expr =
    match raw_expr with
    | Ast.Type -> Ast.Type

    | Ast.Var index as var ->
      if index >= cutoff then Ast.Var (index + shift_by) else var

    | Ast.Pi {input_var; input_type; output_type} ->
      let input_type = shift_expr cutoff input_type in
      let output_type = shift_expr (cutoff + 1) output_type in
      Ast.Pi {input_var; input_type; output_type}

    | Ast.Fun {input_var; input_type; body} ->
      let input_type = CCOpt.map (shift_expr cutoff) input_type in
      let body = shift_expr (cutoff + 1) body in
      Ast.Fun {input_var; input_type; body}

    | Ast.App {fn; arg} ->
      let fn = shift_expr cutoff fn in
      let arg = shift_expr cutoff arg in
      Ast.App {fn; arg}

    | Ast.Ascription {expr; expr_type} ->
      let expr = shift_expr cutoff expr in
      let expr_type = shift_expr cutoff expr_type in
      Ast.Ascription {expr; expr_type}

  and shift_expr cutoff expr =
    Loc.update_data expr @@ shift_raw_expr cutoff

  in shift_expr 0 expr

let rec subst_raw_expr from_index to_expr raw_expr =
  match raw_expr with
  | Ast.Type -> Ast.Type
  (* | Var index as var ->
   *   let open Loc in
   *   if index = from_index then to_expr.data else var *)
  | Ast.Pi {input_var; input_type; output_type} ->
    let input_type = subst from_index to_expr input_type in
    let output_type = subst (from_index + 1) (shift 1 to_expr) output_type in
    Ast.Pi {input_var; input_type; output_type}

  | Ast.Fun {input_var; input_type; body} ->
    let input_type = CCOpt.map (subst from_index to_expr) input_type in
    let body = subst (from_index + 1) (shift 1 to_expr) body in
    Ast.Fun {input_var; input_type; body}

  | Ast.App {fn; arg} ->
    let fn = subst from_index to_expr fn in
    let arg = subst from_index to_expr arg in
    Ast.App {fn; arg}

  | Ast.Ascription {expr; expr_type} ->
    let expr = subst from_index to_expr expr in
    let expr_type = subst from_index to_expr expr_type in
    Ast.Ascription {expr; expr_type}

  (* This case should never happen because it's already taken care of
  in subst*)
  | Ast.Var _ -> assert false

and subst from_index to_expr expr =
  match expr.data with
  | Ast.Var index ->
    if index = from_index then to_expr else expr
  | _ ->
    Loc.update_data expr @@ subst_raw_expr from_index to_expr

let beta_reduce body arg =
    let arg = shift 1 arg in
    body |> subst 0 arg |> shift (-1)

let rec normalize ctx (expr : Ast.expr) =
  match expr.data with
  | Ast.Type -> expr
  | Ast.Var index -> 
      let binding = Context.get_binding index ctx in
      CCOpt.get_or binding ~default:expr
  
  | Ast.Ascription {expr; _} -> normalize ctx expr

  | Ast.App {fn; arg} ->
      begin
        match (normalize ctx fn).data with
        | Ast.Fun {body; _} ->
          let arg = normalize ctx arg in 
          beta_reduce body arg
        | _ -> expr
      end

  | Ast.Fun {input_var; body; _} ->
      let ctx = Context.add_binding input_var ctx in
      let body = normalize ctx body in
      Loc.set_data expr @@ Ast.Fun {input_var; input_type=None; body}

  | Ast.Pi {input_var; input_type; output_type} ->
    let input_type = normalize ctx input_type in
    let ctx = Context.add_binding input_var ctx in 
    let output_type = normalize ctx output_type in
    Loc.set_data expr @@ Ast.Pi {input_var; input_type; output_type}