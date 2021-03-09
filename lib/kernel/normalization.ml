open Containers

(* Implementation of shift, subst and normalize for expressions.
   These closely follow those found in the Types and Programming Languages book,
   with adjustments made to handle the dependent Pi type.

   The key idea is that shifting need only be done whenever we go underneath a
   binder, so we don't shift when we handle the input_type in Pi.
*)

let shift shift_by (expr : Ast.expr) =
  let rec shift_raw_expr cutoff raw_expr =
    let open Ast in
    match raw_expr with
    | Type -> Type
    | Var index as var ->
      if index >= cutoff then Var (index + shift_by) else var
    | Pi {input_var; input_type; output_type} ->
      let input_type = shift_expr cutoff input_type in
      let output_type = shift_expr (cutoff + 1) output_type in
      Pi {input_var; input_type; output_type}
    | Fun {input_var; body} ->
      let body = shift_expr (cutoff + 1) body in
      Fun {input_var; body}
    | App {fn; arg} ->
      let fn = shift_expr cutoff fn in
      let arg = shift_expr cutoff arg in
      App {fn; arg}
    | Ascription {expr; expr_type} ->
      let expr = shift_expr cutoff expr in
      let expr_type = shift_expr cutoff expr_type in
      Ascription {expr; expr_type}

  and shift_expr cutoff expr =
    Parsing.Location.update_data expr @@ shift_raw_expr cutoff

  in shift_expr 0 expr

let rec subst_raw_expr from_index to_expr raw_expr =
  let open Ast in
  match raw_expr with
  | Type -> Type
  (* | Var index as var ->
   *   let open Parsing.Location in
   *   if index = from_index then to_expr.data else var *)
  | Pi {input_var; input_type; output_type} ->
    let input_type = subst from_index to_expr input_type in
    let output_type = subst (from_index + 1) (shift 1 to_expr) output_type in
    Pi {input_var; input_type; output_type}
  | Fun {input_var; body} ->
    let body = subst (from_index + 1) (shift 1 to_expr) body in
    Fun {input_var; body}
  | App {fn; arg} ->
    let fn = subst from_index to_expr fn in
    let arg = subst from_index to_expr arg in
    App {fn; arg}
  | Ascription {expr; expr_type} ->
    let expr = subst from_index to_expr expr in
    let expr_type = subst from_index to_expr expr_type in
    Ascription {expr; expr_type}
  | Var _ -> assert false

and subst from_index (to_expr : Ast.expr) expr =
  match expr with
  | {data = Var index; _} ->
    if index = from_index then to_expr else expr
  | _ ->
    Parsing.Location.update_data expr @@ subst_raw_expr from_index to_expr

(* TODO *)
(* let rec normalize ctx expr =
 *   match expr with
 *   | _ -> assert false *)
