open Containers

(* Implementation of Ast.shift, subst and normalize for expressions.
   These closely follow those found in the Types and Programming Languages book,
   with adjustments made to handle the dependent Pi type.

   The key idea is that Ast.shifting need only be done whenever we go underneath a
   binder, so we don't Ast.shift when we handle the input_type in Pi.
*)

module Loc = Common.Location

let subst from_index to_expr =
  let v =
    let open Loc in
    object (self)
      inherit [_] Ast.ast_mapper
      
      method! visit_Var {data=(from_index, to_expr); _} index =
        if index = from_index then to_expr.data else Var index

      method! visit_Pi from_to input_var input_type output_type =
        let input_type = self#visit_expr from_to input_type in
        let output_type = self#subst_under_binder from_to output_type in
        Ast.Pi {input_var; input_type; output_type}

      method! visit_Fun from_to input_var input_type body =
        let input_type = CCOpt.map (self#visit_expr from_to) input_type in
        let body = self#subst_under_binder from_to body in
        Ast.Fun {input_var; input_type; body}

      method! visit_Let from_to var_name binding body =
        let binding = self#visit_expr from_to binding in
        let body = self#subst_under_binder from_to body in
        Ast.Let {var_name; binding; body}
      
      (* Handy helper function for performing substitutions under binders.
         These include lambda and Pi. *)
      method subst_under_binder {data=(from_index, to_expr); _} = 
        to_expr
        |> Ast.shift 1
        |> fun to_expr -> self#visit_expr @@ Loc.locate (from_index + 1, to_expr)

      (* Note that we don't need to override the default methods that the Visitors
         package generates for visit_Type, visit_Kind, visit_App and 
         visit_Ascription since we only need special rules for handle variables 
         and substituing under binders differently. *)

  end in v#visit_expr @@ Loc.locate (from_index, to_expr)

let beta_reduce body arg =
    let arg = Ast.shift 1 arg in
    body 
    |> subst 0 arg 
    |> Ast.shift (-1)

(* let normalize ctx =
  let v =
    object (self)
      inherit [_] Ast.ast_mapper

      method! visit_Var {data=ctx; _} index =
        index
        |> Context.get_binding ctx
        |> CCOpt.map Loc.data
        |> CCOpt.get_or ~default:(Ast.Var index)

      method! visit_Ascription ctx expr _ =
        self#visit_raw_expr ctx expr.data

      method! visit_App ctx fn arg =
        match self#visit_raw_expr ctx fn.data with
        | Ast.Fun {body; _} ->
          let arg = self#visit_expr ctx arg in
          let body = beta_reduce body arg in
          self#visit_raw_expr ctx body.data
        | _ -> Ast.App {fn; arg} 

      method! visit_Fun ctx input_var _ body =
        let ctx =  self#add_binding input_var ctx in
        let body = self#visit_expr ctx body in
        Ast.Fun {input_type=None; input_var; body}

      method! visit_Pi ctx input_var input_type output_type =
        let input_type = self#visit_expr ctx input_type in
        let ctx = self#add_binding input_var ctx in 
        let output_type = self#visit_expr ctx output_type in
        Ast.Pi {input_var; input_type; output_type}

      method add_binding input_var ctx =
          Loc.update_data ctx @@ fun ctx -> Context.add_binding input_var ctx

      method! visit_Let ctx _ binding body =
        let binding = self#visit_expr ctx binding in
        let body = beta_reduce body binding in
        self#visit_raw_expr ctx body.data

  end in v#visit_expr @@ Loc.locate ctx *)

let rec normalize ctx (expr : Ast.expr) =
  match expr.data with
  | Ast.Type | Ast.Kind -> expr
  | Ast.Var index -> 
    index
    |> Context.get_binding ctx
    |> CCOpt.get_or ~default:expr
  
  | Ast.Ascription {expr; _} -> normalize ctx expr

  | Ast.App {fn; arg} ->
    begin
      match (normalize ctx fn).data with
      | Ast.Fun {body; _} ->
        let arg = normalize ctx arg in
        let body = beta_reduce body arg in
        normalize ctx body
      | Var _ -> expr
      | _ -> assert false
    end

  | Ast.Fun {input_var; body; _} ->
    let ctx = Context.add_binding input_var ctx in
    let body = normalize ctx body in
    Loc.set_data expr @@ Ast.Fun {input_type=None; input_var; body}

  | Ast.Pi {input_var; input_type; output_type} ->
    let input_type = normalize ctx input_type in
    let ctx = Context.add_binding input_var ctx in 
    let output_type = normalize ctx output_type in
    Loc.set_data expr @@ Ast.Pi {input_var; input_type; output_type}

  | Ast.Let {binding; body; _} ->
    let binding = normalize ctx binding in
    let body = beta_reduce body binding in
    normalize ctx body