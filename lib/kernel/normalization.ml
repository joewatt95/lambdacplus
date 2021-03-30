open Containers

(* Implementation of Ast.shift, subst and normalize for expressions.
   These closely follow those found in the Types and Programming Languages book,
   with adjustments made to handle the dependent Pi type.

   The key idea is that Ast.shifting need only be done whenever we go underneath a
   binder, so we don't Ast.shift when we handle the input_type in Pi.
*)
open Common

let subst from_index to_expr =
  let v =
    let open Location in
    object (self)
      inherit [_] Common.Ast.ast_mapper
      
      method! visit_Var {data=(from_index, to_expr); _} index =
        if index = from_index then to_expr.data else Var index

      method! visit_Fun from_to input_var input_type body =
        let input_type = CCOpt.map (self#visit_expr from_to) input_type in
        let body = self#subst_under_binder from_to body in
        Ast.Fun {input_var; input_type; body}

      method! visit_abstraction from_to {var_name; expr; body} =
        let expr = self#visit_expr from_to expr in
        let body = self#subst_under_binder from_to body in
        {var_name; expr; body}

      method! visit_match_binding from_to {match_var; match_body} =
        let match_body = self#subst_under_binder from_to match_body in
        {match_var; match_body}

      (* method! visit_Let_pair from_to left_var right_var binding body =
        let binding = self#visit_expr from_to binding in
        let body = self#subst_under_binder 2 from_to body in
        Ast.Let_pair {left_var; right_var; binding; body} *)

      (* Handy helper function for performing substitutions under binders.
         These include lambda and Pi. *)
      method subst_under_binder {data=(from_index, to_expr); _} = 
        to_expr
        |> Ast.shift 1
        |> fun to_expr -> self#visit_expr @@ Location.locate (from_index + 1, to_expr)
      
      (* Note that we don't need to override the default methods that the Visitors
         package generates for visit_Type, visit_Kind, visit_App and 
         visit_Ascription since we only need special rules for handle variables 
         and substituing under binders differently. *)

  end in v#visit_expr @@ Location.locate (from_index, to_expr)

let beta_reduce body arg =
    let arg = Ast.shift 1 arg in
    body 
    |> subst 0 arg 
    |> Ast.shift (-1)

let normalize ctx =
  let v =
    object (self)
      inherit [_] Ast.ast_mapper

      method! visit_Var {data=ctx; _} index =
        (* if Stdlib.(Context.index_to_var_name ctx index = "g")
        then Context.pretty_print ctx; *)
        index
        |> Context.get_binding ctx
        |> CCOpt.map Location.data
        |> CCOpt.get_or ~default:(Ast.Var index)

      method! visit_Ascription ctx expr _ =
        self#visit_raw_expr ctx expr.data

      method! visit_App ctx {left=fn; right=arg} =
        let fn = self#visit_expr ctx fn in
        let arg = self#visit_expr ctx arg in
        match fn.data with
        | Ast.Fun {body; _} ->
          let body = beta_reduce body arg in
          self#visit_raw_expr ctx body.data
        | _ -> Ast.App {left=fn; right=arg} 

      method! visit_Fun ctx input_var _ body =
        let ctx =  self#add_binding input_var ctx in
        let body = self#visit_expr ctx body in
        Ast.Fun {input_type=None; input_var; body}

      method! visit_abstraction ctx {var_name; expr; body} =
        let expr = self#visit_expr ctx expr in
        let ctx = self#add_binding var_name ctx in
        let body = self#visit_expr ctx body in
        {var_name; expr; body}

      method! visit_match_binding ctx {match_var; match_body} =
        let ctx = self#add_binding match_var ctx in
        let match_body = self#visit_expr ctx match_body in
        (* print_endline @@ Ast.show_expr Format.pp_print_int match_body; *)
        {match_var; match_body}

      method! visit_Match ctx expr inl inr =
        expr
        |> self#visit_expr ctx
        |> Location.data
        |> begin
            function 
            | Ast.Inl expr -> 
              let inl_body = beta_reduce inl.match_body expr in
              self#visit_raw_expr ctx inl_body.data
            | Ast.Inr expr ->
              let inr_body = beta_reduce inr.match_body expr in
              self#visit_raw_expr ctx inr_body.data
            | _ ->
                let inl = self#visit_match_binding ctx inl in
                let inr = self#visit_match_binding ctx inr in
                Ast.Match {expr; inl; inr}
          end

      method add_binding input_var ctx =
          Location.update_data ctx @@ fun ctx -> Context.add_binding input_var ctx

      method! visit_Let ctx {expr=binding; body; _} _ =
        (* let fn = Location.locate @@ Ast.Fun {input_type=None; input_var="dummy"; body} in 
        self#visit_App ctx {left=fn; right=binding} *)
        let binding = self#visit_expr ctx binding in
        let body = beta_reduce body binding in
        self#visit_raw_expr ctx body.data

      method! visit_Fst ctx expr =
        let expr = self#visit_expr ctx expr in
        match expr.data with
        | Ast.Pair {left; _} -> self#visit_raw_expr ctx left.data
        | _ -> Ast.Fst expr

      method! visit_Snd ctx expr =
        let expr = self#visit_expr ctx expr in
        match expr.data with
        | Ast.Pair {right;_} -> self#visit_raw_expr ctx right.data
        | _ -> Ast.Snd expr

      (* method! visit_Let_pair ctx _  _ binding body =
        let binding = self#visit_expr ctx binding in
        body
        |> Fun.flip beta_reduce @@ Location.locate @@ Ast.Fst binding 
        |> Fun.flip beta_reduce @@ Location.locate @@ Ast.Snd binding 
        |> Location.data
        |> self#visit_raw_expr ctx  *)

  end in v#visit_expr @@ Location.locate ctx

(* let rec normalize ctx (expr : Ast.expr) =
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
    normalize ctx body *)