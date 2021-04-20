open Containers
open Common

let subst from_index to_expr =
  let v =
    let open Location in
    object (self)
      inherit [_] Common.Ast.ast_mapper
      
      method! visit_Var {data=(from_index, to_expr); _} index =
        if index = from_index then to_expr.data else Var index

      method! visit_Fun from_to input_var input_type body =
        let input_type = Option.map (self#visit_expr from_to) input_type in
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
      inherit [_] Ast.ast_mapper as super

      (* Lookup the variable in the context. If it's there, take the binding.
      Otherwise, return the original variable *)
      method! visit_Var {data=ctx; _} index =
        index
        |> Context.get_binding ctx
        |> Option.map Location.data
        |> Option.get_or ~default:(Ast.Var index)

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
        let expr = self#visit_expr ctx expr in
        let normalize_branch expr branch =
          branch
          |> Ast.match_body
          |> Fun.flip beta_reduce expr
          |> Location.data
          |> self#visit_raw_expr ctx
        in expr
           |> Location.data
           |> begin function 
              | Ast.Inl expr -> normalize_branch expr inl
              | Ast.Inr expr -> normalize_branch expr inr
             (* let inl_body = beta_reduce inl.match_body expr in
             self#visit_raw_expr ctx inl_body.data *)
           (* | Ast.Inr expr ->
             let inr_body = beta_reduce inr.match_body expr in
             self#visit_raw_expr ctx inr_body.data *)
              (* Handle the case when expr normalizes to a neutral term. *)
              | _ ->
                let inl = self#visit_match_binding ctx inl in
                let inr = self#visit_match_binding ctx inr in
                Ast.Match {expr; inl; inr}
          end

      method! visit_Exists_elim ctx expr witness_var witness_cert body =
        let expr = self#visit_expr ctx expr in
        expr
        |> Location.data
        |> begin function
           | Ast.Exists_pair {left; right} ->
            body
            |> Fun.flip beta_reduce left
            |> Fun.flip beta_reduce right
            |> Location.data
            |> self#visit_raw_expr ctx
           | _ ->
            body
            |> super#visit_Exists_elim_body ctx witness_var witness_cert
            |> fun body -> Ast.Exists_elim {expr; witness_var; witness_cert; body}
           end

      (* Handy method for adding bindings to contexts wrapped with a location. *)
      method add_binding input_var ctx =
          Location.update_data ctx @@ Context.add_binding input_var

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