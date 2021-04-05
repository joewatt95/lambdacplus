open Containers
open Common

let eval_stmt (stmt : int Ast.stmt) ctx =
  let module Norm = Normalization in
  match stmt.data with
  | Ast.Check expr -> 
    Typing.infer ctx expr, ctx

  | Ast.Eval expr ->
    ignore @@ Typing.infer ctx expr;
    (* print_endline @@ Ast.show_expr Format.pp_print_int @@ Norm.normalize ctx expr; *)
    Norm.normalize ctx expr, ctx

  | Ast.Axiom {var_name; var_type} ->
    Typing.check_well_formed_type ctx var_type;
    let var_type = Norm.normalize ctx var_type in
    let ctx = Context.add_binding var_name ~var_type ctx in
    (* We need to get the type from the context instead of returning it directly
    because calling get_type shifts all the indices by 1 to account for the
    new var we just added to the context. *)
    Context.get_type ctx 0, ctx

  | Ast.Def {var_name; binding; ascribed_type} ->
    let var_type = 
      Typing.infer_annotation ~outer_expr:binding ctx binding ascribed_type in
    let binding = Norm.normalize ctx binding in
    let ctx =
      Context.add_binding var_name ~var_type ~binding ctx 
    in
    (* Here we also need to shift all the indices in the binding and type before
    we can return them. *)
    let expr = 0 |> Context.get_binding ctx |> Option.get_exn in 
    let ascribed_type = Context.get_type ctx 0 in
    Location.locate @@ Ast.Ascription {ascribed_type; expr}, ctx

let eval_stmts stmts ctx =
  List.fold_left 
    (fun (_, ctx) stmt -> eval_stmt stmt ctx)
    (Ast.located_kind, ctx)
    stmts