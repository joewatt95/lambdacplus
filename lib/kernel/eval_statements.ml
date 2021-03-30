open Common

module Norm = Normalization

let eval_stmt (stmt : int Ast.stmt) ctx =
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
    let ctx = Context.add_binding var_name ~var_type:var_type ctx in
    var_type, ctx

  | Ast.Def {var_name; binding; ascribed_type=None} ->
    let inferred_type = Typing.infer ctx binding in
    let binding = Norm.normalize ctx binding in
    (* let return_val = 
      Parsing.Location.locate @@
        Ast.Ascription {expr=binding; expr_type=inferred_type} in *)
    let ctx =
      Context.add_binding var_name ~var_type:inferred_type ~binding:binding ctx 
    in
    (* print_endline @@ Context.show ctx;
    print_endline @@ Ast.show_expr Format.pp_print_int binding; *)
    binding, ctx

  | Ast.Def {var_name; binding; ascribed_type=Some ascribed_type} ->
    let inferred_type = Typing.infer_annotation ~outer_expr:binding ctx binding ascribed_type in
    let binding = Norm.normalize ctx binding in
    let ctx =
      Context.add_binding var_name ~var_type:inferred_type ~binding:binding ctx 
    in
    binding, ctx

let eval_stmts stmts ctx =
  List.fold_left 
    (fun (_, ctx) stmt -> eval_stmt stmt ctx)
    (Ast.located_kind, ctx)
    stmts