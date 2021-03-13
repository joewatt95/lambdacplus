let rec check_beta_equality expr1 expr2 =
  let open Parsing.Location in
  match expr1.data, expr2.data with
  | Ast.Type, Ast.Type -> true

  | Ast.Var index1, Ast.Var index2 -> 
    (* print_endline @@ "Checking" ^ (string_of_int index1) ^ (string_of_int index2); *)
    index1 = index2

  | Ast.App {fn=fn1; arg=arg1}, Ast.App {fn=fn2; arg=arg2} ->
    check_beta_equality fn1 fn2 && check_beta_equality arg1 arg2

  | Ast.Pi {input_type=in1; output_type=out1; _},
    Ast.Pi {input_type=in2; output_type=out2; _} ->
    check_beta_equality in1 in2 && check_beta_equality out1 out2

  | Ast.Fun {body=body1; _}, Ast.Fun {body=body2; _} ->
    check_beta_equality body1 body2

  | _, _ -> assert false