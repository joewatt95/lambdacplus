(* I don't think this is very correct. *)
let check_beta_equality expr1 expr2 =
  let rec check_beta_equality' expr1 expr2 =
    let open Ast in
    let open Parsing.Location in
    match expr1.data, expr2.data with
    | Type, Type -> true
    | Var index1, Var index2 -> index1 = index2
    | App {fn=fn1; arg=arg1}, App {fn=fn2; arg=arg2} ->
      check_beta_equality' fn1 fn2 && check_beta_equality' arg1 arg2 
    | Pi {input_type=in1; output_type=out1; _},
      Pi {input_type=in2; output_type=out2; _} ->
      check_beta_equality' in1 in2 && check_beta_equality' out1 out2
    | Fun {body=body1; _}, Fun {body=body2; _} ->
      check_beta_equality' body1 body2
    | _, _ -> assert false
  in
  check_beta_equality' expr1 expr2