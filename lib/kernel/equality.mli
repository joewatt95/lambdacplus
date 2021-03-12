(* Check beta equality of 2 expressions. Both expressions
assumed to have been normalized to full head normal form. *)
val check_beta_equality : Ast.expr -> Ast.expr -> bool