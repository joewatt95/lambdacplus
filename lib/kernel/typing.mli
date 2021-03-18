exception Pi_expected of {
  app : Ast.expr; 
  fn: Ast.expr; 
  inferred_type : Ast.expr
}

val check : Context.t -> Ast.expr -> Ast.expr -> unit

val infer : Context.t -> Ast.expr -> Ast.expr

val check_well_formed_type : Context.t -> Ast.expr -> unit