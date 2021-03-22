(* Given a naming context and an expression in the internal de bruijn AST,
   unparse it to a readable string with variable names. *)
val unparse_internal_expr : Kernel.Context.t -> int Common.Ast.expr -> string