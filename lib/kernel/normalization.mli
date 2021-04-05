(* Normalize an expression with respect to a context. *)
val normalize : Context.t -> int Common.Ast.expr -> int Common.Ast.expr

(* (beta_reduce body arg) performs the beta reduction operation on body.
   This works by walking down the AST of body and replacing all variables of
   de bruijn index 0 with arg.
   This function is used whenever we want to substitute under a binder. *)
val beta_reduce : int Common.Ast.expr -> int Common.Ast.expr -> int Common.Ast.expr

(* This is the standard substitution operation for de bruijn ASTs.
   (subst index expr to_expr) replaces all occurrences of the variable given
   by `index` in expr to the expression given by `to_expr`. *)
val subst : int -> int Common.Ast.expr -> int Common.Ast.expr -> int Common.Ast.expr