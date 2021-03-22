(* Normalize an expression with respect to a context. *)
val normalize : Context.t -> int Common.Ast.expr -> int Common.Ast.expr

(* (beta_reduce body arg) performs the beta reduction operation on body.
   This works by walking down the AST of body and replacing all variables of
   de bruijn index 0 with arg.
   This function is also used for type checking and inference with the Pi type. *)
val beta_reduce : int Common.Ast.expr -> int Common.Ast.expr -> int Common.Ast.expr

val subst : int -> int Common.Ast.expr -> int Common.Ast.expr -> int Common.Ast.expr