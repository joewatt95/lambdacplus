(* Given a statement and context, evaluate the statement.
   The newly modified context and resultant expression are returned as a tuple. *)
val eval_stmt : Ast.stmt -> Context.t -> Ast.expr * Context.t

(* Like above, but instead evaluate a list of statements instead of just a single one. *)
val eval_stmts : Ast.list_of_stmts -> Context.t -> Ast.expr * Context.t