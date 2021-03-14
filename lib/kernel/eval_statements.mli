val eval_stmt : Ast.stmt -> Context.t -> Ast.expr * Context.t

val eval_stmts : Ast.list_of_stmts -> Context.t -> Ast.expr * Context.t