val parser_to_internal_expr : Parsing.Ast.expr -> Context.t -> Ast.expr

val parser_to_internal_stmt : Parsing.Ast.stmt -> Context.t -> Ast.stmt * Context.t

val parser_to_internal_stmts : Parsing.Ast.list_of_stmts -> Context.t -> Ast.list_of_stmts * Context.t

val internal_to_parser_expr : Ast.expr -> Context.t -> Parsing.Ast.expr
