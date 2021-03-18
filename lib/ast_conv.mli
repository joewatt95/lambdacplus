exception Unknown_var_name of Parsing.Ast.expr
exception Underscore_var_name of Parsing.Ast.expr

(* Convert a parsed expression to our internal AST. *)
val parser_to_internal_expr : Kernel.Context.t -> Parsing.Ast.expr -> Kernel.Ast.expr

(* Convert a parsed statement to our internal AST.
   Note that the return type here is actually (Ast.expr * ctx) because Def and
   Axiom will modify the context, affecting future statements.
   This is important to carry around in stmts_to_internal_ast when converting a
   list of statements to our internal AST.
*)
val parser_to_internal_stmt : Parsing.Ast.stmt -> Kernel.Context.t -> Kernel.Ast.stmt * Kernel.Context.t

(* This converts a list of parser statements to our internal AST. *)
val parser_to_internal_stmts : Parsing.Ast.list_of_stmts -> Kernel.Context.t -> Kernel.Ast.list_of_stmts * Kernel.Context.t

(* Convert an expression from our internal AST back to the parser's AST *)
val internal_to_parser_expr : Kernel.Context.t -> Kernel.Ast.expr -> Parsing.Ast.expr