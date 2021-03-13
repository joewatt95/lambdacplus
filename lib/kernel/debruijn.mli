(* Convert a parsed expression to our internal AST. *)
val parser_to_internal_expr : Parsing.Ast.expr -> Context.t -> Ast.expr

(* Convert a parsed statement to our internal AST.
   Note that the return type here is actually (Ast.expr * ctx) because Def and
   Axiom will modify the context, affecting future statements.
   This is important to carry around in stmts_to_internal_ast when converting a
   list of statements to our internal AST.
*)
val parser_to_internal_stmt : Parsing.Ast.stmt -> Context.t -> Ast.stmt * Context.t

(* This converts a list of parser statements to our internal AST. *)
val parser_to_internal_stmts : Parsing.Ast.list_of_stmts -> Context.t -> Ast.list_of_stmts * Context.t

(* Convert an expression from our internal AST back to the parser's AST *)
val internal_to_parser_expr : Ast.expr -> Context.t -> Parsing.Ast.expr