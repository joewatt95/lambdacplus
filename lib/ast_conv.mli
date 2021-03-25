exception Unknown_var_name of string Common.Ast.expr
exception Underscore_var_name of Common.Location.source_loc

(* Convert a parsed expression to our internal AST. *)
val parser_to_internal_expr : Kernel.Context.t -> string Common.Ast.expr -> int Common.Ast.expr

(* Convert a parsed statement to our internal AST.
   Note that the return type here is actually (Ast.expr * ctx) because Def and
   Axiom will modify the context, affecting future statements.
   This is important to carry around in stmts_to_internal_ast when converting a
   list of statements to our internal AST.
*)
val parser_to_internal_stmt : string Common.Ast.stmt -> Kernel.Context.t -> int Common.Ast.stmt * Kernel.Context.t

(* This converts a list of parser statements to our internal AST. *)
val parser_to_internal_stmts : string Common.Ast.list_of_stmts -> Kernel.Context.t -> int Common.Ast.list_of_stmts * Kernel.Context.t

(* Convert an expression from our internal AST back to the parser's AST.
   This is called after an expression has been typechecked and fully normalized. *)
val internal_to_parser_expr : Kernel.Context.t -> int Common.Ast.expr -> string Common.Ast.expr