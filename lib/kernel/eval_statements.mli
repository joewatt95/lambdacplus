val eval_stmt : int Common.Ast.stmt -> Context.t -> int Common.Ast.expr * Context.t
(** [eval_stmt statement ctx] evaluates [statement] using [ctx].
It returns a pair of ([expr], [new_ctx])
where
- [expr] is the expression resulting from evaluating the statement.
- [new_ctx] is the new context.

*)

val eval_stmts : int Common.Ast.list_of_stmts -> Context.t -> int Common.Ast.expr * Context.t
(** [eval_stmts statements ctx] works like {! eval_stmt}, just that we fold over
the list of statements that is [statements] here.
*)