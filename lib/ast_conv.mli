(**  This module is responsible for converting between
- the internal de bruijn AST, [int Common.Ast.expr],
  which uses de bruin indices to identify variables.

- the parser's AST, [string Common.Ast.expr],
  which uses strings representing variable names to identify variables.

The techniques used in the actual implementation follow closely the Types and
Programming Languages book by Pierce.

@see <https://www.cis.upenn.edu/~bcpierce/tapl/> Types and Programming Languages
*)

exception Unknown_var_name of string Common.Ast.expr
(** This is raised whenever we encounter an unknown variable name.

The expression in this exception is always a variable node of the form

[{data=Ast.Var str; source_loc}]

where
- [str] is the variable name
- source_loc is the source location corresponding to this node
*)

exception Underscore_var_name of Common.Location.source_loc
(** This is raised whenever we encounter an underscore, ie [_], used in a position
that is not a variable.

The [source_loc] in this exception indicates the source location where the
underscore was found.
*)

val parser_to_internal_expr : Kernel.Context.t -> string Common.Ast.expr -> int Common.Ast.expr
(** [parser_to_internal_expr ctx expr] converts [expr], from the parser's AST
to the de bruijn one using the naming context ctx.
*)

val parser_to_internal_stmt : string Common.Ast.stmt -> Kernel.Context.t -> int Common.Ast.stmt * Kernel.Context.t
(** [parser_to_internal_stmt statement ctx]
converts [statement] from the parser's AST to the de bruijn one.

The return value is a pair of the form [(expr, ctx)]
where
- [expr] is the converted expression
- [ctx] is the new context, possibly extending with a new binding
*)

val parser_to_internal_stmts : string Common.Ast.list_of_stmts -> Kernel.Context.t -> int Common.Ast.list_of_stmts * Kernel.Context.t
(** [parser_to_internal_stmts statements ctx] folds over [statements] using
{! parser_to_internal_stmt}
*)

val internal_to_parser_expr : Kernel.Context.t -> int Common.Ast.expr -> string Common.Ast.expr
(** [internal_to_parser_expr ctx expr] converts [expr] from the de bruijn AST
back to the parser's AST.

This is called after an expression has been typechecked and fully normalized.
*)