val unparse_internal_expr : Kernel.Context.t -> int Common.Ast.expr -> string
(** [unparse_internal_expr ctx expr] unparses [expr] using [ctx] to a readable
string with variable names.

The context [ctx] is used to convert the de bruijn indices back to variable names.
*)