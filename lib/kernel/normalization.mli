(** This module mainly implements the substitution and normalization operations
for de bruijn ASTs. 

The most interesting function here is {! normalize}, which corresponds to
the judgment Γ ⊢ E ⇓ v as found in the semantics.

These closely follow those found in the book, Types and Programming Languages
by Pierce. A link to the book is given below.
The key modification is that we now have more kinds of binders than just
lambdas.

Note that we do not define any exceptions here and no error reporting for
these functions is defined in {!module:Lambdacplus.Error_reporting} because
we only normalize expressions after typechecking them and we assume nothing
can go wrong when doing so.
The only kind of errors that can occur here are our internal bugs and we don't
report those to the user.

@see <https://www.cis.upenn.edu/~bcpierce/tapl/> Types and Programming Languages
*)

val normalize : Context.t -> int Common.Ast.expr -> int Common.Ast.expr
(** [normalize ctx expr] normalizes [expr] with respect to [ctx].
This corresponds to Γ ⊢ E ⇓ v
*)

val beta_reduce : int Common.Ast.expr -> int Common.Ast.expr -> int Common.Ast.expr
(** [beta_reduce body arg] performs the beta reduction operation on [body].

This works by walking down the AST of [body] and replacing all variables of
de bruijn index 0 with [arg].
A new expression is returned and the old one remains unchanged.

This function is used whenever we want to substitute under a binder.
*)

val subst : int -> int Common.Ast.expr -> int Common.Ast.expr -> int Common.Ast.expr
(** [subst index expr to_expr] is the standard substitution operation for de bruijn ASTs.

This replaces all occurrences of the variable given by [index] in [expr] to the
expression given by [to_expr]. 
A new expression is returned and the old one remains unchanged.
*)