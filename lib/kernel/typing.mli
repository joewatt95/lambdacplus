(* Used to store expressions along with a context.
This is used in exceptions that we raise when we encounter typing errors.
We save the evaluation context alongside the expression so that we can unparse
them later on, even if they're deeply nested under many binders. *)
type expr_with_ctx = {expr : int Common.Ast.expr; ctx : Context.t}

(* Given a context, expression and the expected type, check if the expression
   has the expected type. This function throws a Type_mismatch exception if 
   something goes wrong, otherwise it just returns (). 

   The `outer_expr` argument is used in the creation of the Type_mismatch
   exception should anything go wrong. *)
val check : outer_expr:int Common.Ast.expr -> Context.t -> int Common.Ast.expr -> int Common.Ast.expr -> unit

(* check can throw the Type_mismatch exception which can be read as saying that
while typechecking `outer_expr`, we inferred the type of `expr` to be
`inferred_type`. However, it was expected to have a type given by
`expected_type`.

Note that there are 2 cases to `expected_type`. It can either be a concrete
type, in the event that the user supplied that type in the form of an
annotation ie ascription. 
Otherwise, if we don't know precisely what type it is, we give a string
indicating what family it should belong to. For instance, when we're typing
checking a function application, we expect the function expression to be of
some Pi type, though we may not know precisely what the exact form is. *)
type expected_type =
  | Exact of int Common.Ast.expr
  | Family of string

(* This is read as:
While typechecking `outer_expr`, we found that `expr` has the type `inferred_type`
but it was expected to have `expected_type`.
*)
exception Type_mismatch of {
  outer_expr : int Common.Ast.expr;
  expr : int Common.Ast.expr;
  inferred_type : expr_with_ctx;
  expected_type : expected_type;
}

(* Read as:
While typechecking `outer_expr`, we found that the left branch has a type of
`inl_type` while the right branch has type `inr_type` instead.
*)
exception Type_mismatch_in_match of {
  outer_expr : int Common.Ast.expr;
  inl_type : expr_with_ctx;
  inr_type : expr_with_ctx;
}

(* This is thrown when we infer the type of an existential elimination, Exists_elim.
Read as:
While typechecking `outer_expr` (an existential elimination of the form
let {x, y} := E in E'), we found that the inferred type of E', `inferred_type`,
contains `free_var` free in it.

Remember that the variable introduced into the context for the witness cannot
appear free in the conclusion.
*)
exception Type_contains_free_var of {
  outer_expr : int Common.Ast.expr;
  inferred_type : expr_with_ctx;
  free_var : string
}

(* Given a context and expression, compute the type of the expression.
An exception is thrown if we have insufficient information to compute the type. *)
val infer : Context.t -> int Common.Ast.expr -> int Common.Ast.expr

(* `infer_annotation ~outer_expr ctx expr ascribed_type` implements the typing
judgment for type ascriptions. 

If `ascribed_type` is Some, check that it's well formed and that `expr` has
that type. Otherwise, try to infer the type of `expr` from `ctx` directly.
If all goes well, a normalized form of the inferred type is returned.
Otherwise, an exception is thrown. *)
val infer_annotation : 
  outer_expr:int Common.Ast.expr -> Context.t -> int Common.Ast.expr -> int Common.Ast.expr option -> int Common.Ast.expr 

(* This is thrown by `infer` if we're unable to infer the type of an expression. *)
exception Cannot_infer_type of int Common.Ast.expr

(* Check if expr is a well formed type (ie it has type Type or Kind) wrt to
a given context. *)
val check_well_formed_type : Context.t -> int Common.Ast.expr -> unit

(* This is thrown by check_well_formed_type if an expression which we expect to
  be a well formed type is not. *)
exception Ill_formed_type of {
  expr : int Common.Ast.expr;
  inferred_type : expr_with_ctx;
}