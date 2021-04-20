(** This module implements the bidirectional typing judgments as described in 
the semantic specification.

In particular,
- {! infer} corresponds to the judgment Γ ⊦ E ⇒ τ
- {! check} corresponds to the judgment Γ ⊦ E ⇐ τ

Technically we implement these functions to take some extra parameters for the
purposes of error reporting.

This module also defines some type related exceptions that can arise when
type checking input from the user.
These are used by {!module:Lambdacplus.Error_reporting} to format errors to
report to the user.
*)

type expr_with_ctx = {
  expr : int Common.Ast.expr;
  (** The expression that gave an error while typechecking. *)
  
  ctx : Context.t
  (** The evaluation context at the point when {! expr} was encountered. *)
}
(** Used to store expressions along with a context.
This is contained inn exceptions that we raise when we encounter typing errors.
*)


val check : outer_expr:int Common.Ast.expr -> Context.t -> int Common.Ast.expr -> int Common.Ast.expr -> unit
(** [check ~outer_expr ctx expr expected_type] checks if expr has the type
[expected_type] with respect to [ctx].

This function throws a {! Type_mismatch} exception if 
something goes wrong, otherwise it just returns (). 

The [outer_expr] argument is used in the creation of the {! Type_mismatch}
exception should anything go wrong.
This is used to provide contextual information for error reporting.
*)

(** This is only used in {! Type_mismatch} exceptions. 
See below for details. *)
type expected_type =
  | Exact of int Common.Ast.expr
  | Family of string

exception Type_mismatch of {
  outer_expr : int Common.Ast.expr;
  expr : int Common.Ast.expr;
  inferred_type : expr_with_ctx;
  expected_type : expected_type;
}
(** This is read as:

While typechecking {! outer_expr}, we found that {! expr} has the type
[inferred_type] but it was expected to have {! expected_type}.

Note that there are 2 cases to `expected_type`. It can either be a concrete
type, in the event that the user supplied that type in the form of an
annotation. 
Otherwise, if we don't know precisely what type it is, we give a string
indicating what family it should belong to. For instance, when we're typing
checking a function application, we expect the function expression to be of
some Pi type, though we may not know precisely what the exact form is.
*)

exception Type_mismatch_in_match of {
  outer_expr : int Common.Ast.expr;
  inl_type : expr_with_ctx;
  inr_type : expr_with_ctx;
}
(* Read as:
While typechecking the match expression given by {! outer_expr}, we found that 
the left branch has a type of {! inl_type} while the right branch has type
{! inr_type} instead.
*)

exception Type_contains_free_var of {
  outer_expr : int Common.Ast.expr;
  inferred_type : expr_with_ctx;
  free_var : string
}
(**
Read as:
While typechecking {! outer_expr}
(which is an existential elimination of the form let \{x, y\} := E in E'), 
we found that the inferred type of E', [inferred_type],
contains [free_var] free in it.

Remember that the variable introduced into the context for the witness cannot
appear free in the conclusion.
*)

val infer : Context.t -> int Common.Ast.expr -> int Common.Ast.expr
(** [infer ctx expr] tries to compute the type of [expr] with respect to [ctx].

A {! Cannot_infer_type} exception is thrown if we have insufficient information
to compute the type.
*)

val infer_annotation : 
  outer_expr:int Common.Ast.expr -> Context.t -> int Common.Ast.expr -> int Common.Ast.expr option -> int Common.Ast.expr 
(** [infer_annotation ~outer_expr ctx expr ascribed_type] implements the typing
judgment for type ascriptions.

If [ascribed_type] is [Some], check that it's well formed and that [expr] has
that type. Otherwise, try to {! infer} the type of [expr] from [ctx] directly.
If all goes well, a normalized form of the inferred type is returned.
Otherwise, an exception is thrown.
*)

exception Cannot_infer_type of int Common.Ast.expr
(** This is thrown by {! infer} if we're unable to infer the type of [expr]. *)

val check_well_formed_type : Context.t -> int Common.Ast.expr -> unit
(** [check_well_formed_type ctx expr]
checks if [expr] is a well formed type (ie it has type [Type] or [Kind]) wrt
to [ctx]. 

If all goes well, () is returned. Otherwise, {! Ill_formed_type} is thrown.
*)

exception Ill_formed_type of {
  expr : int Common.Ast.expr;
  inferred_type : expr_with_ctx;
}
(** Read as:
[expr] is not a well formed type because we inferred its type to be [inferred_type],
but it should be [Type] or [Kind] instead.
*)