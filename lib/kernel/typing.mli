(* While typechecking `app`, we inferred the type of `fn` to be `inferred_type`,
   but it was supposed to be a Pi. *)
exception Pi_expected of {
  app : Ast.expr; 
  fn : Ast.expr; 
  inferred_type : Ast.expr
}

(* This is thrown when we are unable to infer the type of a function because
   it has no type annotations. *)
exception Cannot_infer_type_of_fn of Ast.expr

(* This is thrown when we get a Kind expression and are thus unable to infer its
   type. *)
exception Cannot_infer_type_of_kind of Common.Location.source_loc

(* While checking if `expr` is a valid type, we inferred its type to be
  `inferred_type` but it should be Type or Kind. *)
exception Ill_formed_type of {
  expr : Ast.expr;
  inferred_type : Ast.expr;
}

(* The user has ascribed `expected_type` to `expr` but we instead inferred the
   type of `expr` to be `inferred_type`.
   The `outer_expr` parameter indicates the surrounding expression containing the
   erraneous one. This provides contextual information for error reporting. *)
exception Type_mismatch of {
  outer_expr : Ast.expr;
  expr : Ast.expr;
  inferred_type : Ast.expr;
  expected_type : Ast.expr
}

(* Given a context, expression and the expected type, check if the expression
   has the expected type. This function throws a Type_mismatch exception if 
   something goes wrong, otherwise it just returns (). 

   The `outer_expr` argument is used in the creation of the Type_mismatch
   exception should anything go wrong. *)
val check : outer_expr:Ast.expr -> Context.t -> Ast.expr -> Ast.expr -> unit

(* Given a context and expression, compute the type of the expression.
   An exception is thrown if we have insufficient information to compute the type. *)
val infer : Context.t -> Ast.expr -> Ast.expr

(* Check if expr is a well formed type (ie it has type Type or Kind) wrt to
   a given context. *)
val check_well_formed_type : Context.t -> Ast.expr -> unit