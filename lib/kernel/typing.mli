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

exception Type_mismatch of {
  outer_expr : int Common.Ast.expr;
  expr : int Common.Ast.expr;
  inferred_type : int Common.Ast.expr;
  expected_type : expected_type;
}

(* Given a context and expression, compute the type of the expression.
   An exception is thrown if we have insufficient information to compute the type. *)
val infer : Context.t -> int Common.Ast.expr -> int Common.Ast.expr

val infer_annotation : 
  outer_expr:int Common.Ast.expr -> Context.t -> int Common.Ast.expr -> int Common.Ast.expr -> int Common.Ast.expr 

(* This is thrown by `infer` if we're unable to infer the type of an expression. *)
exception Cannot_infer_type of int Common.Ast.expr

(* Check if expr is a well formed type (ie it has type Type or Kind) wrt to
   a given context. *)
val check_well_formed_type : Context.t -> int Common.Ast.expr -> unit

(* This is thrown by check_well_formed_type if an expression which we expect to
  be a well formed type is not. *)
exception Ill_formed_type of {
  expr : int Common.Ast.expr;
  inferred_type : int Common.Ast.expr;
}