(** This module implements the Context as described in the semantics.
These function similarly to those described the Types and Programming Languages
book by Pierce.

@see <https://www.cis.upenn.edu/~bcpierce/tapl/> Types and Programming Languages

The key modification that we make is that we take contexts to be lists of triples
of the form

[(var_name, var_type, binding)]

where
  - [var_name] is a string denoting the name of the variable.
  - [var_type] is the type of the [binding].
  - [binding] is the expression bound to [var_name].

In other words, we use the same context to store types and bindings.
Note that the index of [var_name] in the context is the de bruijn index of the 
variable.

This is used to implement 2 kinds of contexts, namely
- the naming context
- the evaluation context

The former is used to convert the parser's AST into the internal de bruijn AST,
while the evaluation context is used to typecheck and evaluate expressions
that are in the de bruijn AST.
*)

type t
(** Underlying type of contexts. This context module is existentially quantified
over this type variable.  *)

val empty : t
(** Empty context *)

val is_empty : t -> bool
(** [is_empty ctx] checks if ctx is empty *)

val length : t -> int
(** [length ctx] returns the length of ctx *)

val add_binding : string -> ?var_type:int Common.Ast.expr -> ?binding:int Common.Ast.expr -> t -> t
(** [add_binding var_name ?var_type ?binding ctx] returns ctx extended with
the binding (var_name, var_type, binding)

Note that if [var_name] already exists in the context, the new binding added to
it shadows the old one.

Here we allow [var_type] and [binding] to be optional. This is useful because
when we convert the parser's AST to the de bruijn one, we only add var names to the
context to convert them to de bruijn indices. In such cases, we do not
want to add a binding or type to it. *)

val add_name_bindings : t -> string list -> t
(** [add_name_bindings ctx \[var_name_0 ... var_name_n\]]
returns ctx extended with
[(var_name0, None, None)] ... [(var_name_n, None, None)]

This is used when converting the parser's AST to the de bruijn one.
*)

val var_name_to_index : t -> string -> int option
(** [var_name_to_index ctx str] finds the first index corresponding to the
variable name [str].

If such an index exists, a [Some] is returned. Otherwise the output is [None].
*)

val index_to_var_name : t -> int -> string
(** [index_to_var_name ctx index] returns the variable name corresponding to
[index] in [ctx].

Note that this function throws an exception if [index] is out of bounds.
*)

val get_binding : t -> int -> int Common.Ast.expr option
(** [get_binding ctx index] returns the {! binding} corresponding to a variable
identified by the de bruijn index, [index].

Note that this function throws an exception if [index] is out of bounds.
*)

val get_type : t -> int -> int Common.Ast.expr
(** [get_binding ctx index] returns the {! type} corresponding to a variable
identified by the de bruijn index, [index].

Note that this function throws an exception if [index] is out of bounds.
Also, if the variable has no type binding, ie {! var_type} is None, then an
exception is thrown.
*)

val is_var_name_bound : t -> string -> bool
(** [is_var_name_bound ctx var_name] checks if [var_name] already exists in [ctx]. 
This is used when generating fresh variable names when unparsing expressions
from the de bruijn AST back to the parser's AST.   
*)

val pretty_print : t -> unit
(** [prett_print ctx] pretty prints [ctx] to [stdout].

This is mostly for internal debugging purposes.
*)