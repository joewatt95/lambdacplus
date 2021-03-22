(* This module signature for the Context module serves as a layer of abstraction
   and API to work with.
   Contexts can be thought of as lists of triples of the form
     (Variable name, Type, Binding)
   Note that "Binding" may be None or Some.
*)

(* Underlying type of contexts *)
type t

(* Empty context *)
val empty : t

(* Check if a context is empty *)
val is_empty : t -> bool

(* Get the length of a context. *)
val length : t -> int

(* Add a binding to the context. 
   Here we allow var_type and binding to be optional. This is useful because
   when we convert the parser's AST to our own, we only add var names to the
   context to convert them to de bruijn indices. *)
val add_binding : string -> ?var_type:int Common.Ast.expr -> ?binding:int Common.Ast.expr -> t -> t

(* Given a variable name and a context, find the first index corresponding to
the variable. *)
val var_name_to_index : t -> string -> int option

(* Given an index and a context, return the variable name. *)
val index_to_var_name : t -> int -> string

(* Get the binding corresponding to a variable identified by its de bruijn 
index. *)
val get_binding : t -> int -> int Common.Ast.expr option

(* Get the type corresponding to a variable identified by its de bruijn 
index. *)
val get_type : t -> int -> int Common.Ast.expr

(* Check if a variable name is bound in a context. *)
val is_var_name_bound : t -> string -> bool

(* Convert a context to a string for printing. *)
val show : t -> string