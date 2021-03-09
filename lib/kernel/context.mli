(* This module signature for the Context module serves as a layer of abstraction
   and API to work with. *)

type t

val empty : t

val is_empty : t -> bool

val length : t -> int

(* Here we allow var_type and binding to be optional. This is useful because
   when we convert the parser's AST to our own, we only add var names to the
   context to convert them to de bruijn indices. *)
val add_binding : string -> ?var_type:Ast.expr -> ?binding:Ast.expr -> t -> t

val var_name_to_index : string -> t -> int

val index_to_var_name : int -> t -> string

val get_binding : int -> t -> Ast.expr option

val get_type : int -> t -> Ast.expr

val is_var_name_bound : string -> t -> bool

val show : t -> string
