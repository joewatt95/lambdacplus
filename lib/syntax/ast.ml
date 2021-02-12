(*
https://github.com/andrejbauer/spartan-type-theory/blob/master/src/input.mli

Here we use the ppx_deriving library to automatically generate show functions that
can convert each ADT into a string.
*)

type name = string
[@@deriving show]

(*
Note here that we don't distinguish between types and expressions anymore since
types have been promoted to expressions.
*)
type expr =
  | Type (* The paradoxical type universe, ie the type of all types. *)
  (* (var, type of var, return type) *)
  | Pi of name * expr * expr
  | Var of name
  (* (var, type of var, body expression) *)
  | Fun of name * expr * expr
  | App of expr * expr
[@@deriving show]

type stmt =
  | Def of name * expr
  | Axiom of name * expr
  | Check of expr
  | Eval of expr
[@@deriving show]

type list_of_stmts = stmt list
[@@deriving show]
