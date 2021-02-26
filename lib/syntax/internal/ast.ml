(*
https://github.com/andrejbauer/spartan-type-theory/blob/master/src/input.mli

Here we use the ppx_deriving library to automatically generate show functions that
can convert each ADT into a string.
*)

(*
Note here that we don't distinguish between types and expressions anymore since
types have been promoted to expressions.
*)
type expr =
  | Type (* The paradoxical type universe, ie the type of all types. *)
    (* var_name is the original name input by the user *)
  | Pi of {input_var : string;
           input_type : expr;
           output_type : expr}
  | Var of int
  | Fun of {input_var : string;
            body : expr;
            fn_type : expr}
  | App of {fn : expr; arg : expr}
[@@deriving show]

type list_of_exprs = expr list
[@@deriving show]

type stmt =
  | Def of {var_name : string; var_expr : expr}
  | Axiom of {var_name : string; var_type : expr}
  | Check of expr
  | Eval of expr
[@@deriving show]

type list_of_stmts = stmt list
[@@deriving show]
