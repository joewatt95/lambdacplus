(* This is the AST that the parser generates from the concrete syntax. *)

type expr = raw_expr Location.located
and raw_expr =
  | Type
  | Pi of {input_var : string;
           input_type : expr;
           output_type : expr}
  | Var of string
  | Fun of {input_var : string;
            body : expr}
  | App of {fn : expr;
            arg : expr}
  | Ascription of {expr : expr;
                   expr_type : expr}
[@@deriving show]

type list_of_exprs = expr list
[@@deriving show]

type stmt = raw_stmt Location.located
and raw_stmt =
  | Def of {var_name : string; binding : expr}
  | Axiom of {var_name : string; var_type : expr}
  | Check of expr
  | Eval of expr
[@@deriving show]

type list_of_stmts = stmt list
[@@deriving show]
