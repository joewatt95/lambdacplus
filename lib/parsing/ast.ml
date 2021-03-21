(* This is the AST that the parser generates from the concrete syntax. *)
module Loc = Common.Location

type expr = raw_expr Loc.located
and abstraction = {
  var_name : (string [@visitors.opaque]);
  expr : expr;
  body : expr
}
and raw_expr =
  | Type
  | Kind
  | Var of (string [@visitors.opaque])
  | Pi of abstraction 
  | Sigma of abstraction
  | Pair of {expr1 : expr; expr2 : expr}
  | Fst of expr
  | Snd of expr
  | Fun of {input_var : (string [@visitors.opaque]);
            input_type : (expr option [@visitors.opaque]);
            body : expr}
  | App of {fn : expr;
            arg : expr}
  | Ascription of {expr : expr;
                   ascribed_type : expr}
  | Let of abstraction
[@@deriving show,
  visitors {variety="fold"; ancestors=["Loc.fold"]}]

(* Template for defining catamorphisms over the AST that ignore source 
   locations. *)
class virtual ['self] ast_folder =
  object (_ : 'self)
    inherit [_] fold as super

    method! visit_expr env {data; _} = super#visit_raw_expr env data
  end

type list_of_exprs = expr list
[@@deriving show]

type stmt = raw_stmt Loc.located
and raw_stmt =
  | Def of {var_name : string; binding : expr}
  | Axiom of {var_name : string; var_type : expr}
  | Check of expr
  | Eval of expr
[@@deriving show]

type list_of_stmts = stmt list
[@@deriving show]