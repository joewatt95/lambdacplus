(* This is the AST that the parser generates from the concrete syntax. *)

module Loc = Common.Location

type expr = raw_expr Loc.located
and raw_expr =
  | Type
  | Kind
  | Pi of {input_var : string;
           input_type : expr;
           output_type : expr}
  | Var of string
  | Fun of {input_var : string;
            input_type : expr option;
            body : expr}
  | App of {fn : expr;
            arg : expr}
  | Ascription of {expr : expr;
                   expr_type : expr}
  | Let of {var_name : string;
            binding : expr;
            body : expr}
[@@deriving show]

(* Unparses an expression back to a string for printing. *)
let rec unparse (expr : expr) =
 match expr.data with
 | Type -> "Type"
 | Kind -> "Kind"
 | Var var_name -> var_name 
 | Pi {input_var; input_type; output_type} ->
  "∏ (" ^ input_var ^ " : " ^ unparse input_type ^ "), " ^ unparse output_type
 | Fun {input_var; body; _} ->
  "λ " ^ input_var ^ " ⇒ " ^ unparse body
 | App {fn; arg} ->
  "(" ^ unparse fn ^ " " ^ unparse arg ^")"

 (* No other cases are possible since all expressions are type checked and then
  eagerly normalized to one of the above forms. *)
 | _ -> assert false

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