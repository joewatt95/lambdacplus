(*
https://github.com/andrejbauer/spartan-type-theory/blob/master/src/parser.mly
https://github.com/amblafont/sedlex-menhir/blob/master/sedlex-menhir/parser.mly
 *)

(*
Dummy token and some precedence rules to make function application left
associative.
See:
https://ptival.github.io/2017/05/16/parser-generators-and-function-application/
 *)
%token APP

(* Types and expressions *)
%token TYPE PI FUN

(* Misc punctuation *)
%token LPAREN RPAREN COLON_EQ COLON COMMA DOUBLE_ARROW

(* Variables *)
%token <string> VAR_NAME

(* Top level commands *)
%token DEF CHECK AXIOM EVAL

(* End of line and input *)
%token EOF

(* Lowest precedence *)
%nonassoc LPAREN VAR_NAME FUN PI TYPE
(* Highest precedence *)
%nonassoc APP

%start <Ast.list_of_stmts> main

%%

main:
| stmt_=stmt EOF { [stmt_] }
| stmt_=stmt stmts=main { stmt_ :: stmts }

stmt:
  | DEF var_name_=var_name COLON_EQ expr_=expr  { Ast.Def (var_name_, expr_) }
  | AXIOM var_name_=var_name COLON expr_=expr   { Ast.Axiom (var_name_, expr_) }
  | CHECK expr_=expr                            { Ast.Check expr_ }
  | EVAL expr_=expr                             { Ast.Eval expr_ }

expr:
  | TYPE                                        { Type }
  | var_name_=var_name                          { Var var_name_ }
  | fun_expr_=fun_expr                          { fun_expr_ }
  | pi_expr_=pi_expr                            { pi_expr_ }
  | LPAREN expr_=expr RPAREN                    { expr_ }
  (* This last rule is causing shift/reduce conflicts in menhir. *)
  | fn=expr arg=expr                 %prec APP  { Ast.App (fn, arg) }

var_name:
  | VAR_NAME                                    { $1 }

fun_expr:
  | FUN LPAREN var_name_=var_name COLON input_type=expr RPAREN COLON ret_type=expr DOUBLE_ARROW body=expr
    { Ast.Fun (var_name_, body, Ast.Pi (var_name_, input_type, ret_type)) }

pi_expr:
  | PI arg_list_=arg_list COMMA body=expr
     { List.fold_right (fun (var_name_, type_) b -> Ast.Pi (var_name_, type_, b)) arg_list_ body }

arg_list:
  | LPAREN var_name_=var_name COLON type_=expr RPAREN arg_list_=arg_list
    { (var_name_, type_) :: arg_list_ }
  | LPAREN var_name_=var_name COLON type_=expr RPAREN
    { [(var_name_, type_)] }
