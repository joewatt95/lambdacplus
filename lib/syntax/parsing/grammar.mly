(*
https://github.com/andrejbauer/spartan-type-theory/blob/master/src/parser.mly
https://github.com/amblafont/sedlex-menhir/blob/master/sedlex-menhir/parser.mly

https://mukulrathi.co.uk/create-your-own-programming-language/parsing-ocamllex-menhir/
 *)

%{
  open Ast
  open Syntax.Location
%}

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

%start <list_of_stmts> main

%%

main:
  | stmt_=stmt EOF { [stmt_] }
  | stmt_=stmt stmts=main { stmt_ :: stmts }

(*
https://discuss.ocaml.org/t/your-favorite-menhir-tricks-and-fanciness/7299/4
https://inbox.ocaml.org/caml-list/58FE606B.5000400@inria.fr/t/
 *)
%inline located(X):
   x=X
       { locate x $loc}

stmt: located(plain_stmt) { $1 }
plain_stmt:
  | DEF var_name_=var_name COLON_EQ expr_=expr  { Def {var_name=var_name_; var_expr=expr_} }
  | AXIOM var_name_=var_name COLON expr_=expr   { Axiom {var_name=var_name_; var_type=expr_} }
  | CHECK expr_=expr                            { Check expr_ }
  | EVAL expr_=expr                             { Eval expr_ }

expr: located(plain_expr) { $1 }
plain_expr:
  (* This 1st rule is causing shift/reduce conflicts in menhir. *)
  | fn=expr arg=expr                 %prec APP  { App {fn=fn; arg=arg} }
  | TYPE                                        { Type }
  | var_name_=var_name                          { Var var_name_ }
  | fun_expr_=fun_expr                          { fun_expr_ }
  | pi_expr_=pi_expr                            { pi_expr_ }
  | ascription_=ascription                      { ascription_ }
  | LPAREN expr_=expr RPAREN                    { expr_ }

(* var_name: located(plain_var_name) { $1 } *)
var_name:
  | VAR_NAME                                    { $1 }

ascription: located(plain_ascription) { $1 }
plain_ascription:
    | LPAREN expr1=expr COLON expr2 = expr RPAREN {Ascription {expr=expr1; expr_type=expr2}}

fun_expr:
  | FUN arg_list=fun_arg_list DOUBLE_ARROW body=expr
  { List.fold_right
    (fun var_name_ b : expr ->
      locate (Fun {input_var=var_name_; body=b}) $loc)
    arg_list body}

fun_arg_list:
  | var_name_=var_name arg_list_=fun_arg_list
    { var_name_ :: arg_list_ }
  | var_name_=var_name
    { [var_name_] }

pi_expr:
  | PI arg_list=pi_arg_list COMMA body=expr
  { List.fold_right
    (fun (var_name_, type_) b : expr ->
       locate
         (Pi {input_var=var_name_; input_type=type_; output_type=b}) $loc)
    arg_list body}

pi_arg_list:
  | LPAREN var_name_=var_name COLON type_=expr RPAREN arg_list_=pi_arg_list
    { (var_name_, type_) :: arg_list_ }
  | LPAREN var_name_=var_name COLON type_=expr RPAREN
    { [(var_name_, type_)] }
