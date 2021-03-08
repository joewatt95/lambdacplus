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

(* References on Menhir's new syntax and carrying around source locations
   http://gallium.inria.fr/blog/parser-construction-menhir-appetizers/
   https://gitlab.inria.fr/fpottier/menhir/blob/master/doc/new-rule-syntax-summary.md
 *)

let main := terminated(nonempty_list(stmt), EOF)

let located(x) == ~ = x; { locate x $loc }

let stmt := located(
  | DEF; ~ = var_name; COLON_EQ; var_expr = expr; { Def {var_name; var_expr}}
  | AXIOM; ~ = var_name; COLON; var_type = expr;  { Axiom {var_name; var_type} }
  | CHECK; ~ = expr;                              { Check expr }
  | EVAL; ~ = expr;                               { Eval expr }
)

let expr :=
  | located(raw_expr)
  (* Don't include the left and right parens in the source location *)
  | delimited(LPAREN, expr, RPAREN)
  | fun_expr
  | pi_expr

let raw_expr :=
  (* This 1st rule is causing shift/reduce conflicts in menhir. *)
  | fn=expr; arg=expr;                           { App {fn; arg} } %prec APP
  | TYPE;                                        { Type }
  | ~ = var_name;                                { Var var_name }
  | ascription

let var_name := VAR_NAME

let ascription :=
      (expr, expr_type) = delimited(LPAREN, separated_pair(expr, COLON, expr), RPAREN);
    { Ascription {expr; expr_type} }

let fun_expr := FUN; ~ = fun_arg_list; DOUBLE_ARROW; body=expr;
    { List.fold_right
        (fun input_var body : expr ->
          locate (Fun {input_var; body}) $loc)
      fun_arg_list body}

let fun_arg_list := nonempty_list(var_name)

let pi_expr := PI; ~ = pi_arg_list; COMMA; output_type=expr;
  { List.fold_right
    (fun (input_var, input_type) output_type : expr ->
       locate
         (Pi {input_var; input_type; output_type}) $loc)
    pi_arg_list output_type}

let pi_arg_list :=
      nonempty_list(delimited(LPAREN, separated_pair(var_name, COLON, expr), RPAREN))
