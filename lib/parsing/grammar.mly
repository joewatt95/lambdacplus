(*
References on Menhir's new syntax and carrying around source locations
http://gallium.inria.fr/blog/parser-construction-menhir-appetizers/
https://gitlab.inria.fr/fpottier/menhir/blob/master/doc/new-rule-syntax-summary.md

Dummy token and some precedence rules to make function application left
associative. See:
https://ptival.github.io/2017/05/16/parser-generators-and-function-application/

TODO: Implement error reporting
https://baturin.org/blog/declarative-parse-error-reporting-with-menhir/
 *)
%{
  module Loc = Common.Location
%}

%token APP

(* Types and expressions *)
%token TYPE KIND PI SIGMA FUN LET IN FST SND

(* Misc punctuation *)
%token LPAREN RPAREN COLON_EQ COLON COMMA DOUBLE_ARROW

(* Variables *)
%token <string> VAR_NAME

(* Top level commands *)
%token DEF CHECK AXIOM EVAL

(* End of line and input *)
%token EOF

(* Lowest precedence *)
%nonassoc LPAREN VAR_NAME FUN PI SIGMA TYPE LET KIND FST SND
(* Highest precedence *)
%nonassoc APP

%start <Ast.list_of_stmts> main

%%

let main := terminated(nonempty_list(stmt), EOF)

let located(x) == ~ = x; { Loc.locate x ~source_loc:$loc }

let stmt := located(
  | DEF; ~ = var_name; COLON_EQ; binding = expr;  { Ast.Def {var_name; binding}}
  | AXIOM; ~ = var_name; COLON; var_type = expr;  { Ast.Axiom {var_name; var_type} }
  | CHECK; ~ = expr;                              { Ast.Check expr }
  | EVAL; ~ = expr;                               { Ast.Eval expr }
)

let expr :=
  | located(raw_expr)
  (* Don't include the left and right parens in the source location *)
  | delimited(LPAREN, expr, RPAREN)
  | fun_expr
  | pi_expr

let raw_expr :=
  (* This 1st rule is causing shift/reduce conflicts in menhir. *)
  | fn=expr; arg=expr;                           { Ast.App {fn; arg} } %prec APP
  (* This let rule is also causing shift/reduce conflicts. *)
  | let_expr
  | sigma_expr
  | pair_expr
  | TYPE;                                        { Ast.Type }
  | KIND;                                        { Ast.Kind }
  | FST; ~ = expr;                               <Ast.Fst>
  | SND; ~ = expr;                               <Ast.Snd>
  | ~ = var_name;                                <Ast.Var>
  | ascription

let var_name := VAR_NAME

let annotated_expr := 
  delimited(LPAREN, separated_pair(expr, COLON, expr), RPAREN) 

let ascription :=
      (expr, ascribed_type) = annotated_expr;
    { Ast.Ascription {expr; ascribed_type} }

let fun_expr := FUN; ~ = fun_arg_list; DOUBLE_ARROW; body=expr;
    { List.fold_right
        (fun (input_var, input_type) body ->
          Loc.locate (Ast.Fun {input_var; input_type; body}) ~source_loc:$loc)
      fun_arg_list body}

let fun_arg_list := nonempty_list(fun_arg)

let fun_arg := 
  | ~ = var_name;
    { (var_name, None) }
  | (var_name, expr_type) = delimited(LPAREN, separated_pair(var_name, COLON, expr), RPAREN);    
    { (var_name, Some expr_type) }

let pi_expr := PI; ~ = pi_arg_list; COMMA; output_type=expr;
  { List.fold_right
    (fun (input_var, input_type) output_type ->
       Loc.locate
         (Ast.Pi {var_name=input_var; expr=input_type; body=output_type}) ~source_loc:$loc)
    pi_arg_list output_type}

let pi_arg_list :=
      nonempty_list(delimited(LPAREN, separated_pair(var_name, COLON, expr), RPAREN))

let let_expr := 
  LET; ~ = var_name; COLON_EQ; binding=expr; IN; body=expr;
  { Ast.Let {var_name; expr=binding; body} }

let sigma_expr := SIGMA; 
  (input_var, input_type) = delimited(LPAREN, separated_pair(var_name, COLON, expr), RPAREN);
  COMMA; output_type=expr;
  { Ast.Sigma {var_name=input_var; expr=input_type; body=output_type} }

let pair_expr := 
  (expr1, expr2) = delimited(LPAREN, separated_pair(expr, COMMA, expr), RPAREN);
  { Ast.Pair {expr1; expr2} }