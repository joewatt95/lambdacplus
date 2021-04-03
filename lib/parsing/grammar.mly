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
  open Common
%}

%token APP

(* Types and expressions *)
%token TYPE KIND PI SIGMA FUN LET IN FST SND ARROW PROD
      PLUS MATCH INL INR BAR END WITH 
      EXISTS LEFT_CURLY RIGHT_CURLY

(* For proof terms, like Lean. *)
%token ASSUME HAVE FROM SHOW THEOREM

(* Misc punctuation *)
%token LPAREN RPAREN COLON_EQ COLON COMMA DOUBLE_ARROW

(* Variables *)
%token <string> VAR_NAME

(* Top level commands *)
%token DEF CHECK AXIOM EVAL

(* End of line and input *)
%token EOF

(* Lowest precedence *)
%nonassoc LPAREN VAR_NAME FUN PI SIGMA TYPE LET KIND FST SND MATCH INL INR 
          ASSUME HAVE SHOW EXISTS LEFT_CURLY
(* Highest precedence *)
%nonassoc APP

%start <string Ast.list_of_stmts> main

%%

let main := terminated(nonempty_list(stmt), EOF)

let located(x) == ~ = x; { Location.locate x ~source_loc:$loc }

let stmt == located(
  | def_stmt
  | AXIOM; ~ = var_name; COLON; var_type = expr;  { Ast.Axiom {var_name; var_type} }
  | CHECK; ~ = expr;                              { Ast.Check expr }
  | EVAL; ~ = expr;                               { Ast.Eval expr }
  | THEOREM; ~=var_name; COLON; claim=expr; COLON_EQ; proof=expr;
  {
    (* let binding = 
      Location.locate @@ Ast.Ascription {expr=proof; ascribed_type=claim} in *)
    Ast.Def {var_name; binding=proof; ascribed_type=Some claim}
  } 
)

let def_stmt ==
  | DEF; ~ = var_name; COLON_EQ; binding = expr;
    { Ast.Def {var_name; binding; ascribed_type=None}}
  | DEF; ~ = var_name; COLON; ascribed_type=expr; COLON_EQ; binding = expr;
    { Ast.Def {var_name; binding; ascribed_type=Some ascribed_type} }

let expr :=
  | located(raw_expr)
  (* Don't include the left and right parens in the source location *)
  | delimited(LPAREN, expr, RPAREN)
  | fun_expr
  | pi_expr
  | proof_term

let raw_expr :=
  (* This 1st rule is causing shift/reduce conflicts in menhir. *)
  | fn=expr; arg=expr;                           { Ast.App {left=fn; right=arg} } %prec APP
  (* This let rule is also causing shift/reduce conflicts. *)
  | let_expr
  | sigma_expr
  | exists_expr
  | pair_expr
  | TYPE;                                        { Ast.Type }
  | KIND;                                        { Ast.Kind }
  | FST; ~ = expr;                               <Ast.Fst>
  | SND; ~ = expr;                               <Ast.Snd>
  | ~ = var_name;                                <Ast.Var>
  | ascription
  | ~ = expr; ARROW; body = expr;                { Ast.Pi{var_name="_"; expr; body}}
  | ~ = expr; PROD; body = expr;                { Ast.Sigma{var_name="_"; expr; body}}
  | INL; ~ = expr;                                  <Ast.Inl>
  | INR; ~ = expr ;                                 <Ast.Inr>
  | match_expr
  | sum_expr 
  | exists_pair_expr
  (* | EXISTS_ELIM; left=expr; right=expr;   { Ast.Exists_elim {left; right} } *)
 
 (* | let_pair *)

(* From Lean reference manual:
assume h : p, t is sugar for λ h : p, t
have h : p, from s, t is sugar for (λ h : p, t) s
suffices h : p, from s, t is sugar for (λ h : p, s) t
show p, t is sugar for (t : p) *)
 let proof_term ==
  | ASSUME; ~=fun_arg_list; COMMA; body=expr;
    { List.fold_right
        (fun (input_var, input_type) body ->
          Location.locate (Ast.Fun {input_var; input_type; body}) ~source_loc:$loc)
      fun_arg_list body }
  | located(
    | HAVE; ~=var_name; COLON; claim=expr; COMMA; FROM; proof=expr; COMMA; body=expr;
      {
        (* let fn = Location.locate @@ Ast.Fun {input_var=var_name; input_type=Some claim; body} in
        Ast.App {left=fn; right=proof} *)
        (* let binding = Location.locate @@ Ast.Ascription {expr=proof; ascribed_type=claim} in *)
        Ast.Let {abstraction={var_name; expr=proof; body}; ascribed_type=Some claim} 
      }
    | HAVE; claim=expr; COMMA; FROM; proof=expr; COMMA; body=expr;
      {
        (* let fn = Location.locate @@ Ast.Fun {input_var="this"; input_type=Some claim; body} in
        Ast.App {left=fn; right=proof} *)
        (* let binding = Location.locate @@ Ast.Ascription {expr=proof; ascribed_type=claim} in *)
        Ast.Let {abstraction={var_name="this"; expr=proof; body}; ascribed_type=Some claim} 
      }
    | SHOW; claim=expr; COMMA; FROM; proof=expr;
    { Ast.Ascription {expr=proof; ascribed_type=claim} }
  )

let var_name == VAR_NAME

let annotated_expr == 
  delimited(LPAREN, separated_pair(expr, COLON, expr), RPAREN) 

let ascription ==
      (expr, ascribed_type) = annotated_expr;
    { Ast.Ascription {expr; ascribed_type} }

let fun_expr == FUN; ~ = fun_arg_list; DOUBLE_ARROW; body=expr;
    { List.fold_right
        (fun (input_var, input_type) body ->
          Location.locate (Ast.Fun {input_var; input_type; body}) ~source_loc:$loc)
      fun_arg_list body }

let fun_arg_list == nonempty_list(fun_arg)

let annotated_name == separated_pair(var_name, COLON, expr)

let bracketed_annotated_name == 
  delimited(LPAREN, separated_pair(var_name, COLON, expr), RPAREN)

let fun_arg == 
  | ~ = var_name;
    { (var_name, None) }
  | (var_name, expr_type) = bracketed_annotated_name;    
    { (var_name, Some expr_type) }

let pi_expr == 
  | PI; ~ = pi_arg_list; COMMA; output_type=expr;
    { List.fold_right
      (fun (input_var, input_type) output_type ->
         Location.locate
           (Ast.Pi {var_name=input_var; expr=input_type; body=output_type}) ~source_loc:$loc)
      pi_arg_list output_type}
  (* Parens are optional in the event there's only one input variable. *)
  | PI; (input_var, input_type) = annotated_name; COMMA; output_type=expr;
    { Location.locate (Ast.Pi {var_name=input_var; expr=input_type; body=output_type}) ~source_loc:$loc }

let pi_arg_list ==
    | nonempty_list(bracketed_annotated_name)

let let_expr ==
  | LET; ~=var_name; COLON_EQ; binding=expr; IN; body=expr;
    { Ast.Let {abstraction={var_name; expr=binding; body}; ascribed_type=None} }
  | LET; ~=var_name; COLON; ascribed_type=expr; COLON_EQ; binding=expr; IN; body=expr;
    { Ast.Let {abstraction={var_name; expr=binding; body}; ascribed_type = Some ascribed_type} }
    
  | LET; (witness_var, witness_cert) =
    delimited(LEFT_CURLY, separated_pair(var_name, COMMA, var_name), RIGHT_CURLY);
    COLON_EQ; ~=expr; IN; body=expr;
    { Ast.Exists_elim {expr; witness_var; witness_cert; body} }

let sigma_expr == SIGMA; 
  (input_var, input_type) = sigma_arg;
  COMMA; output_type=expr;
  { Ast.Sigma {var_name=input_var; expr=input_type; body=output_type} }

let exists_expr == EXISTS; 
  (input_var, input_type) = sigma_arg;
  COMMA; output_type=expr;
  { Ast.Exists {var_name=input_var; expr=input_type; body=output_type} }

let sigma_arg == 
  | delimited(option(LPAREN), separated_pair(var_name, COLON, expr), option(RPAREN))

let pair_expr == 
  (left, right) = delimited(LPAREN, separated_pair(expr, COMMA, expr), RPAREN);
  { Ast.Pair {left; right} }

let exists_pair_expr == 
  (left, right) = delimited(LEFT_CURLY, separated_pair(expr, COMMA, expr), RIGHT_CURLY);
  { Ast.Exists_pair {left; right} }

let sum_expr ==
  (left, right) = separated_pair(expr, PLUS, expr);
  { Ast.Sum {left; right} }

let match_expr ==
  | MATCH; ~ = expr; WITH;
    BAR; INL; var_left = var_name; DOUBLE_ARROW; body_left = expr;
    BAR; INR; var_right = var_name; DOUBLE_ARROW; body_right = expr;
    END;
  { Ast.Match
    {expr;
     inl={match_var=var_left; match_body=body_left};
     inr={match_var=var_right; match_body=body_right}} }
  | MATCH; ~ = expr; WITH;
    BAR; INR; var_right = var_name; DOUBLE_ARROW; body_right = expr;
    BAR; INL; var_left = var_name; DOUBLE_ARROW; body_left = expr;
    END;
  { Ast.Match
    {expr;
     inl={match_var=var_left; match_body=body_left};
     inr={match_var=var_right; match_body=body_right}} }


    (*
    {
      let inner_fn = Location.locate ~source_loc:$loc @@
        Ast.Fun {input_type=Some prop; input_var=var_name_prop; body} in
      let right = Location.locate ~source_loc:$loc @@
        Ast.Fun {input_type=None; input_var=var_name; body=inner_fn} in
      Ast.Exists_elim {left=expr; right}
    }
    *)
    
(*
let let_pair == 
  LET; LPAREN; left_var=var_name; COMMA; right_var=var_name; RPAREN; 
  COLON_EQ; binding=expr; IN; body=expr; 
  { Ast.Let_pair {left_var; right_var; binding; body} }
*)