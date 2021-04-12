(*
This module contains our grammar, written using Menhir.

References on Menhir's new syntax and carrying around source locations
http://gallium.inria.fr/blog/parser-construction-menhir-appetizers/
https://gitlab.inria.fr/fpottier/menhir/blob/master/doc/new-rule-syntax-summary.md

Dummy token and some precedence rules to make function application left
associative. See:
https://ptival.github.io/2017/05/16/parser-generators-and-function-application/

Error reporting:
https://baturin.org/blog/declarative-parse-error-reporting-with-menhir/
 *)

%{
  open Containers
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
%nonassoc APP

(* This forces the parser to parse `P /\ Q -> P` as `(P /\ Q) -> P` rather than
P /\ (Q -> P) *)
%right ARROW
%right PROD SUM

(* Highest precedence *)

%start <string Ast.list_of_stmts> main

%%

let main := terminated(nonempty_list(stmt), EOF)

let located(x) == ~ = x; { Location.locate x ~source_loc:$loc }

let stmt == located(
  | def_stmt
  | AXIOM; ~ = var_name; COLON; var_type = expr;  { Ast.Axiom {var_name; var_type} }
  | CHECK; ~ = expr;                              { Ast.Check expr }
  | EVAL; ~ = expr;                               { Ast.Eval expr }
  (* theorem var_name : claim := proof
  Unlike def statements, the type annotation here is not optional.
  *)
  | THEOREM; ~=var_name; COLON; claim=expr; COLON_EQ; proof=expr;
  { Ast.Def {var_name; binding=proof; ascribed_type=Some claim} } 
)

(* - def var_name := binding
   - def var_name : ascribed_type := binding
*)
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
  | sigma_expr
  | exists_expr
  | proof_term

let raw_expr :=
  (* This 1st rule is causing shift/reduce conflicts in menhir. *)
  | fn=expr; arg=expr;                           { Ast.App {left=fn; right=arg} } %prec APP
  (* This let rule is also causing shift/reduce conflicts. *)
  | let_expr
  | pair_expr
  | TYPE;                                        { Ast.Type }
  | KIND;                                        { Ast.Kind }
  | FST; ~ = expr;                               <Ast.Fst>
  | SND; ~ = expr;                               <Ast.Snd>
  | ~ = var_name;                                <Ast.Var>
  | ascription

  (* A -> B is abbreviated by `Pi (_ : A), B` *)
  | ~ = expr; ARROW; body = expr;                { Ast.Pi{var_name="_"; expr; body}} %prec ARROW

  (* A * B and A /\ B are abbreviated by `Sigma (_ : A), B` *)
  | ~ = expr; PROD; body = expr;                { Ast.Sigma{var_name="_"; expr; body}} %prec PROD
  | INL; ~ = expr;                                  <Ast.Inl>
  | INR; ~ = expr ;                                 <Ast.Inr>
  | match_expr
  | sum_expr 
  | exists_pair_expr

(* Syntactic sugar inspired by Lean's structured proof terms.
`assume h : p, ...` abbreviates `fun (h : p) => ...`

`have h : p, from s, ...` abbreviates `let h : p := s in ...`

`have p, from s, ...` abbreviates `let this : p := s in ...`
When no name is provided, we implicitly name it with "this".

`show p, from expr` abbreviates `(expr : p)`
This is used to end a proof.
*)
 let proof_term ==
  | ASSUME; ~=fun_arg_list; COMMA; body=expr;
    { let (_, end_pos) = body.source_loc in
      let ((input_var, input_type, _), tail) = List.hd_tl fun_arg_list in
      let tail = List.fold_right
        (fun (input_var, input_type, (start_pos, _)) body ->
          Location.locate ~source_loc:(start_pos, end_pos) (Ast.Fun {input_var; input_type; body}))
        tail body
      in Location.locate ~source_loc:($startpos, end_pos) @@ Ast.Fun {input_var; input_type; body=tail} 
     }
  | located(
    | HAVE; ~=var_name; COLON; claim=expr; COMMA; FROM; proof=expr; COMMA; body=expr;
    { Ast.Let {abstraction={var_name; expr=proof; body}; ascribed_type=Some claim} }
    | HAVE; claim=expr; COMMA; FROM; proof=expr; COMMA; body=expr;
    { Ast.Let {abstraction={var_name="this"; expr=proof; body}; ascribed_type=Some claim} }
    | SHOW; claim=expr; COMMA; FROM; proof=expr;
    { Ast.Ascription {expr=proof; ascribed_type=claim} }
  )

let var_name == VAR_NAME

let annotated_expr == 
  delimited(LPAREN, separated_pair(expr, COLON, expr), RPAREN) 

let ascription ==
      (expr, ascribed_type) = annotated_expr;
    { Ast.Ascription {expr; ascribed_type} }

(* Function expressons *)
let fun_expr == FUN; ~=fun_arg_list; DOUBLE_ARROW; body=expr;
 { let (_, end_pos) = body.source_loc in
   let ((input_var, input_type, _), tail) = List.hd_tl fun_arg_list in
   let tail = List.fold_right
    (fun (input_var, input_type, (start_pos, _)) body ->
      Location.locate ~source_loc:(start_pos, end_pos) @@
        Ast.Fun {input_var; input_type; body})
    tail body
  in Location.locate ~source_loc:($startpos, end_pos) @@ 
    Ast.Fun {input_var; input_type; body=tail} 
 }

let fun_arg_list == nonempty_list(fun_arg)

let annotated_name == separated_pair(var_name, COLON, expr)

let bracketed_annotated_name == 
  delimited(LPAREN, separated_pair(var_name, COLON, expr), RPAREN)

let fun_arg == 
  | ~ = var_name;
    { (var_name, None, $loc) }
  | (var_name, expr_type) = bracketed_annotated_name;
    { (var_name, Some expr_type, $loc) }

(* Pi expressions *)
let pi_expr == 
  | PI; pi_arg_list = nonempty_list(pi_arg); COMMA; output_type=expr;
   { let (_, end_pos) = output_type.source_loc in
   let ((input_var, input_type, _), tail) = List.hd_tl pi_arg_list in
   let tail = List.fold_right
    (fun (input_var, input_type, (start_pos, _)) body ->
      Location.locate ~source_loc:(start_pos, end_pos) @@
        Ast.Pi {var_name=input_var; expr=input_type; body})
    tail output_type
    in Location.locate ~source_loc:($startpos, end_pos) @@ 
         Ast.Pi {var_name=input_var; expr=input_type; body=tail} 
   }

  (* Parens are optional in the event there's only one input variable. *)
  | PI; (input_var, input_type) = annotated_name; COMMA; output_type=expr;
    { Location.locate ~source_loc:$loc
      (Ast.Pi {var_name=input_var; expr=input_type; body=output_type}) }

let pi_arg ==
 | (var_name, expr_type) = bracketed_annotated_name;
    { (var_name, expr_type, $loc) }

let let_expr ==
  | LET; ~=var_name; COLON_EQ; binding=expr; IN; body=expr;
    { Ast.Let {abstraction={var_name; expr=binding; body}; ascribed_type=None} }
  | LET; ~=var_name; COLON; ascribed_type=expr; COLON_EQ; binding=expr; IN; body=expr;
    { Ast.Let {abstraction={var_name; expr=binding; body}; ascribed_type = Some ascribed_type} }
    
  | LET; (witness_var, witness_cert) =
    delimited(LEFT_CURLY, separated_pair(var_name, COMMA, var_name), RIGHT_CURLY);
    COLON_EQ; ~=expr; IN; body=expr;
    { Ast.Exists_elim {expr; witness_var; witness_cert; body} }

let sigma_expr ==
 | SIGMA; pi_arg_list = nonempty_list(pi_arg); COMMA; output_type=expr;
   { let (_, end_pos) = output_type.source_loc in
   let ((input_var, input_type, _), tail) = List.hd_tl pi_arg_list in
   let tail = List.fold_right
    (fun (input_var, input_type, (start_pos, _)) body ->
      Location.locate ~source_loc:(start_pos, end_pos) @@
        Ast.Sigma {var_name=input_var; expr=input_type; body})
    tail output_type
    in Location.locate ~source_loc:($startpos, end_pos) @@ 
         Ast.Sigma {var_name=input_var; expr=input_type; body=tail} 
   }
   (*
  (input_var, input_type) = sigma_arg;
  COMMA; output_type=expr;
  { Ast.Sigma {var_name=input_var; expr=input_type; body=output_type} }
  *)

let exists_expr ==
| EXISTS; pi_arg_list = nonempty_list(pi_arg); COMMA; output_type=expr;
   { let (_, end_pos) = output_type.source_loc in
   let ((input_var, input_type, _), tail) = List.hd_tl pi_arg_list in
   let tail = List.fold_right
    (fun (input_var, input_type, (start_pos, _)) body ->
      Location.locate ~source_loc:(start_pos, end_pos) @@
        Ast.Exists {var_name=input_var; expr=input_type; body})
    tail output_type
    in Location.locate ~source_loc:($startpos, end_pos) @@ 
         Ast.Exists {var_name=input_var; expr=input_type; body=tail} 
   }
   (*
  (input_var, input_type) = sigma_arg;
  COMMA; output_type=expr;
  { Ast.Exists {var_name=input_var; expr=input_type; body=output_type} }
  *)

(*
let sigma_arg == 
  | delimited(option(LPAREN), separated_pair(var_name, COLON, expr), option(RPAREN))
*)

let pair_expr == 
  (left, right) = delimited(LPAREN, separated_pair(expr, COMMA, expr), RPAREN);
  { Ast.Pair {left; right} }

let exists_pair_expr == 
  (left, right) = delimited(LEFT_CURLY, separated_pair(expr, COMMA, expr), RIGHT_CURLY);
  { Ast.Exists_pair {left; right} }

let sum_expr ==
  (left, right) = separated_pair(expr, PLUS, expr);
  { Ast.Sum {left; right} } %prec SUM

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