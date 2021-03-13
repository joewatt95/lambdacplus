(*
This module converts the parser's AST into to our internal AST, which uses
de Bruijn indices.

Useful references:
https://www.cis.upenn.edu/~bcpierce/tapl/checkers/simplebool/parser.mly
https://www.cis.upenn.edu/~bcpierce/tapl/checkers/untyped/syntax.ml

https://github.com/andrejbauer/spartan-type-theory/blob/3806830b2a52a630696b7d218f033fd92b148d2e/src/desguar.ml
*)

open Containers

module Loc = Parsing.Location
module PAst = Parsing.Ast

(******************************************************************************)
(* Functions to convert from the parser's AST to our internal AST *)

let update_data_with_ctx (located : 'a Loc.located) f ctx =
  Loc.update_data located @@ Fun.flip f ctx

(* Convert a parsed expression to our internal AST. *)
let rec parser_to_internal_raw_expr raw_expr ctx =
  match raw_expr with
  | PAst.Var var_name ->
    let var_index = Context.var_name_to_index var_name ctx in
    Ast.Var var_index

  (* For Fun and Pi, we add the input var to the context to get a new one, which
     we then use to convert the body. *)
  | PAst.Fun {input_var; body} ->
    let new_ctx = Context.add_binding input_var ctx in
    let body = parser_to_internal_expr body new_ctx in
    Ast.Fun {input_var; body}

  | PAst.Pi {input_var; input_type; output_type} ->
    let new_ctx = Context.add_binding input_var ctx in
    let input_type = parser_to_internal_expr input_type ctx in
    let output_type = parser_to_internal_expr output_type new_ctx in
    Ast.Pi {input_var; input_type; output_type}

  | PAst.App {fn; arg} ->
    let fn = parser_to_internal_expr fn ctx in
    let arg = parser_to_internal_expr arg ctx in
    Ast.App {fn; arg}

  | PAst.Ascription {expr; expr_type} ->
    let expr = parser_to_internal_expr expr ctx in
    let expr_type = parser_to_internal_expr expr_type ctx in
    Ast.Ascription {expr; expr_type}

  | Type -> Type

and parser_to_internal_expr expr ctx =
  update_data_with_ctx expr parser_to_internal_raw_expr ctx

(* Convert a parsed statement to our internal AST.
   Note that the return type here is actually (Ast.expr * ctx) because Def and
   Axiom will modify the context, affecting future statements.
   This is important to carry around in stmts_to_internal_ast when converting a
   list of statements to our internal AST.
*)
let rec parser_to_internal_raw_stmt raw_stmt ctx =
  match raw_stmt with
  | PAst.Def {var_name; binding} ->
    let binding = parser_to_internal_expr binding ctx in
    let new_ctx = Context.add_binding var_name ctx in
    Ast.Def {var_name; binding}, new_ctx

  | PAst.Axiom {var_name; var_type} ->
    let var_type = parser_to_internal_expr var_type ctx in
    let ctx = Context.add_binding var_name ctx in
    Ast.Axiom {var_name; var_type}, ctx

  | PAst.Eval expr ->
    let expr = parser_to_internal_expr expr ctx in
    Ast.Eval expr, ctx

  | PAst.Check expr ->
    let expr = parser_to_internal_expr expr ctx in
    Ast.Check expr, ctx

and parser_to_internal_stmt stmt ctx =
  (* Here we need to open the Syntax.Location module so that Ocaml can infer
     the type of stmt properly. Otherwise, it complains that data is an unbound
     field of stmt *)
  let open Parsing.Location in
  let internal_raw_stmt, new_ctx = parser_to_internal_raw_stmt stmt.data ctx in
  set_data stmt internal_raw_stmt, new_ctx

(*
This converts a list of parser statements to our internal AST.
We do so by converting the head and then carrying along the new context to
convert the tail.
Note that the resulting list of statements is reversed since we keep cons'ing
newly converted statements onto the accumulator. Hence we need to call List.rev
at the end.
*)
let parser_to_internal_stmts stmts ctx =
  let combine (stmts, ctx) stmt =
    let stmt, ctx = parser_to_internal_stmt stmt ctx in
    stmt :: stmts, ctx
  in
  let stmts, ctx = List.fold_left combine ([], ctx) stmts in
  List.rev stmts, ctx

(******************************************************************************)

(******************************************************************************)
(* Functions to convert from our internal AST to the parser's AST *)
(* Based on pg 85 of Types and Programming Languages *)

let pick_fresh_name var_name ctx =
  let append_prime = (Fun.flip (^)) "'" in
  let is_var_name_free var_name =
    not @@ Context.is_var_name_bound var_name ctx in
  let new_var_name =
    Utils.General.until is_var_name_free append_prime var_name in
  new_var_name, Context.add_binding new_var_name ctx

(* Convert an expression from our internal AST back to the parser's AST *)
let rec internal_to_parser_raw_expr raw_expr ctx =
  match raw_expr with
  | Ast.Var var_index ->
    let var_name = Context.index_to_var_name var_index ctx in
    PAst.Var var_name

  | Ast.Fun {input_var; body} ->
    let input_var, new_ctx = pick_fresh_name input_var ctx in
    let body = internal_to_parser_expr body new_ctx in
    PAst.Fun {input_var; body}

  | Ast.Pi {input_var; input_type; output_type} ->
    let input_var, new_ctx = pick_fresh_name input_var ctx in
    let input_type = internal_to_parser_expr input_type ctx in
    let output_type = internal_to_parser_expr output_type new_ctx in
    PAst.Pi {input_var; input_type; output_type}

  | Ast.App {fn; arg} ->
    let fn = internal_to_parser_expr fn ctx in
    let arg = internal_to_parser_expr arg ctx in
    PAst.App {fn; arg}

  | Ast.Ascription {expr; expr_type} ->
    let expr = internal_to_parser_expr expr ctx in
    let expr_type = internal_to_parser_expr expr_type ctx in
    PAst.Ascription {expr; expr_type}

  | Type -> PAst.Type

and internal_to_parser_expr expr ctx =
  update_data_with_ctx expr internal_to_parser_raw_expr ctx

(* let stmt_to_parser_ast ctx stmt =
 *   let open Parsing.Ast in
 *   match stmt with
 *   | Ast.Def (index, expr) -> assert false
 *   | Ast.Axiom (index, declared_type) -> assert false
 *     Context.add_name_binding ctx var_name
 *   | Ast.Eval expr -> Eval (stmt_to_parser_ast ctx stmt), ctx
 *   | Ast.Check expr -> Check (parser_to_internal_expr ctx expr), ctx *)

(******************************************************************************)
