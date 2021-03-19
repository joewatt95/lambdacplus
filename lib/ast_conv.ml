(* This module converts the parser's AST into to our internal AST, which uses
   de Bruijn indices.
*)

open Containers

module Loc = Common.Location
module PAst = Parsing.Ast
module KAst = Kernel.Ast

(******************************************************************************)
(* Functions to convert from the parser's AST to our internal AST *)

exception Unknown_var_name of PAst.expr
exception Underscore_var_name of PAst.expr

(* let () = Printexc.register_printer @@
  function
  | Unknown_var_name {data=PAst.Var var_name; source_loc} ->
    Some ("Unknown variable name " ^ var_name ^ " at " ^ Loc.show_source_loc source_loc)
  | Underscore_var_name {data=PAst.Var _; _} ->
    Some ("Underscore is not a valid var name.")
  | _ -> None *)

let rec parser_to_internal_raw_expr ctx (expr : PAst.expr) =
  match expr.data with
  | PAst.Var var_name ->
    var_name
    |> begin 
        function
        | "_" -> raise @@ Underscore_var_name expr
        | var_name -> var_name 
       end
    |> Kernel.Context.var_name_to_index ctx
    |> CCOpt.get_lazy (fun () -> raise @@ Unknown_var_name expr)
    |> fun index -> KAst.Var index

  (* For Fun and Pi, we add the input var to the context to get a new one, which
     we then use to convert the body. *)
  | PAst.Fun {input_var; input_type; body} ->
    let input_type = CCOpt.map (parser_to_internal_expr ctx) input_type in
    let new_ctx = Kernel.Context.add_binding input_var ctx in
    let body = parser_to_internal_expr new_ctx body in
    KAst.Fun {input_var; input_type; body}

  | PAst.Pi {input_var; input_type; output_type} ->
    let new_ctx = Kernel.Context.add_binding input_var ctx in
    let input_type = parser_to_internal_expr ctx input_type in
    let output_type = parser_to_internal_expr new_ctx output_type in
    KAst.Pi {input_var; input_type; output_type}

  | PAst.App {fn; arg} ->
    let fn = parser_to_internal_expr ctx fn in
    let arg = parser_to_internal_expr ctx arg in
    KAst.App {fn; arg}

  | PAst.Ascription {expr; expr_type} ->
    let expr = parser_to_internal_expr ctx expr in
    let expr_type = parser_to_internal_expr ctx expr_type in
    KAst.Ascription {expr; expr_type}
  
  | PAst.Let {var_name; binding; body} ->
    let binding = parser_to_internal_expr ctx binding in
    let new_ctx = Kernel.Context.add_binding var_name ctx in
    let body = parser_to_internal_expr new_ctx body in
    KAst.Let {var_name; binding; body}

  | PAst.Type -> KAst.Type
  | PAst.Kind -> KAst.Kind

and parser_to_internal_expr ctx expr =
  expr
  |> parser_to_internal_raw_expr ctx
  |> Loc.set_data expr

let rec parser_to_internal_raw_stmt raw_stmt ctx =
  match raw_stmt with
  | PAst.Def {var_name; binding} ->
    let binding = parser_to_internal_expr ctx binding in
    let new_ctx = Kernel.Context.add_binding var_name ctx in
    KAst.Def {var_name; binding}, new_ctx

  | PAst.Axiom {var_name; var_type} ->
    let var_type = parser_to_internal_expr ctx var_type in
    let ctx = Kernel.Context.add_binding var_name ctx in
    KAst.Axiom {var_name; var_type}, ctx

  | PAst.Eval expr ->
    let expr = parser_to_internal_expr ctx expr in
    KAst.Eval expr, ctx

  | PAst.Check expr ->
    let expr = parser_to_internal_expr ctx expr in
    KAst.Check expr, ctx

and parser_to_internal_stmt (stmt : PAst.stmt) ctx =
  let internal_raw_stmt, new_ctx = parser_to_internal_raw_stmt stmt.data ctx in
  Loc.set_data stmt internal_raw_stmt, new_ctx

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
    not @@ Kernel.Context.is_var_name_bound ctx var_name
  in
  let new_var_name =
     Common.Utils.until is_var_name_free append_prime var_name in
  new_var_name, Kernel.Context.add_binding new_var_name ctx

let rec internal_to_parser_raw_expr ctx raw_expr =
  match raw_expr with
  | KAst.Var var_index ->
    var_index
    |> Kernel.Context.index_to_var_name ctx
    |> fun var_name -> PAst.Var var_name

  | KAst.Fun {input_var; input_type; body} ->
    let input_type = CCOpt.map (internal_to_parser_expr ctx) input_type in
    let input_var, new_ctx = pick_fresh_name input_var ctx in
    let body = internal_to_parser_expr new_ctx body in
    PAst.Fun {input_var; input_type; body}

  | KAst.Pi {input_var; input_type; output_type} ->
    let input_type = internal_to_parser_expr ctx input_type in
    let input_var, new_ctx = pick_fresh_name input_var ctx in
    let output_type = internal_to_parser_expr new_ctx output_type in
    PAst.Pi {input_var; input_type; output_type}

  | KAst.App {fn; arg} ->
    let fn = internal_to_parser_expr ctx fn in
    let arg = internal_to_parser_expr ctx arg in
    PAst.App {fn; arg}

  | KAst.Ascription {expr; expr_type} ->
    let expr = internal_to_parser_expr ctx expr  in
    let expr_type = internal_to_parser_expr ctx expr_type in
    PAst.Ascription {expr; expr_type}

  | Type -> PAst.Type

  | Kind -> PAst.Kind
  
  | KAst.Let _ -> assert false

and internal_to_parser_expr ctx expr =
  Loc.update_data expr @@ internal_to_parser_raw_expr ctx

(* let stmt_to_parser_ast ctx stmt =
 *   let open Parsing.Ast in
 *   match stmt with
 *   | Ast.Def (index, expr) -> assert false
 *   | Ast.Axiom (index, declared_type) -> assert false
 *     Context.add_name_binding ctx var_name
 *   | Ast.Eval expr -> Eval (stmt_to_parser_ast ctx stmt), ctx
 *   | Ast.Check expr -> Check (parser_to_internal_expr ctx expr), ctx *)

(******************************************************************************)