(* This module converts the parser's AST into to our internal AST, which uses
   de Bruijn indices.
*)

open Containers
open Common

(* module PAst = Parsing.Ast
module KAst = Kernel.Ast *)

(******************************************************************************)
(* Functions to convert from the parser's AST to our internal AST *)

exception Unknown_var_name of string Ast.expr
exception Underscore_var_name of Location.source_loc
(* 
let conv ctx : PAst.expr -> KAst.expr =
  let v = 
    object
    inherit [_] PAst.ast_folder as super

    method build_Var _ var_name = KAst.located_kind
    method build_Type _ = KAst.Type
    method build_Kind _ = KAst.Kind

    method build_abstraction _ var_name expr body = assert false

    method build_Pi _ = assert false
    method build_Sigma _ = assert false
    method build_Fun _ _ _ _ = assert false
    method build_App _ _ = assert false
    method build_Let _ = assert false
    method build_Ascription _ = assert false
    method build_Pair _ = assert false
    method build_Fst _ = assert false
    method build_Snd _ = assert false

    method build_located _ _ _ _ = KAst.Kind
    method visit_'a _ _ = KAst.located_kind
    
    end in v#visit_expr @@ Loc.locate ctx *)

(* How to use Visitors package to clean up this boilerplate? *)
let rec parser_to_internal_raw_expr ctx (expr : string Ast.expr) =
  match expr.data with
  | Ast.Var var_name ->
    var_name
    |> begin 
        function
        | "_" -> raise @@ Underscore_var_name expr.source_loc
        | var_name -> var_name
       end
    |> Kernel.Context.var_name_to_index ctx
    |> CCOpt.get_lazy (fun () -> raise @@ Unknown_var_name expr)
    |> fun index -> Ast.Var index

  (* For Fun and Pi, we add the input var to the context to get a new one, which
     we then use to convert the body. *)
  | Ast.Fun {input_var; input_type; body} ->
    let input_type = CCOpt.map (parser_to_internal_expr ctx) input_type in
    let new_ctx = Kernel.Context.add_binding input_var ctx in
    let body = parser_to_internal_expr new_ctx body in
    Ast.Fun {input_var; input_type; body}

  (* | PAst.Pi {input_var; input_type; output_type} ->
    let new_ctx = Kernel.Context.add_binding input_var ctx in
    let input_type = parser_to_internal_expr ctx input_type in
    let output_type = parser_to_internal_expr new_ctx output_type in
    KAst.Pi {input_var; input_type; output_type} *)

  | Ast.App {left=fn; right=arg} ->
    let fn = parser_to_internal_expr ctx fn in
    let arg = parser_to_internal_expr ctx arg in
    Ast.App {left=fn; right=arg}

  | Ast.Ascription {expr; ascribed_type} ->
    let expr = parser_to_internal_expr ctx expr in
    let ascribed_type = parser_to_internal_expr ctx ascribed_type in
    Ast.Ascription {expr; ascribed_type}

  | Ast.Pair {left; right} ->
    let left = parser_to_internal_expr ctx left in
    let right = parser_to_internal_expr ctx right in
    Ast.Pair {left; right}

  | Ast.Fst expr -> Ast.Fst (parser_to_internal_expr ctx expr)
  | Ast.Snd expr -> Ast.Snd (parser_to_internal_expr ctx expr)

  | Ast.Sum {left; right} ->
    let left = parser_to_internal_expr ctx left in
    let right = parser_to_internal_expr ctx right in
    Ast.Sum {left; right}

  | Ast.Match {expr; inl; inr} ->
    let expr = parser_to_internal_expr ctx expr in
    let conv_match_binding ctx ({match_var; match_body} : string Ast.match_binding) =
      let ctx = Kernel.Context.add_binding match_var ctx in
      let match_body = parser_to_internal_expr ctx match_body in
      ({match_var; match_body} : int Ast.match_binding)
    in
    let inl = conv_match_binding ctx inl in
    let inr = conv_match_binding ctx inr in
    Ast.Match {expr; inl; inr}

  | Ast.Inl expr -> Ast.Inl (parser_to_internal_expr ctx expr)
  | Ast.Inr expr -> Ast.Inr (parser_to_internal_expr ctx expr)

  (* | PAst.Let {var_name; binding; body} ->
    let binding = parser_to_internal_expr ctx binding in
    let new_ctx = Kernel.Context.add_binding var_name ctx in
    let body = parser_to_internal_expr new_ctx body in
    KAst.Let {var_name; binding; body} *)

  | Ast.Type -> Ast.Type
  | Ast.Kind -> Ast.Kind

  | Ast.Pi abstraction -> 
    Ast.Pi (parser_to_internal_abstraction ctx abstraction)
  | Ast.Sigma abstraction ->
    Ast.Sigma (parser_to_internal_abstraction ctx abstraction)
  | Ast.Let abstraction ->
    Ast.Let (parser_to_internal_abstraction ctx abstraction)

  (* | Ast.Let_pair {left_var; right_var; binding; body} ->
    let binding = parser_to_internal_expr ctx binding in
    let ctx = ctx
              |> Kernel.Context.add_binding left_var
              |> Kernel.Context.add_binding right_var              
    in
    let body = parser_to_internal_expr ctx body in
    Ast.Let_pair {left_var; right_var; binding; body} *)

and parser_to_internal_abstraction ctx ({var_name; expr; body} : string Ast.abstraction) =
    let expr = parser_to_internal_expr ctx expr in
    let new_ctx = Kernel.Context.add_binding var_name ctx in
    let body = parser_to_internal_expr new_ctx body in
    ({var_name; expr; body} : int Ast.abstraction)

and parser_to_internal_expr ctx expr =
  expr
  |> parser_to_internal_raw_expr ctx
  |> Location.set_data expr

let rec parser_to_internal_raw_stmt raw_stmt ctx =
  match raw_stmt with
  | Ast.Def {var_name; binding} ->
    let binding = parser_to_internal_expr ctx binding in
    let new_ctx = Kernel.Context.add_binding var_name ctx in
    Ast.Def {var_name; binding}, new_ctx

  | Ast.Axiom {var_name; var_type} ->
    let var_type = parser_to_internal_expr ctx var_type in
    let ctx = Kernel.Context.add_binding var_name ctx in
    Ast.Axiom {var_name; var_type}, ctx

  | Ast.Eval expr ->
    let expr = parser_to_internal_expr ctx expr in
    Ast.Eval expr, ctx

  | Ast.Check expr ->
    let expr = parser_to_internal_expr ctx expr in
    Ast.Check expr, ctx

and parser_to_internal_stmt (stmt : string Ast.stmt) ctx =
  let internal_raw_stmt, new_ctx = parser_to_internal_raw_stmt stmt.data ctx in
  Location.set_data stmt internal_raw_stmt, new_ctx

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
  match var_name with
  | "_" -> var_name, Kernel.Context.add_binding var_name ctx
  | _ ->
    let append_prime = (Fun.flip (^)) "'" in
    let is_var_name_free var_name =
      not @@ Kernel.Context.is_var_name_bound ctx var_name
    in
    let new_var_name =
      Common.Utils.until is_var_name_free append_prime var_name in
    new_var_name, Kernel.Context.add_binding new_var_name ctx

let rec internal_to_parser_raw_expr ctx raw_expr =
  match raw_expr with
  | Ast.Var var_index ->
    var_index
    |> Kernel.Context.index_to_var_name ctx
    |> fun var_name -> Ast.Var var_name

  | Ast.Fun {input_var; input_type; body} ->
    let input_type = CCOpt.map (internal_to_parser_expr ctx) input_type in
    let input_var, new_ctx = pick_fresh_name input_var ctx in
    let body = internal_to_parser_expr new_ctx body in
    Ast.Fun {input_var; input_type; body}

  (* | KAst.Pi {var_name=input_var; expr=input_type; body=output_type} ->
    let input_type = internal_to_parser_expr ctx input_type in
    let input_var, new_ctx = pick_fresh_name input_var ctx in
    let output_type = internal_to_parser_expr new_ctx output_type in
    PAst.Pi {var_name=input_var; expr=input_type; body=output_type} *)

  | Ast.App {left=fn; right=arg} ->
    let fn = internal_to_parser_expr ctx fn in
    let arg = internal_to_parser_expr ctx arg in
    Ast.App {left=fn; right=arg}
  
  | Ast.Ascription {expr; ascribed_type} ->
    let expr = internal_to_parser_expr ctx expr in
    let ascribed_type = internal_to_parser_expr ctx ascribed_type in
    Ast.Ascription {expr; ascribed_type}

  | Ast.Pair {left; right} ->
    let left = internal_to_parser_expr ctx left in
    let right = internal_to_parser_expr ctx right in
    Ast.Pair {left; right}

  | Ast.Fst expr -> Ast.Fst (internal_to_parser_expr ctx expr)
  | Ast.Snd expr -> Ast.Snd (internal_to_parser_expr ctx expr)

  | Ast.Sum {left; right} ->
    let left = internal_to_parser_expr ctx left in
    let right = internal_to_parser_expr ctx right in
    Ast.Sum {left; right}

  | Ast.Match {expr; inl; inr} ->
    let expr = internal_to_parser_expr ctx expr in
    let conv_match_binding ctx ({match_var; match_body} : int Ast.match_binding) =
      let match_var, new_ctx = pick_fresh_name match_var ctx in
      let match_body = internal_to_parser_expr new_ctx match_body in
      ({match_var; match_body} : string Ast.match_binding)
    in
    let inl = conv_match_binding ctx inl in
    let inr = conv_match_binding ctx inr in
    Ast.Match {expr; inl; inr}
  | Ast.Inl expr -> Ast.Inl (internal_to_parser_expr ctx expr)
  | Ast.Inr expr -> Ast.Inr (internal_to_parser_expr ctx expr)

  | Ast.Pi abstraction -> 
    Ast.Pi (internal_to_parser_abstraction ctx abstraction)
  | Ast.Sigma abstraction ->
    Ast.Sigma (internal_to_parser_abstraction ctx abstraction)
  | Ast.Let abstraction ->
    Ast.Let (internal_to_parser_abstraction ctx abstraction)

  (* | Ast.Let_pair {left_var; right_var; binding; body} ->
    let binding = internal_to_parser_expr ctx binding in
    let left_var, ctx = pick_fresh_name left_var ctx in
    let right_var, ctx = pick_fresh_name right_var ctx in
    let body = internal_to_parser_expr ctx body in
    Ast.Let_pair {left_var; right_var; binding; body} *)

  | Ast.Type -> Ast.Type
  | Ast.Kind -> Ast.Kind
  
and internal_to_parser_expr ctx expr =
  Location.update_data expr @@ internal_to_parser_raw_expr ctx

and internal_to_parser_abstraction ctx ({var_name; expr; body} : int Ast.abstraction) =
  let expr = internal_to_parser_expr ctx expr in
  let var_name, new_ctx = pick_fresh_name var_name ctx in 
  let body = internal_to_parser_expr new_ctx body in
  ({var_name; expr; body} : string Ast.abstraction)