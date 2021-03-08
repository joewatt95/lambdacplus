(*
This module converts the parser's AST into to our internal AST, which uses
de Bruijn indices.

To convert between both ASTs, we define the naming context and an API for
manipulating it.

Useful references:
https://www.cis.upenn.edu/~bcpierce/tapl/checkers/simplebool/parser.mly
https://www.cis.upenn.edu/~bcpierce/tapl/checkers/untyped/syntax.ml

https://github.com/andrejbauer/spartan-type-theory/blob/3806830b2a52a630696b7d218f033fd92b148d2e/src/desguar.ml
*)

open Containers

(* This is thrown if the index can't be found in the naming context *)
exception UnknownIndex

(* This is thrown if the variable name can't be found in the naming context *)
exception UnknownVar

(* Initial, empty context *)
let empty_ctx = BatVect.empty

(* Extend a context with a variable name *)
let extend_ctx = BatVect.prepend

(* Find the index corresponding to the variable name in the naming context *)
let var_name_to_index var_name ctx =
  let open Either in
  try
    Right (BatVect.findi (Stdlib.(=) var_name) ctx)
  with Not_found -> Left UnknownIndex

(* Find the variable name corresponding to the index in the naming context *)
let index_to_var_name index ctx =
  let open Either in
  try
    Right (BatVect.get ctx index)
  with BatVect.Out_of_bounds -> Left UnknownVar

(* Get the length of the naming context *)
let ctx_length = BatVect.length

(* Check if a variable name is bound in a context *)
let is_var_name_bound ctx var_name = BatVect.exists (Stdlib.(=) var_name) ctx

(* Check if a variable name is free in a context *)
let is_var_name_free ctx = Fun.negate (is_var_name_bound ctx)

(* For debugging *)
let print_ctx ctx = ctx
                    |> BatVect.to_list
                    |> List.to_string Fun.id ~start:"[" ~stop:"]" ~sep:"; "
                    |> print_endline

(******************************************************************************)
(* Functions to convert from the parser's AST to our internal AST *)

(*
For now, we do not support variable shadowing, so if a variable has been declared
previously, we throw this error.
*)
exception VariablePreviouslyDefd

let update_data_with_ctx (located : 'a Parsing.Location.located) f ctx =
  Parsing.Location.update_data_with_f located @@ Fun.flip f ctx

(* Convert a parsed expression to our internal AST. *)
let rec parser_to_internal_raw_expr raw_expr ctx =
  let open Parsing.Ast in
  match raw_expr with
  | Var var_name ->
    (* If we can't find the index of the variable, then it wasn't declared
       previously, ie the variable is unknown. Hence we just throw an error. *)
    Either.fold (var_name_to_index var_name ctx)
      ~left:raise ~right:(fun var_index -> Ast.Var var_index)
  (* For Fun and Pi, we add the input var to the context to get a new one, which
     we then use to convert the body. *)
  | Fun {input_var; body} ->
    let new_ctx = extend_ctx input_var ctx in
    let body = parser_to_internal_expr body new_ctx in
    Ast.Fun {input_var; body}
  | Pi {input_var; input_type; output_type} ->
    let new_ctx = extend_ctx input_var ctx in
    let input_type = parser_to_internal_expr input_type ctx in
    let output_type = parser_to_internal_expr output_type new_ctx in
    Ast.Pi {input_var; input_type; output_type}
  | App {fn; arg} ->
    let fn = parser_to_internal_expr fn ctx in
    let arg = parser_to_internal_expr arg ctx in
    Ast.App {fn; arg}
  | Ascription {expr; expr_type} ->
    let expr = parser_to_internal_expr expr ctx in
    let expr_type = parser_to_internal_expr expr_type ctx in
    Ast.Ascription {expr; expr_type}
  | Type -> Type

and parser_to_internal_expr expr ctx =
  update_data_with_ctx expr parser_to_internal_raw_expr ctx

(* Convert a parsed statement to our internal AST.
Note that the return type here is actually (Ast.expr * ctx) because Def and Axiom
will modify the context, affecting future statements.
This is important to carry around in stmts_to_internal_ast when converting a
list of statements to our internal AST.
*)
let rec parser_to_internal_raw_stmt raw_stmt ctx =
  let open Parsing.Ast in
  match raw_stmt with
  | Def {var_name; var_expr} ->
    (* Currently, we don't handle variable shadowing. *)
    Either.fold (var_name_to_index var_name ctx)
      ~right:(fun _ -> raise VariablePreviouslyDefd)
      ~left:(fun _ ->
          (* This allows for recursive definitions *)
          let new_ctx = extend_ctx var_name ctx in
          let var_expr = parser_to_internal_expr var_expr new_ctx in
          Ast.Def {var_name; var_expr}, new_ctx)
  | Axiom {var_name; var_type} ->
    let var_type = parser_to_internal_expr var_type ctx in
    Ast.Axiom {var_name; var_type},
    extend_ctx var_name ctx
  | Eval expr ->
    let expr = parser_to_internal_expr expr ctx in
    Ast.Eval expr, ctx
  | Check expr ->
    let expr = parser_to_internal_expr expr ctx in
    Ast.Check expr, ctx

and parser_to_internal_stmt stmt ctx =
  (* Here we need to open the Syntax.Location module so that Ocaml can infer
     the type of stmt properly. Otherwise, it complains that data is an unbound
     field of stmt *)
  let open Parsing.Location in
  let internal_raw_stmt, new_ctx = parser_to_internal_raw_stmt stmt.data ctx in
  update_data stmt internal_raw_stmt, new_ctx

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
  let new_var_name =
    Utils.General.until (is_var_name_free ctx) append_prime var_name in
  new_var_name, extend_ctx new_var_name ctx

(* Convert an expression from our internal AST back to the parser's AST *)
let rec internal_to_parser_raw_expr raw_expr ctx =
  let open Parsing.Ast in
  match raw_expr with
  | Ast.Var var_index ->
    Either.fold (index_to_var_name var_index ctx)
      ~right:(fun var_name -> Var var_name)
      ~left:raise
  | Ast.Fun {input_var; body} ->
    let input_var, new_ctx = pick_fresh_name input_var ctx in
    let body = internal_to_parser_expr body new_ctx in
    Fun {input_var; body}
  | Ast.Pi {input_var; input_type; output_type} ->
    let input_var, new_ctx = pick_fresh_name input_var ctx in
    let input_type = internal_to_parser_expr input_type ctx in
    let output_type = internal_to_parser_expr output_type new_ctx in
    Pi {input_var; input_type; output_type}
  | Ast.App {fn; arg} ->
    let fn = internal_to_parser_expr fn ctx in
    let arg = internal_to_parser_expr arg ctx in
    App {fn; arg}
  | Ast.Ascription {expr; expr_type} ->
    let expr = internal_to_parser_expr expr ctx in
    let expr_type = internal_to_parser_expr expr_type ctx in
    Ascription {expr; expr_type}
  | Type -> Type

and internal_to_parser_expr expr ctx =
  update_data_with_ctx expr internal_to_parser_raw_expr ctx

(* let stmt_to_parser_ast ctx stmt =
 *   let open Parsing.Ast in
 *   match stmt with
 *   | Ast.Def (index, expr) -> assert false
 *   | Ast.Axiom (index, declared_type) -> assert false
 *     extend_ctx ctx var_name
 *   | Ast.Eval expr -> Eval (stmt_to_parser_ast ctx stmt), ctx
 *   | Ast.Check expr -> Check (parser_to_internal_expr ctx expr), ctx *)

(******************************************************************************)
