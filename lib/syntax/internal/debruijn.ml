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

(******************************************************************************)
(* API for the naming context *)

(*
BatVect is a module provided by the Batteries library. It's a persistent array-like
data structure with:
- O(1) length
- amortized O(1) prepend/append
- O(log N) set/get
*)

(* This is thrown if the index can't be found in the naming context *)
exception UnknownIndex

(* This is thrown if the variable name can't be found in the naming context *)
exception UnknownVar

type naming_ctx = string BatVect.t

(* Initial, empty context *)
let empty_ctx = BatVect.empty

(* Extend a context with a variable name *)
let extend_ctx = BatVect.prepend

(* Find the index corresponding to the variable name in the naming context *)
let var_name_to_index var_name ctx =
  let open CCEither in
  try
    Right (BatVect.findi (Stdlib.(=) var_name) ctx)
  with Not_found -> Left UnknownIndex

(* Find thee variable name corresponding to the index in the naming context *)
let index_to_var_name index ctx =
  let open CCEither in
  try
    Right (BatVect.get ctx index)
  with BatVect.Out_of_bounds -> Left UnknownVar

(* Get the length of the naming context *)
let ctx_length = BatVect.length

(* Check if a variable name is bound in a context *)
let is_var_name_bound ctx var_name = BatVect.exists (Stdlib.(=) var_name) ctx

(* Check if a variable name is free in a context *)
let is_var_name_free ctx = CCFun.negate (is_var_name_bound ctx)

(* For debugging *)
let print_ctx ctx = ctx
                    |> BatVect.to_list
                    |> CCList.to_string CCFun.id ~start:"[" ~stop:"]" ~sep:";"
                    |> print_endline

(******************************************************************************)

(******************************************************************************)
(* Functions to convert from the parser's AST to our internal AST *)

(*
For now, we do not support variable shadowing, so if a variable has been declared
previously, we throw this error.
*)
exception VariablePreviouslyDefd

(* Convert a parsed expression to our internal AST. *)
let rec parser_to_internal_expr expr ctx =
  let open Parsing.Ast in
  match expr with
  | Var var_name ->
    (* If we can't find the index of the variable, then it wasn't declared
       previously, ie the variable is unknown. Hence we just throw an error. *)
    CCEither.fold (var_name_to_index var_name ctx)
      ~left:raise ~right:(fun index -> Ast.Var index)
  (* For Fun and Pi, we add the input var to the context to get a new one, which
  we then use to convert the body. *)
  | Fun (input_var, body, type_of_fun) ->
    let new_ctx = extend_ctx input_var ctx in
    Ast.Fun (input_var,
             parser_to_internal_expr body new_ctx,
             parser_to_internal_expr type_of_fun ctx)
  | Pi (input_var, input_type, ret_type) ->
    let new_ctx = extend_ctx input_var ctx in
    Ast.Pi (input_var,
            parser_to_internal_expr input_type ctx,
            parser_to_internal_expr ret_type new_ctx)
  | App (expr1, expr2) ->
    Ast.App (parser_to_internal_expr expr1 ctx, parser_to_internal_expr expr2 ctx)
  | Type -> Type

(* Convert a parsed statement to our internal AST.
Note that the return type here is actually (Ast.expr * ctx) because Def and Axiom
will modify the context, affecting future statements.
This is important to carry around in stmts_to_internal_ast when converting a
list of statements to our internal AST.
*)
let parser_to_internal_stmt stmt ctx =
  let open Parsing.Ast in
  match stmt with
  | Def (var_name, expr) ->
    (* Currently, we don't handle variable shadowing. *)
    CCEither.fold (var_name_to_index var_name ctx)
      ~right:(fun _ -> raise VariablePreviouslyDefd)
      ~left:(fun _ ->
          (* This allows for recursive definitions *)
          let new_ctx = extend_ctx var_name ctx in
          Ast.Def (var_name, parser_to_internal_expr expr new_ctx), new_ctx)
  | Axiom (var_name, declared_type) ->
    Ast.Axiom (var_name, parser_to_internal_expr declared_type ctx),
    extend_ctx var_name ctx
  | Eval expr -> Ast.Eval (parser_to_internal_expr expr ctx), ctx
  | Check expr -> Ast.Check (parser_to_internal_expr expr ctx), ctx

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

let pick_fresh_name ctx var_name =
  let append_prime = (CCFun.flip (^)) "'" in
  let new_var_name =
    Utils.General.until (is_var_name_free ctx) append_prime var_name in
  extend_ctx new_var_name ctx, new_var_name

(* Convert an expression from our internal AST back to the parser's AST *)
let rec internal_to_parser_expr ctx expr =
  let open Parsing.Ast in
  match expr with
  | Ast.Var index ->
    CCEither.fold (index_to_var_name index ctx)
      ~right:(fun var_name -> Var var_name)
      ~left:raise
  | Ast.Fun (input_var, body, type_of_fun) ->
    let new_ctx, input_var = pick_fresh_name ctx input_var in
    Fun (input_var,
         internal_to_parser_expr new_ctx body,
         internal_to_parser_expr ctx type_of_fun)
  | Ast.Pi (input_var, input_type, ret_type) ->
    let new_ctx, input_var = pick_fresh_name ctx input_var in
    Pi (input_var,
        internal_to_parser_expr ctx input_type,
        internal_to_parser_expr new_ctx ret_type)
  | Ast.App (expr1, expr2) ->
    App (internal_to_parser_expr ctx expr1, internal_to_parser_expr ctx expr2)
  | Type -> Type

(* let stmt_to_parser_ast ctx stmt =
 *   let open Parsing.Ast in
 *   match stmt with
 *   | Ast.Def (index, expr) -> assert false
 *   | Ast.Axiom (index, declared_type) -> assert false
 *     extend_ctx ctx var_name
 *   | Ast.Eval expr -> Eval (stmt_to_parser_ast ctx stmt), ctx
 *   | Ast.Check expr -> Check (parser_to_internal_expr ctx expr), ctx *)

(******************************************************************************)
