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
let extend_ctx = CCFun.flip BatVect.prepend

(* Find the index corresponding to the variable name in the naming context *)
let find_index ctx var_name =
  let open CCEither in
  try
    Right (BatVect.findi (Stdlib.(==) var_name) ctx)
  with Not_found -> Left UnknownIndex

(* Find thee variable name corresponding to the index in the naming context *)
let find_var ctx index =
  let open CCEither in
  try
    Right (BatVect.get ctx index)
  with BatVect.Out_of_bounds -> Left UnknownVar

(* Get the length of the naming context *)
let ctx_length = BatVect.length
(******************************************************************************)

(******************************************************************************)
(* Functions to convert from the parser's AST to our internal AST *)

(*
For now, we do not support variable shadowing, so if a variable has been declared
previously, we throw this error.
*)
exception VariablePreviouslyDefd

(* Convert a parsed expression to our internal AST. *)
let rec expr_to_internal_ast ctx expr =
  let open Parsing.Ast in
  match expr with
  | Var var_name ->
    begin
      match find_index ctx var_name with
      | Right index -> Ast.Var index
      (* If we can't find the index of the variable, then it wasn't declared
      previously, ie the variable is unknown. Hence we just throw an error. *)
      | Left exc -> raise exc
    end
  (* For Fun and Pi, we add the input var to the context to get a new one, which
  we then use to convert the body. *)
  | Fun (input_var, body, type_of_fun) ->
    let new_ctx = extend_ctx ctx input_var in
    Ast.Fun (ctx_length ctx,
             expr_to_internal_ast new_ctx body,
             expr_to_internal_ast ctx type_of_fun)
  | Pi (input_var, input_type, ret_type) ->
    let new_ctx = extend_ctx ctx input_var in
    Ast.Pi (ctx_length ctx,
            expr_to_internal_ast ctx input_type,
            expr_to_internal_ast new_ctx ret_type)
  | App (expr1, expr2) ->
    Ast.App (expr_to_internal_ast ctx expr1, expr_to_internal_ast ctx expr2)
  | Type -> Type

(* Convert a parsed statement to our internal AST.
Note that the return type here is actually (Ast.expr * ctx) because Def and Axiom
will modify the context, affecting future statements.
This is important to carry around in stmts_to_internal_ast when converting a
list of statements to our internal AST.
*)
let stmt_to_internal_ast ctx stmt =
  let open Parsing.Ast in
  match stmt with
  | Def (var_name, expr) ->
    begin
      (* Currently, we don't handle variable shadowing. *)
      match find_index ctx var_name with
      | Right _ -> raise VariablePreviouslyDefd
      | Left _ ->
        Ast.Def (var_name, expr_to_internal_ast ctx expr), extend_ctx ctx var_name
    end
  | Axiom (var_name, declared_type) ->
    Ast.Axiom (var_name, expr_to_internal_ast ctx declared_type),
    extend_ctx ctx var_name
  | Eval expr -> Ast.Eval (expr_to_internal_ast ctx expr), ctx
  | Check expr -> Ast.Check (expr_to_internal_ast ctx expr), ctx

let rec stmts_to_internal_ast ctx stmts =
  match stmts with
  (* No more statements to convert to our internal AST. *)
  | [] -> [], ctx
  | stmt :: stmts ->
    (* Convert the head and carry the new context along to convert the tail. *)
    let stmt, ctx = stmt_to_internal_ast ctx stmt in
    let stmts, ctx = stmts_to_internal_ast ctx stmts in
    stmt :: stmts, ctx

(******************************************************************************)

(******************************************************************************)
(* Functions to convert from our internal AST to the parser's AST *)
(* WORK IN PROGRESS, will be based on pg 85 of Types and Programming Languages *)

(******************************************************************************)
