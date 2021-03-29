open Containers

(* This module contains the AST of our langauge. It's parameterized over 'a
  where 'a is the type of the data stored in variable nodes.
  In our case, this is either a string or int.
  The former corresponds to the parser's AST, which spit out by the parser
  after parsing the concrete syntax. Here, variable names are strings that
  the user input.
  For the case when 'a is int, the AST corresponds to our internal AST, where
  the int at the variable nodes represents its de bruijn index.
*)

(* Expressions are raw expressions together with their source locations. *)
type 'a expr = 'a raw_expr Location.located 

(* This is used to represent the Pi and Sigma types as well as the local let
  binding, since they all have similar structure:
    Pi (`var_name` : `expr`), `body`
    Sigma (`var_name` : `expr`), `body`
    Let `var_name` := `expr` in `body`
  Note that `var_name` is bound in `body` but not `expr`.
*)
and 'a abstraction = {
  var_name : (string [@visitors.opaque] [@equal Utils.always_true]);
  expr : 'a expr;
  body : 'a expr
}

(* Used to represent the 2 clauses of match expressions, ie: 
   inl `match_var` -> `match_body`
   inr `match_var` -> `match_body`
  *)
and 'a match_binding = {
  match_var : (string [@visitors.opaque] [@equal Utils.always_true]);
  match_body : 'a expr;
}

(* Used to represent function application, the pair constructor for Sigma types
  and Sum type. *)
and 'a pair = {
  left : 'a expr;
  right : 'a expr
}

(* Raw expressions *)
and 'a raw_expr =
  (* 2 sorted universe *)
  | Type
  | Kind
  (* Variables *)
  | Var of ('a [@visitors.opaque])

  (* Pi types *)
  | Pi of 'a abstraction 
  | Fun of
    { input_var : (string [@visitors.opaque] [@equal Utils.always_true]);
      input_type : ('a expr option [@visitors.opaque]);
      body : 'a expr }
  | App of 'a pair

  (* Sigma types *)
  | Sigma of 'a abstraction
  | Pair of 'a pair
  | Fst of 'a expr
  | Snd of 'a expr

  (* Sum types *)
  | Sum of 'a pair
  | Inl of 'a expr
  | Inr of 'a expr
  | Match of 
    { expr : 'a expr; 
      inl : 'a match_binding; 
      inr : 'a match_binding }
 
  (* Optional type ascriptons *)
  | Ascription of { expr : 'a expr; ascribed_type : 'a expr }

  (* Local let binding *)
  | Let of 'a abstraction

  (* | Let_pair of
    { left_var : (string [@visitors.opaque]);
      right_var : (string [@visitors.opaque]);
      binding : 'a expr;
      body : 'a expr } *)

[@@deriving show, fields, eq,
  visitors {variety="map"; ancestors=["Location.fold"]}, 
  visitors {variety="fold"; ancestors=["Location.fold"]}]

let located_kind = Location.locate Kind

(* Template for defining functions that map over the AST while preserving source
   locations. *)
class virtual ['self] ast_mapper =
  object (_ : 'self)
    inherit [_] map as super

    method! visit_expr env ({data; _} as expr) = 
      data
      |> super#visit_raw_expr env
      |> Location.set_data expr

    (* These are never called. *)
    method visit_'a _ _ = located_kind
    method build_located _ _ _ _ = located_kind
  end

(* Template for defining catamorphisms over the AST that ignore source 
   locations. *)
class virtual ['self] ast_folder =
  object (_ : 'self)
    inherit [_] fold as super

    method! visit_expr env {data; _} = super#visit_raw_expr env data
  end

(* Shift operation for de bruijn ASTs. *)
let shift shift_by =
  let v = 
    object (self) 
      inherit [_] ast_mapper

      method! visit_Var {data=cutoff; _} index =
        if index >= cutoff then Var (index + shift_by) else Var index 

      method! visit_Fun cutoff input_var input_type body =
        let input_type = CCOpt.map (self#visit_expr cutoff) input_type in
        let body = self#shift_under_binder cutoff body in
        (* let body = self#visit_expr (self#incr_cutoff cutoff) body in *)
        Fun {input_var; input_type; body}

      method! visit_abstraction cutoff {var_name; expr; body} =
        let expr = self#visit_expr cutoff expr in
        let body = self#shift_under_binder cutoff body in
        (* let body = self#visit_expr (self#incr_cutoff cutoff) body in *)
        {var_name; expr; body}

      method! visit_match_binding cutoff {match_var; match_body} =
        let match_body = self#shift_under_binder cutoff match_body in
        {match_var; match_body}

      (* method! visit_Let_pair cutoff left_var right_var binding body = 
        let binding = self#visit_expr cutoff binding in
        let body = self#shift_under_binder 2 cutoff body in
        Let_pair {left_var; right_var; binding; body} *)

      (* Increment the cutoff whenever we go under a binder. *)
      method shift_under_binder {data=cutoff; _} expr =
        let new_cutoff = Location.locate @@ cutoff + 1 in
        self#visit_expr new_cutoff expr 

      (* Note that we don't need to impement the visit_App, visit_Type,
         visit_Kind and visit_Ascription methods since the Visitors package
         automatically handles all that boilerplate for us. *)
  end in v#visit_expr @@ Location.locate 0

type 'a list_of_exprs = 'a expr list
[@@deriving show]

type 'a stmt = 'a raw_stmt Location.located 
and 'a raw_stmt =
  | Def of {var_name : string; binding : 'a expr}
  | Axiom of {var_name : string; var_type : 'a expr}
  | Check of 'a expr
  | Eval of 'a expr
[@@deriving show]

type 'a list_of_stmts = 'a stmt list
[@@deriving show]

(* let shift shift_by (expr : expr) =
  let rec shift_raw_expr cutoff raw_expr =
    match raw_expr with
    | Type | Kind -> raw_expr

    | Var index as var ->
      if index >= cutoff then Var (index + shift_by) else var

    | Pi {input_var; input_type; output_type} ->
      let input_type = shift_expr cutoff input_type in
      let output_type = shift_expr (cutoff + 1) output_type in
      Pi {input_var; input_type; output_type}

    | Fun {input_var; input_type; body} ->
      let input_type = CCOpt.map (shift_expr cutoff) input_type in
      let body = shift_expr (cutoff + 1) body in
      Fun {input_var; input_type; body}

    | App {fn; arg} ->
      let fn = shift_expr cutoff fn in
      let arg = shift_expr cutoff arg in
      App {fn; arg}

    | Ascription {expr; expr_type} ->
      let expr = shift_expr cutoff expr in
      let expr_type = shift_expr cutoff expr_type in
      Ascription {expr; expr_type}
    
    | Let {var_name; binding; body} ->
      let binding = shift_expr cutoff binding in
      let body = shift_expr (cutoff + 1) body in
      Let {var_name; binding; body}

  and shift_expr cutoff expr =
    Loc.update_data expr @@ shift_raw_expr cutoff

  in shift_expr 0 expr *)