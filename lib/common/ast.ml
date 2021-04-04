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
inr `match_var` -> `match_body` *)
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
  | Type | Kind

  (* Variables
  'a is string for the parser's AST and int for the internal AST using de
  bruijn indices.  *)
  | Var of ('a [@visitors.opaque])

  (* Pi types *)
  | Pi of 'a abstraction 
  | Fun of
    { input_var : (string [@visitors.opaque] [@equal Utils.always_true]);
      input_type : ('a expr option [@visitors.opaque] [@equal Utils.always_true]);
      body : 'a expr }
  | App of 'a pair

  (* Sigma types *)
  | Sigma of 'a abstraction
  | Pair of 'a pair
  | Fst of 'a expr
  | Snd of 'a expr

  (* Existential quantifier *)
  | Exists_pair of 'a pair
  | Exists of 'a abstraction

  (* let {witness_var, witness_cert} := expr in body *)
  | Exists_elim of 
    { expr : 'a expr; 
      witness_var : (string [@visitors.opaque] [@equal Utils.always_true]);
      witness_cert: (string [@visitors.opaque] [@equal Utils.always_true]);
      (* witness_prop : 'a expr; *)
      body : 'a expr }

  (* Sum types *)
  | Sum of 'a pair
  | Inl of 'a expr
  | Inr of 'a expr

  (* match expr with
     | inl
     | inr *)
  | Match of 
    { expr : 'a expr; 
      inl : 'a match_binding; 
      inr : 'a match_binding }
 
  (* Optional type ascriptons *)
  | Ascription of { expr : 'a expr; ascribed_type : 'a expr }

  (* Local let binding *)
  | Let of 
    { abstraction : 'a abstraction; 
      ascribed_type : ('a expr option [@visitors.opaque]) }

  (* | Let_pair of
    { left_var : (string [@visitors.opaque]);
      right_var : (string [@visitors.opaque]);
      binding : 'a expr;
      body : 'a expr } *)

(* Derive show, fields and eq methods ala Haskell. *)
[@@deriving show, fields, eq,
  (* Use the visitors package to derive functions that map and fold over our AST. *)
  visitors {variety="map"; ancestors=["Location.fold"]},
  visitors {variety="fold"; ancestors=["Location.fold"]},
  visitors {variety="iter"; ancestors=["Location.fold"]}]

let located_kind = Location.locate Kind

(* Template for defining functions that map over the AST while preserving source
locations. This is used by the functions which traverse our AST and perform
the shift, substitute and normalize operations.

Note that we don't need to impement the visit_App, visit_Type,
visit_Kind and visit_Ascription methods since the Visitors package
automatically handles all that boilerplate for us.

We only need to override methods that visit AST nodes which contain binders
since that is where we need to do something that differs from the default
visiting strategy.

`env` is a parameter that is threaded through all the mapping operations.
We use this to carry information as we traverse deeper into our AST.
*)
class virtual ['self] ast_mapper =
  object (self : 'self)
    inherit [_] map as super

    (* Map over the underlying raw expression and preserve the source location. *)
    method! visit_expr env expr =
      Location.update_data expr @@ super#visit_raw_expr env

    (* Existential elimination expressions are to be treated as binary functions
    for mapping operations like shifting, substitution and normalization.
    To map over `let {witness_var, witness_cert} := expr in body`, we 
    map over expr and then we transform the body to
    `fun witness_var => fun witness_cert => E'`
    and then map over that.
    This saves us the trouble of having to worry about binders and shifting
    indices. *)
    method visit_Exists_elim_body env witness_var witness_cert body =
      let make_fun input_var body = Fun {input_var; body; input_type=None} in
      body
      (* Construct fun witness_cert => body*)
      |> make_fun witness_cert
      |> Location.locate
      (* Construct fun witness_var => fun witness_cert => body *)
      |> make_fun witness_var
      (* Map over this new AST node *)
      |> self#visit_raw_expr env
      (* Pull apart fun witness_var => fun witness_cert => body and grab the 
      body *)
      |> begin function
         | Fun {body={data=Fun {body; _}; _}; _} -> body 

         (* Nothing else is possible. *)
         | _ -> assert false
         end

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

(* Shift operation for de bruijn ASTs. The main thing to bear in mind is that
the cutoff index must be incremented whenever we go under a binder. *)
let shift shift_by =
  let visitor = 
    object (self) 
      inherit [_] ast_mapper

      (* If the index at the variable is at least `cutoff`, we shift it by
      `shift_by`. *)
      method! visit_Var {data=cutoff; _} index =
        if index >= cutoff then Var (index + shift_by) else Var index 

      method! visit_Fun cutoff input_var input_type body =
        let input_type = Option.map (self#visit_expr cutoff) input_type in
        let body = self#shift_under_binder cutoff body in
        Fun {input_var; input_type; body}

      method! visit_abstraction cutoff {var_name; expr; body} =
        let expr = self#visit_expr cutoff expr in
        let body = self#shift_under_binder cutoff body in
        (* let body = self#visit_expr (self#incr_cutoff cutoff) body in *)
        {var_name; expr; body}

      method! visit_Let cutoff abstraction ascribed_type =
        let ascribed_type = Option.map (self#visit_expr cutoff) ascribed_type in
        let abstraction = self#visit_abstraction cutoff abstraction in
        Let {abstraction; ascribed_type} 

      method! visit_match_binding cutoff {match_var; match_body} =
        let match_body = self#shift_under_binder cutoff match_body in
        {match_var; match_body}

      method! visit_Exists_elim cutoff expr witness_var witness_cert body =
        let expr = self#visit_expr cutoff expr in
        let body = self#visit_Exists_elim_body cutoff witness_var witness_cert body in
        Exists_elim {expr; witness_var; witness_cert; body}

      (* method! visit_Let_pair cutoff left_var right_var binding body = 
        let binding = self#visit_expr cutoff binding in
        let body = self#shift_under_binder 2 cutoff body in
        Let_pair {left_var; right_var; binding; body} *)

      (* Increment the cutoff whenever we go under a binder. *)
      method shift_under_binder {data=cutoff; _} expr =
        let new_cutoff = Location.locate @@ cutoff + 1 in
        self#visit_expr new_cutoff expr
      
  end in visitor#visit_expr @@ Location.locate 0

(* Search for variables with de bruijn index of `index` in the AST. If
any such indices are found, `f` is called.
As before, the key thing is to increment the indices whenever we go under a 
binder. *)
let do_if_index_present index f =
  let visitor =
    object (self)
      inherit [_] iter as super

      method! visit_Var {data=index; _} var_index =
        (* Printf.printf "Var index: %d\n" var_index;
        Printf.printf "Looking for: %d\n" index; *)
        if var_index = index then f index

      method! visit_Fun index _ input_type body =
        Option.iter (self#visit_expr index) input_type;
        self#check_under_binder index body
        (* let body = self#visit_expr (self#incr_cutoff cutoff) body in *)

      method! visit_abstraction index {expr; body; _} =
        self#visit_expr index expr;
        self#check_under_binder index body
        (* let body = self#visit_expr (self#incr_cutoff cutoff) body in *)

      method! visit_Let index abstraction ascribed_type =
        Option.iter (self#visit_expr index) ascribed_type;
        self#visit_abstraction index abstraction

      method! visit_match_binding index {match_body; _} =
        self#check_under_binder index match_body

      (* Here we need to increment the index by 2 since the body is hidden under
      2 binders. *)
      method! visit_Exists_elim index expr _ _ body =
        self#visit_expr index expr;
        let index = Location.update_data index @@ (+) 2 in
        self#visit_expr index body

      (* Increment the index whenever we go under a binder. *)
      method check_under_binder {data=index; _} expr =
        self#visit_expr (Location.locate @@ index + 1) expr 

      method! visit_expr index {data; _} = super#visit_raw_expr index data

      (* Unused dummies. *)
      method visit_'a _ _ = ()
      method build_located _ _ _ _ = ()
  end in visitor#visit_expr @@ Location.locate index

type 'a list_of_exprs = 'a expr list
[@@deriving show]

(* Statements *)
type 'a stmt = 'a raw_stmt Location.located
and 'a raw_stmt =
  | Def of {var_name : string; 
            binding : 'a expr; 
            ascribed_type : ('a expr option [@visitors.opaque] [@Utils.always_true])}
  | Axiom of {var_name : string; var_type : 'a expr}
  | Check of 'a expr
  | Eval of 'a expr
[@@deriving show]

type 'a list_of_stmts = 'a stmt list
[@@deriving show]