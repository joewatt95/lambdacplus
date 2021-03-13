(* Concrete implementation of the Context module.
  Concretely, contexts are finger trees, as implemented in the BFT
  module in the Batteries library.
  Finger trees are purely functional random access lists with logarithmic time
  concatenation and access, and amortized constant time insertion at both ends.
*)

open Containers

(* For convenience *)
module BFT = BatFingerTree
let (%>) = Fun.(%>)

type entry = {
  var_name : string; (* The name of the variable*)
  var_type : Ast.expr option; (* The type of the variable*)
  binding  : Ast.expr option (* The binding of the varaiable *)
} [@@deriving show]

exception UnknownIndex
exception UnknownTypeBinding

type t = entry BFT.t

let show =
  BFT.to_list %>
    List.to_string show_entry ~start:"[" ~stop:"]" ~sep:";" 

let empty = BFT.empty

let is_empty = BFT.is_empty

let length = BFT.size

(*
This loops through every entry in ctx and increments var_type and
binding if they are variables with de bruijn indices.

We need to do this everytime we add something to the context so the indices
refer to the correct thing. To see why this is needed, consider the following
example.

We start with an empty context and add (T, Type, _) to it. Then we later infer
the type of x to be T, ie a variable of index 0. Then we add (x, 0, _) to the
context. Now our context looks like:
      [(x, 0, _); (T, Type, _)]
But now that we have cons'd the new entry onto the context, the index of the
type variable T in the new context is no longer 0. It's actually 1. 

Hence whenever we cons something onto the context, we need to loop through the 
context and check increment the indices in var_type and binding if they're of 
type (Var i). 
Otherwise, ie if they aren't de bruijn indices of variables, we leave them alone.

The same problem occurs with indices in bindings too.
*)
let incr_indices ctx =
  let incr_index = CCOpt.map @@ fun (expr : Ast.expr) ->
    match expr.data with
    | Ast.Var _ -> Parsing.Location.update_data expr @@
      begin
        function
        | Ast.Var index -> Ast.Var (index + 1)
        | _ -> assert false
      end
    | _ -> expr 
  in
  BFT.map 
    (fun entry -> { entry with var_type = incr_index entry.var_type;
                               binding = incr_index entry.binding})
    ctx

let add_binding var_name ?var_type ?binding =
  Fun.flip BFT.cons {var_name; var_type; binding} %>
    incr_indices

let var_name_to_index string ctx =
  let rec find_index current_index ctx =
    match BFT.front ctx with
    | None -> raise UnknownIndex
    | Some (tail, {var_name; _}) ->
      if Stdlib.(=) var_name string
      then current_index
      else find_index (current_index + 1) tail
  in find_index 0 ctx

let index_to_var_name index ctx =
  (BFT.get ctx index).var_name

let get_binding index ctx =
  (BFT.get ctx index).binding

let get_type index ctx =
  match (BFT.get ctx index).var_type with
  | None -> raise UnknownTypeBinding
  | Some expr -> expr

let is_var_name_bound var_name ctx =
  try
    ignore @@ var_name_to_index var_name ctx;
    true
  with _ ->
    false