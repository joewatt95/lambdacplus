(*
BatVect is a module provided by the Batteries library. It's a persistent array-like
data structure with:
- O(1) length
- amortized O(1) prepend/append
- O(log N) set/get
*)

open Containers

type entry = {
  var_name : string;
  var_type : Ast.expr option;
  binding  : Ast.expr option
} [@@deriving show]

exception UnknownIndex
exception UnknownTypeBinding

type t = entry BatFingerTree.t

let empty = BatFingerTree.empty

let is_empty = BatFingerTree.is_empty

let length = BatFingerTree.size

let add_binding ~var_name ?var_type ?binding ctx =
  BatFingerTree.cons ctx {var_name; var_type; binding}

let var_name_to_index string ctx =
  let rec findi current_index ctx =
    match BatFingerTree.front ctx with
    | None -> raise UnknownIndex
    | Some (tail, {var_name; _}) ->
      if Stdlib.(=) var_name string
      then current_index
      else findi (current_index + 1) tail
  in findi 0 ctx

let index_to_var_name index ctx =
  let {var_name; _} = BatFingerTree.get ctx index in
  var_name

let get_binding index ctx =
  let {binding; _} = BatFingerTree.get ctx index in
  binding

let get_type index ctx =
  let {var_type; _} = BatFingerTree.get ctx index in
  match var_type with
  | None -> raise UnknownTypeBinding
  | Some expr -> expr

let is_var_name_bound var_name ctx =
  try
    let _ = var_name_to_index var_name ctx in
    true
  with _ ->
    false

let show =
  let open Fun in
  BatFingerTree.to_list %>
  List.to_string show_entry ~start:"[" ~stop:"]" ~sep:"; "

(* module Context : CONTEXT =
 * struct
 *   type entry = {
 *     var_name : string;
 *     var_type : Ast.expr;
 *     binding  : Ast.expr option
 *   }
 * 
 *   exception UnknownVar
 *   exception UnknownIndex
 * 
 *   type t = entry BatVect.t
 *   let empty = BatVect.empty
 *   let is_empty = BatVect.is_empty
 *   let length = BatVect.length
 * 
 *   let add_binding ~var_name ~var_type ?binding =
 *     BatVect.prepend {var_name; var_type; binding}
                        * 
                        *   let get_entry index ?(exc = BatVect.Out_of_bounds) ctx =
                        *     try
                        *       BatVect.get ctx index
                        *     with BatVect.Out_of_bounds ->
                        *       raise exc
                        * 
                        *   let var_name_to_index string ctx =
                        *     let open Stdlib in
                        *     try
                        *       BatVect.findi (fun {var_name; _} -> var_name = string) ctx
                        *     with Not_found -> raise UnknownIndex
                        * 
                        *   let index_to_var_name index ctx =
                        *     let {var_name; _} = get_entry index ~exc:UnknownVar ctx in
                        *     var_name
                        * 
                        *   let get_binding index ctx =
                        *     let {binding; _} = get_entry index ctx in
                        *     binding
                        * 
                        *   let get_type index ctx =
                        *     let {var_type; _} = get_entry index ctx in
                        *     var_type
                        * end *)
