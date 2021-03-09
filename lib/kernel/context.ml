(*
BatVect is a module provided by the Batteries library. It's a persistent array-like
data structure with:
- O(1) length
- amortized O(1) prepend/append
- O(log N) set/get
*)

open Containers

module type CONTEXT =
sig
  type t
  val empty : t
  val is_empty : t -> bool
  val length : t -> int
  val add_binding : var_name:string -> var_type:Ast.expr -> ?binding:Ast.expr -> t -> t
  val var_name_to_index : string -> t -> int
  val index_to_var_name : int -> t -> string
  val get_binding : int -> t -> Ast.expr option
  val get_type : int -> t -> Ast.expr
  val is_var_name_bound : string -> t -> bool
end

module Context : CONTEXT =
struct
  type entry = {
    var_name : string;
    var_type : Ast.expr;
    binding  : Ast.expr option
  }

  exception UnknownIndex

  type t = entry BatFingerTree.t
  let empty = BatFingerTree.empty
  let is_empty = BatFingerTree.is_empty
  let length = BatFingerTree.size

  let add_binding ~var_name ~var_type ?binding ctx =
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
    var_type

  let is_var_name_bound var_name ctx =
    try
      let _ = var_name_to_index var_name ctx in
      true
    with _ ->
      false
end

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
