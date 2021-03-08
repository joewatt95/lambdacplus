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
end

module context : CONTEXT =
struct
  type entry = {
    var_name : string;
    var_type : Ast.expr;
    binding  : Ast.expr option
  } [@@deriving make, show]

  exception UnknownVar
  exception UnknownIndex

  type t = entry BatVect.t
  let empty = BatVect.empty
  let is_empty = BatVect.is_empty
  let length = BatVect.length
  let add_binding = make_entry %> BactVect.prepend

  let get_entry index ?(exc = BatVect.Out_of_bounds) ctx =
    try
      BatVect.get ctx index
    with BatVect.Out_of_bounds ->
      raise exc

  let var_name_to_index string ctx =
    try
      BatVect.findi (fun {var_name; _} -> var_name = string) ctx
    with Not_found -> raise UnknownIndex

  let index_to_var_name index ctx =
    let {var_name; _} = get_entry index ?exc:UnknownVar ctx in
    var_name

  let get_binding index ctx =
    let {binding; _} = get_entry index ctx in
    binding

  let get_type =
    let {var_type; _} = get_entry index ctx in
    var_type
end
