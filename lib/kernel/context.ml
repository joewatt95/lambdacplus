(* Concrete implementation of the Context module.
  Concretely, contexts are finger trees, as implemented in the BFT
  module in the Batteries library.
  Finger trees are purely functional random access lists with logarithmic time
  concatenation and access, and amortized constant time insertion at both ends.
*)

open Containers

(* For convenience *)
module BFT = BatFingerTree

type entry = {
  var_name : string; (* The name of the variable*)
  var_type : Ast.expr option; (* The type of the variable*)
  binding  : Ast.expr option (* The binding of the varaiable *)
} [@@deriving show, fields]

type t = entry BFT.t

let show ctx =
  ctx
  |> BFT.to_list
  |> List.to_string show_entry ~start:"[" ~stop:"]" ~sep:";" 

let empty = BFT.empty

let is_empty = BFT.is_empty

let length = BFT.size

let add_binding var_name ?var_type ?binding =
  Fun.flip BFT.cons {var_name; var_type; binding}
  (* Fun.flip BFT.cons {var_name; var_type; binding} %>
    incr_indices *)

let var_name_to_index ctx string =
  let rec find_index ctx current_index =
    match BFT.front ctx with
    | None -> None
    | Some (tail, {var_name; _}) ->
      if Stdlib.(=) var_name string
      then Some current_index
      else find_index tail @@ current_index + 1 
  in find_index ctx 0

(* Uses accessor_fn to access a property of the entry record at a given
   index of a context. *)
let get_from_index (ctx : t) (accessor_fn : entry -> 'a) (index : int) : 'a = 
  index
  |> BFT.get ctx
  |> accessor_fn

let index_to_var_name ctx = get_from_index ctx var_name 

(* Taken from:
  https://github.com/minad/andromeda.hs/blob/master/Andromeda/Context.hs

  Whenever we cons a new entry onto the context, we need to go through the whole
  context and correct all the de bruijn indices referring to free variables
  in "binding" and "var_type".

  To see why this is needed, consider the following example.

  We start with an empty context and add (T, Type, _) to it. Then we later infer
  the type of x to be T, ie a variable of index 0. Then we add (x, 0, _) to the
  context. Now our context looks like:
        [(x, 0, _); (T, Type, _)]
  But now that we have cons'd the new entry onto the context, the index of the
  type variable T in the new context is no longer 0. It's actually 1. 

  Now if we cons'd another thing onto the context, this index would need
  to be shifted yet again. Also, if the type were given by a lambda abstraction
  or Pi, we would actually have to recursively traverse that part of the AST and
  shift all the indices.

  Instead of looping through every entry and traversing the whole binding and
  type AST, we can actually just shift the indices accordingly when we do a
  lookup. We know how much to correct the indices by given an entry at index k.
  We need to shift by (k + 1) since that is 1 + the number of times we cons'd
  onto the context since then.
*)
let get_and_shift_indices ctx accessor_fn index =
  index
  |> BFT.get ctx
  |> accessor_fn
  |> CCOpt.map @@ Ast.shift @@ index + 1

let get_binding ctx = get_and_shift_indices ctx binding

let get_type ctx index =
  index
  |> get_and_shift_indices ctx var_type 
  |> CCOpt.get_lazy @@ fun () -> raise Not_found

let is_var_name_bound var_name ctx =
    ctx 
    |> var_name_to_index var_name
    |> CCOpt.is_some 