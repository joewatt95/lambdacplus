(* Concrete implementation of the Context module.
  We use the purely functional random access list implementation as found
  in the Containers library. *)

open Containers
open Common

type entry = {
  var_name : string;               (* The name of the variable*)
  var_type : int Ast.expr option;  (* The type of the variable*)
  binding  : int Ast.expr option   (* The binding of the varaiable *)
} [@@deriving show, fields]
(* Derive pretty printers and accessors for the fields of this record type. *)

type t = entry CCRAL.t

let pp = CCRAL.pp pp_entry @@ Format.formatter_of_out_channel stdout

let empty = CCRAL.empty

let is_empty = CCRAL.is_empty

let length = CCRAL.length

let add_binding var_name ?var_type ?binding =
  CCRAL.cons {var_name; var_type; binding}

let add_name_bindings =
  List.fold_left @@ fun ctx var_name -> add_binding var_name ctx

(* Find the first index in the context corresponding to the variable name
given by `str` *)
let var_name_to_index ctx str =
  ctx
  |> CCRAL.to_iter
  |> Utils.find_first_index @@ Fun.(var_name %> Stdlib.(=) str)

(* Uses accessor_fn to access a property of the entry record at a given
   index of a context. *)
let get_from_index (ctx : t) (accessor_fn : entry -> 'a) (index : int) : 'a = 
  index
  |> CCRAL.get_exn ctx
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
  |> CCRAL.get_exn ctx
  |> accessor_fn
  |> Option.map @@ Ast.shift @@ index + 1

let get_binding ctx = get_and_shift_indices ctx binding

let get_type ctx index =
  index
  |> get_and_shift_indices ctx var_type
  |> Option.get_lazy @@ fun _ -> raise Not_found

let is_var_name_bound var_name ctx =
  ctx
  |> var_name_to_index var_name
  |> Option.is_some