(* This module contains general purpose utilities *)

open Containers

(* This is the same as Haskell's until *)
let rec until pred f init =
  match pred init with
  | true -> init
  | false -> until pred f (f init)

let try_either f =
  let open Either in
  try
    Right (f ())
  with exc ->
    Left exc
