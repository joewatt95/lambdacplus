(* This module contains general purpose utilities *)

(* This is the same as Haskell's until *)
let rec until pred f init =
  match pred init with
  | true -> init
  | false -> until pred f (f init)
