(** This module contains general purpose utilities *)

(** This is the same as Haskell's until *)
let rec until pred f init =
  match pred init with
  | true -> init
  | false -> until pred f @@ f init

(** Useful for comparisons. *)
let always_true _ _ = true

(** Like Haskell's uncurry3 *)
let uncurry3 f (x, y, z) = f x y z