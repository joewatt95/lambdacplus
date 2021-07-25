(** This module contains general purpose utilities *)

val until : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a
(** [until pred f x] behaves the same as Haskell's [until].

@see <http://zvon.org/other/haskell/Outputprelude/until_f.html>
*)

val always_true : 'a -> 'b -> bool
(** [always_true x y] always returns true. 

This is useful for comparisons.
*)

val uncurry3 : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd
(** [uncurry3] functions like [uncurry] but for functions of 3 arguments.
*)

val find_first_index : ('a -> bool) -> 'a Iter.t -> int option