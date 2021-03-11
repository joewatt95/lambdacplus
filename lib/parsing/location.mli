type 'a located = {
  data: 'a;
  source_loc : Lexing.position * Lexing.position [@opaque];
}

val locate : 'a -> Lexing.position * Lexing.position -> 'a located 

val update_data : 'a located -> ('a -> 'b) -> 'b located

val set_data : 'a located -> 'b -> 'b located

val pp_located : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a located -> unit