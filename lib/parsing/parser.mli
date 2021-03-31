val parse : Sedlexing.lexbuf -> string Common.Ast.list_of_stmts

val parse_string : string -> string Common.Ast.list_of_stmts

val parse_channel : in_channel -> string * string Common.Ast.list_of_stmts