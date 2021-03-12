val parse : Sedlexing.lexbuf -> Ast.list_of_stmts

val parse_string : string -> Ast.list_of_stmts

val parse_channel : in_channel -> Ast.list_of_stmts