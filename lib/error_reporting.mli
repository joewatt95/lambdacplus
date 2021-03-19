(* Given an exception that was thrown while parsing the user's input,
   format it nicely into readable output. *)
val fmt_parse_err_str : exn -> string

(* Given a naming context and an exception that was thrown during evaluation,
   format the error nicely.
   Note that the naming context is used to unparse the relevant expressions
   from the internal de bruijn AST to a string with variable names. *)
val fmt_eval_err_str : Kernel.Context.t -> exn -> string