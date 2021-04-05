(* This record types indicates that an error has occured at the given position.
This is passed to the JS frontend for error reporting. *)

type err_loc = {
    start_row : int; end_row : int;
    start_col : int; end_col : int;
}

(* Given an exception that was thrown while parsing the user's input,
   format it nicely into readable output. *)
val fmt_parse_err_str : exn -> string * err_loc

(* Given a naming context and an exception that was thrown during evaluation,
   format the error nicely.
   Note that the naming context is used to unparse the relevant expressions
   from the internal de bruijn AST to a string with variable names. *)
val fmt_eval_err_str : string -> exn -> string * err_loc