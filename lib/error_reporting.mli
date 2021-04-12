(** This module contains facilities for error reporting.
The 2 functions
- {! format_parse_error}
- {! format_typing_error}

They are used by {!val:Toplevel.parse_and_val} to format errors into strings. 
Both of these functions return an {! err_loc} record, which is passed to the 
web-based front end to be reported to the user.
*)

type err_loc = {
    start_row : int; end_row : int;
    start_col : int; end_col : int;
}
(** A record containing the location where an error occured.
*)

val format_parse_error : exn -> string * err_loc
(** [format_parse_error exn str]

Here [exn] is an exception that is encountered during 
- the parsing phase
- the conversion from the parser's concrete AST to the de bruijn one

This function returns [(str, err_loc)]
where
- [str] is a string description of the error that occured
- {! err_loc} is a record type containing the location of the error.
*)

val format_typing_error : string -> exn -> string * err_loc
(** [format_typing_error source_str exn]

Here [exn] is an exception that was thrown while typechecking the input.

- source_str is a string representing the original source entered by the user.
  This is used to grab the erroneous part of the input.
- exn is the exception encountered

This function returns [(str, err_loc)]
where
- [str] is a string description of the error that occured
- {! err_loc} is a record type containing the location of the error.
*)