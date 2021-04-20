open Containers
open Lambdacplus
open Js_of_ocaml

(** [internal_run_repl naming_ctx eval_ctx] runs a simple REPL in the terminal
for testing purposes. 

Note that it only accepts single-line strings, not multi-line ones.
*)
let rec internal_run_repl nctx ectx =
  print_string "> ";
  flush stdout;
  let str = read_line () in
    match String.(compare str "quit") with
    | 0 -> print_endline "bye"
    | _ ->
    match String.(compare str "reset") with
    | 0 -> internal_run_repl Kernel.Context.empty Kernel.Context.empty
    | _ ->
    try
      let stmts, naming_ctx = 
        str
        |> Parsing.Parser.parse_string
        |> Fun.flip Ast_conv.parser_to_internal_stmts
          nctx
      in
      try
        let stmts', ctx' = 
        stmts
        |> Fun.flip Kernel.Eval_statements.eval_stmts ectx
        in
        stmts'
        |> Pretty_print.unparse_internal_expr naming_ctx
        |> fun str -> print_endline @@ "\n" ^ str ^ "\n";
        flush stdout;
        internal_run_repl naming_ctx ctx'
      with exn ->
        print_endline @@ fst @@ Error_reporting.format_typing_error str exn;
        internal_run_repl nctx ectx
    with exn ->
      print_endline @@ fst @@ Error_reporting.format_parse_error exn;
      internal_run_repl nctx ectx

(* let () = internal_run_repl Kernel.Context.empty Kernel.Context.empty *)

(** This object is the main entry point used by the web-based frontend.
It uses js_of_ocaml to create a javascript object that contains
- a property [ctx0] which is a tuple of the initial (empty) naming and evaluation
  contexts
- a method [run_in_ctx str (naming_ctx, eval_ctx)] which wraps the above
  {! run_in_ctx} function.
*)
let () = Js.export_all @@
  object%js
  (* Initial naming and evaluation contexts. *)
  val initialCtxs = Toplevel.initial_contexts

  method parseAndEval str ctxs = 
    str
    |> Js.to_string
    |> Fun.flip Toplevel.parse_and_eval ctxs
    |> begin function
        | Error (str, err_loc) -> (false, ctxs, str, Some err_loc)
        | Ok (ctxs, str) -> (true, ctxs, str, None)
        end
    |> begin fun (flag, ctxs, res, err_loc) ->
        object%js
        val ok = Js.bool flag
        val cmd = str
        val result = Js.string res
        val ctxs = ctxs

        (* Create a javascript object corresponding to the err_loc record. *)
        val errLoc =
          match err_loc with
          | Some {start_row; end_row; start_col; end_col} -> 
            Js.def @@ object%js
              val errStartRow = start_row 
              val errEndRow = end_row 
              val errStartCol = start_col
              val errEndCol = end_col 
              end
          | None -> Js.undefined
          end
        end
  end