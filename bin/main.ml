open Containers
open Cs4215_dependent_types
open Js_of_ocaml

let run_in_context str (nctx, ectx) =
  try
    let stmts, naming_ctx =
      str
      |> Parsing.Parser.parse_string
      |> Fun.flip Ast_conv.parser_to_internal_stmts nctx
    in
    try
      let stmts' , ctx' =
        Fun.flip Kernel.Eval_statements.eval_stmts ectx stmts
      in
      Ok ((naming_ctx, ctx'),
      Pretty_print.unparse_internal_expr naming_ctx stmts')
    with exc ->
      Error (Error_reporting.fmt_eval_err_str str exc)
  with exc ->
    Error (Error_reporting.fmt_parse_err_str exc)

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
        print_endline @@ fst @@ Error_reporting.fmt_eval_err_str str exn;
        internal_run_repl nctx ectx
    with exn ->
      print_endline @@ fst @@ Error_reporting.fmt_parse_err_str exn;
      internal_run_repl nctx ectx

(* let internal_run_once () =


    print_endline @@ Error_reporting.fmt_eval_err_str naming_ctx exc;
    exit 2 *)

(* let () = internal_run_repl Kernel.Context.empty Kernel.Context.empty *)
  
let () = Js.export_all @@
  object%js
    val ctx0 = Kernel.Context.(empty, empty)
    method run_in_ctx str ctx = 
      str
      |> Js.to_string
      |> Fun.flip run_in_context ctx
      |> begin function
         | Error (str, err_loc) -> (false, ctx, str, Some err_loc)
         | Ok (ctx, str) -> (true, ctx, str, None)
         end
      |> begin fun (flag, fctx, res, err_loc) ->
          object%js
          val ok = Js.bool flag
          val cmd = str
          val result = Js.string res
          val ctx = fctx
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