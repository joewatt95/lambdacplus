open Containers
open Cs4215_dependent_types
open Js_of_ocaml

(* let prog = {|
// Assume that A and B are types.
constant A : Type
constant B : Type

// Assume that R is a binary relation on A x B.
// Technically speaking, R is a predicate symbol and these are
// represented by type constructors in the Curry Howard interpretation of
// logic.
constant R : A -> B -> Prop

// Further assume that every (a : A) is related to some (b : B).
axiom R_left_total : ∀ a : A, ∃ b : B, R a b

// We define a choice function using the explicit witness provided by the
// constructive existential quantifier.
def f := λ (a : A) =>
  let exists_b_Rab := R_left_total a in
  fst exists_b_Rab

// pf is a proof that given an arbitrary (a : A), a is really related to (f a). 
def pf := λ (a : A) => snd (R_left_total a)

// Use f and pf to witness the existential below.
check ((f, pf) : ∃ f : A -> B, ∀ a : A, R a (f a))
|} *)

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
      Error (Error_reporting.fmt_eval_err_str naming_ctx exc)
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
      with exc ->
        print_endline @@ Error_reporting.fmt_eval_err_str naming_ctx exc;
        internal_run_repl nctx ectx
    with exc ->
      print_endline @@Error_reporting.fmt_parse_err_str exc;
      internal_run_repl nctx ectx

let internal_run_once () =


    print_endline @@ Error_reporting.fmt_eval_err_str naming_ctx exc;
    exit 2
  
let () =
  Js.export_all
  (object%js
    val ctx0 = (Kernel.Context.empty, Kernel.Context.empty)
    method run_in_ctx str ctx = 
      str
      |> Js.to_string
      |> Fun.flip run_in_context ctx
      |> (fun x -> match x with
      | Error str -> (false, ctx, str)
      | Ok (ctx', str) -> (true, ctx', str))
      |> (fun (flag, fctx, res) ->
        (object%js
          val ok = Js.bool flag
          val cmd = str
          val result = Js.string res
          val ctx = fctx
        end))
  end)
