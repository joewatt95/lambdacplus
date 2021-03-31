open Containers
open Cs4215_dependent_types

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

let global_nctx = ref Kernel.Context.empty
let global_ectx = ref Kernel.Context.empty

let js_run_repl str =
  match String.(compare str "quit") with
  | 0 -> "bye"
  | _ ->
  match String.(compare str "reset") with
  | 0 ->
    global_nctx := Kernel.Context.empty;
    global_ectx := Kernel.Context.empty;
    "reset success"
  | _ ->
    try
      let stmts, naming_ctx =
        str
        |> Parsing.Parser.parse_string
        |> Fun.flip Ast_conv.parser_to_internal_stmts
          !global_nctx
      in
      try
        let stmts' , ctx' =
          stmts
          |> Fun.flip Kernel.Eval_statements.eval_stmts !global_ectx
        in
        let res = stmts'
        |> Pretty_print.unparse_internal_expr naming_ctx in
        global_nctx := naming_ctx;
        global_ectx := ctx';
        res
      with exc ->
        Error_reporting.fmt_eval_err_str naming_ctx exc
    with exc ->
      Error_reporting.fmt_parse_err_str exc

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
  (* For debugging *)
  Printexc.record_backtrace true;

  print_endline "Enter input:";
  let stmts, naming_ctx = 
    try
      (* prog
      |> Parsing.Parser.parse_string *)
      stdin
      |> Parsing.Parser.parse_channel
      |> Fun.flip Ast_conv.parser_to_internal_stmts 
         Kernel.Context.empty 
    with exc ->
      print_endline @@ Error_reporting.fmt_parse_err_str exc;
      exit 1
  in
  (* print_endline @@ Common.Ast.show_list_of_stmts Format.pp_print_int stmts; *)
  try
    stmts
    |> Fun.flip Kernel.Eval_statements.eval_stmts naming_ctx
    |> fst
    (* |> fun expr ->
      print_endline @@ Common.Ast.show_expr Format.pp_print_int expr; expr; *)
    |> Pretty_print.unparse_internal_expr naming_ctx
    (* |> Parsing.Ast.show_expr *)
    |> fun str -> print_endline @@ "\nHere's the output:\n" ^ str;
    flush stdout
  with exc ->
    print_endline @@ Error_reporting.fmt_eval_err_str naming_ctx exc;
    exit 2