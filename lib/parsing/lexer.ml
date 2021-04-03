open Containers

(* Change this to change the encoding. *)
module Encoding = Sedlexing.Utf8

exception Syntax_error of {
  lexeme : string;
  source_loc : Common.Location.source_loc;
}

let raise_syntax_err lexbuf =
  let lexeme = Encoding.lexeme lexbuf in
  let source_loc = Sedlexing.lexing_positions lexbuf in
  raise @@ Syntax_error {lexeme; source_loc}

let whitesp = [%sedlex.regexp? ' ' | '\t']
let newline = [%sedlex.regexp? ('\r' | '\n' | "\r\n")]
let name =
  [%sedlex.regexp? ((alphabetic |  '_'), Star (alphabetic | '_' | '0' .. '9')) 
                   | math]

(* Create a hash table mapping strings to tokens out of the above associative
  list defining reserved keywords.

  Unicode math characters:
  https://unicode.org/charts/PDF/U2200.pdf
*)
let reserved_keywords =
  let open Grammar in
  [(["fun"; "λ"; "lambda"], FUN);
   (["Pi"; "Π"; "∏"; "∀"; "forall"], PI);
   (["Sigma"; "Σ"], SIGMA);
   (["∃"; "exists"], EXISTS);
   (["fst"], FST);
   (["snd"], SND);
   (["match"], MATCH);
   (["inl"], INL);
   (["inr"], INR);
   (["with"], WITH);
   (["end"], END);
   (["Type"; "Prop"], TYPE);
   (["Kind"], KIND);
   (["let"], LET);
   (["in"], IN);
   (* Statements *)
   (["def"], DEF);
   (* These are typed version of def statements, used to state and prove 
    theorems. *)
   (["theorem"; "lemma"], THEOREM);
   (["axiom"; "constant"], AXIOM);
   (["check"], CHECK);
   (["eval"], EVAL);
   (* For proof terms like Lean. *)
   (["assume"], ASSUME);
   (["have"], HAVE);
   (["from"], FROM);
   (["show"], SHOW)]
   (* Fancy stream fusion stuff *)
  |> Iter.of_list
  |> Iter.flat_map_l
      (fun (strings, token) ->
        List.map (fun str -> (str, token)) strings)
  |> Iter.to_hashtbl;;

let rec tokenize lexbuf =
  let open Grammar in
  match%sedlex lexbuf with
  | eof -> EOF
  (* Support single-line comments. Idea taken from
  https://github.com/vshaxe/hxparser/blob/master/src/syntax/lexing/lexer.ml *)
  | '(' -> LPAREN
  | ')' -> RPAREN
  | "{" -> LEFT_CURLY
  | "}" -> RIGHT_CURLY
  | ',' -> COMMA
  | ':' -> COLON
  | ":=" -> COLON_EQ
  | "=>" | "⇒" -> DOUBLE_ARROW
  | "->" | "→" -> ARROW
  | "*" | "⨯" | "∧" | "/\\" -> PROD
  | "+" | "∨" | "\\/" -> PLUS
  | "|" -> BAR

  | name ->
    let lexeme = Encoding.lexeme lexbuf in
    Hashtbl.get_or reserved_keywords lexeme ~default:(VAR_NAME lexeme)
  | newline

  | "--", Star (Compl ('\r' | '\n')) 

  | Plus whitesp -> tokenize lexbuf

  (* For catching errorneous tokens. *)
  | Compl ('\r' | '\n') -> raise_syntax_err lexbuf

  (* This case should not happen since all problematic tokens would have been
     caught by the above case. *)
  | _ -> assert false