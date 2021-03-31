(* TODO: Implement error reporting *)

open Containers

(* Change this to change the encoding. *)
module Encoding = Sedlexing.Utf8

module G = Grammar

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
  [(["fun"; "λ"; "lambda"], G.FUN);
   (["Pi"; "Π"; "∏"; "∀"; "forall"], G.PI);
   (["Sigma"; "Σ"; "∃"; "exists"], G.SIGMA);
   (["fst"], G.FST);
   (["snd"], G.SND);
   (["match"], G.MATCH);
   (["inl"], G.INL);
   (["inr"], G.INR);
   (["with"], G.WITH);
   (["end"], G.END);
   (["Type"; "Prop"], G.TYPE);
   (["Kind"], G.KIND);
   (["let"], G.LET);
   (["in"], G.IN);
   (* Statements *)
   (["def"], G.DEF);
   (* These are typed version of def statements, used to state and prove 
    theorems. *)
   (["theorem"; "lemma"], G.THEOREM);
   (["axiom"; "constant"], G.AXIOM);
   (["check"], G.CHECK);
   (["eval"], G.EVAL);
   (* For proof terms like Lean. *)
   (["assume"], G.ASSUME);
   (["have"], G.HAVE);
   (["from"], G.FROM);
   (["show"], G.SHOW)]
   (* Fancy stream fusion stuff *)
  |> Iter.of_list
  |> Iter.flat_map_l
      (fun (strings, token) ->
        List.map (fun str -> (str, token)) strings)
  |> Iter.to_hashtbl;;

let rec tokenize lexbuf =
  match%sedlex lexbuf with
  | eof -> G.EOF
  (* Support single-line comments. Idea taken from
  https://github.com/vshaxe/hxparser/blob/master/src/syntax/lexing/lexer.ml *)
  | '(' -> G.LPAREN
  | ')' -> G.RPAREN
  | ',' -> G.COMMA
  | ':' -> G.COLON
  | ":=" -> G.COLON_EQ
  | "=>" | "⇒" -> G.DOUBLE_ARROW
  | "->" | "→" -> G.ARROW
  | "*" | "⨯" | "∧" | "/\\" -> G.PROD
  | "+" | "∨" | "\\/" -> G.PLUS
  | "|" -> G.BAR

  | name ->
    let lexeme = Encoding.lexeme lexbuf in
    Hashtbl.get_or reserved_keywords lexeme ~default:(G.VAR_NAME lexeme)
  | newline

  | "--", Star (Compl ('\r' | '\n')) 

  | Plus whitesp -> tokenize lexbuf

  (* For catching errorneous tokens. *)
  | Compl ('\r' | '\n') -> raise_syntax_err lexbuf

  (* This case should not happen since all problematic tokens would have been
     caught by the above case. *)
  | _ -> assert false