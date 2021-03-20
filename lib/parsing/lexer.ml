(* TODO: Implement error reporting *)

open Containers

module Loc = Common.Location

exception Lexing_err of {
  lexeme : string;
  source_loc : Loc.source_loc;
}

let whitesp = [%sedlex.regexp? ' ' | '\t']
let newline = [%sedlex.regexp? ('\r' | '\n' | "\r\n")]
let name = 
  [%sedlex.regexp? ((alphabetic |  '_'), Star (alphabetic | '_' | '0' .. '9')) 
                   | math]

(* Change this to change the encoding. *)
module Encoding = Sedlexing.Utf8

module G = Grammar

(* Create a hash table mapping strings to tokens out of the above associative
list defining reserved keywords.
*)
let reserved_keywords =
  [(["fun"; "λ"; "lambda"], G.FUN);
   (["Pi"; "Π"; "∏"; "∀"; "forall"], G.PI);
   (["Type"; "Prop"], G.TYPE);
   (["Kind"], G.KIND);
   (["let"], G.LET);
   (["in"], G.IN);
   (["def"], G.DEF);
   (["axiom"; "constant"], G.AXIOM);
   (["check"], G.CHECK);
   (["eval"], G.EVAL)]
  |> Iter.of_list
  |> Iter.flat_map_l
      (fun (strings, token) ->
        List.map (fun str -> (str, token)) strings)
  |> Iter.to_hashtbl;;

(* List.iter
  (fun (lst, token) ->
    List.iter (fun str -> Hashtbl.add reserved_keywords' str token) lst)
  reserved_keywords *)

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
  | "=>" -> G.DOUBLE_ARROW

  | name -> 
    let lexeme = Encoding.lexeme lexbuf in
    Hashtbl.get_or reserved_keywords lexeme ~default:(G.VAR_NAME lexeme)
  | newline

  | "//", Star (Compl ('\r' | '\n')) 

  | Plus whitesp -> tokenize lexbuf

  (* For catching errorneous tokens. *)
  | _ ->
    let lexeme = Encoding.lexeme lexbuf in
    let source_loc = Sedlexing.lexing_positions lexbuf in
    raise @@ Lexing_err {lexeme; source_loc}