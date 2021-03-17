open Containers

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
  | "//", Star (Compl ('\r' | '\n')) -> Sedlexing.new_line lexbuf; tokenize lexbuf 
  | '(' -> G.LPAREN
  | ')' -> G.RPAREN
  | ',' -> G.COMMA
  | ':' -> G.COLON
  | ":=" -> G.COLON_EQ
  | "=>"  | "⇒" -> G.DOUBLE_ARROW
  | newline -> Sedlexing.new_line lexbuf; tokenize lexbuf
  | name -> 
    let str = Encoding.lexeme lexbuf in
    Hashtbl.get_or reserved_keywords str ~default:(G.VAR_NAME str)
  | Plus whitesp -> tokenize lexbuf
  | _ -> assert false