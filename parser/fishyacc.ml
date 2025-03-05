open Ast
open Eval
open Parse
open Lexing
(* This magic is used to glue the generated lexer and parser together.
 * Expect one command-line argument, a file to parse.
 * You do not need to understand this interaction with the system. *)

 let string_of_token = function
 | INT n           -> Printf.sprintf "INT(%d)" n
 | VAR id         -> Printf.sprintf "IDENT(%s)" id
 | EOF             -> "EOF"
 | LBRACE         -> "LBRACES"
 | RBRACE         -> "RBRACES"
 | LPAREN          -> "LPAREN"
 | RPAREN          -> "RPAREN"
 | ASSIGN              -> "ASSIGN"
 | AND             -> "AND"
 | OR              -> "OR"
 | EQ              -> "EQ"
 | NEQ             -> "NEQ"
 | LT              -> "LT"
 | LTE             -> "LTE"
 | GT              -> "GT"
 | GTE             -> "GTE"
 | PLUS            -> "PLUS"
 | MINUS           -> "MINUS"
 | TIMES        -> "MULTIPLY"
 | DIV          -> "DIVIDE"
 | RETURN             -> "Return"
 | SEMICOLON       -> ";"
 | _ -> "error"
;;


let rec parse_tokens lexbuf = 
  let token = Lex.lexer lexbuf in
  match token with
  | EOF -> print_endline "EOF"
  | _ -> 
    print_endline (string_of_token token);
    parse_tokens lexbuf



let parse_file() =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  (*let lexbuf = Lexing.from_channel ch in
  let _ =   parse_tokens lexbuf in*)
  Parse.program Lex.lexer (Lexing.from_channel ch)

let parse_stdin() =
  Parse.program Lex.lexer (Lexing.from_channel stdin)

(* Expect 1 command line argument, the file to parse 
 * usage: ps2yacc [file-to-parse] *)
let _ =
  let prog = parse_file() in
  let ans = eval prog in
  print_string ("answer = "^(string_of_int ans)^"\n")
