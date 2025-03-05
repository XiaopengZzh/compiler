(* Lexer for Fish --- TODO *)

(* You need to add new definition to build the
 * appropriate terminals to feed to parse.mly.
 *)

{
open Parse
open Lexing

let incr_lineno lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }
}

(* definition section *)
let cr='\013'
let nl='\010'
(* end of line *)
let eol=(cr nl|nl|cr)
(* whitespace *)
let ws=('\012'|'\t'|' ')*
let digit=['0'-'9']*
let letter=['a'-'z' 'A'-'Z']
let var=letter (digit | letter | '_')*

(* rules section *)
rule lexer = parse
| "/*" { skip_comment lexbuf; lexer lexbuf }
| eol { incr_lineno lexbuf; lexer lexbuf } 
| ws+ { lexer lexbuf }
| digit+ { INT(int_of_string(Lexing.lexeme lexbuf)) } 
| '{' {LBRACE}
| '}' {RBRACE}
| '+' {PLUS}
| '-' {MINUS}
| '*' {TIMES}
| '/' {DIV}
| "==" {EQ}
| "!=" {NEQ}
| "<" {LT}
| "<=" {LTE}
| ">" {GT}
| ">=" {GTE}
| ';' {SEMICOLON}
| '(' {LPAREN}
| ')' {RPAREN}
| '=' {ASSIGN}
| '!' {NOT}
| "&&" {AND}
| "||" {OR}
| "if" {IF}
| "else" {ELSE}
| "while" {WHILE}
| "for" {FOR}
| "return" {RETURN}
| var {VAR(Lexing.lexeme lexbuf)}
| eof {EOF}

and skip_comment = parse
| "*/" { () }
| eof { () }
| _ { skip_comment lexbuf }
