{
open Parse
open Lexing
}

let cr='\013'
let nl='\010'
let eol=(cr nl|nl|cr)
let ws=('\012'|'\t'|' ')*
let digit=['0'-'9']
let hex=['0'-'9''A'-'F''a'-'f']

let id = ['A'-'Z''a'-'z''_''.']['.''a'-'z''A'-'Z''0'-'9''_']*

rule lexer = parse
| eol { lexer lexbuf }
| ws+ { lexer lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| ',' { COMMA }
| "add" { ADD }
| "beq" { BEQ }
| "jal" { JAL }
| "jalr" { JALR }
| "li" { LI }
| "lui" { LUI }
| "ori" { ORI }
| "lw" { LW }
| "sw" { SW }
| "x"digit+ { REG(Lexing.lexeme lexbuf) }
| id { ID(Lexing.lexeme lexbuf) }
| "0x"hex+ { INT(Int32.of_string(Lexing.lexeme lexbuf)) }
| digit+ { INT(Int32.of_string(Lexing.lexeme lexbuf)) }
| '-'digit+ { INT(Int32.of_string(Lexing.lexeme lexbuf)) }
| eof { EOF }
