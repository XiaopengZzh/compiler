type token =
  | IF
  | LPAREN
  | RPAREN
  | LAMBDA
  | EOF
  | LT
  | EQ
  | LET
  | LETREC
  | CONS
  | FST
  | SND
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | INT of (
# 22 "scish_parse.mly"
        int
# 22 "scish_parse.mli"
)
  | ID of (
# 23 "scish_parse.mly"
        string
# 27 "scish_parse.mli"
)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Scish_ast.exp
