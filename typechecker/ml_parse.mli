type token =
  | TRUE
  | FALSE
  | HD
  | TL
  | FST
  | SND
  | ISNIL
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LPAREN
  | RPAREN
  | IF
  | THEN
  | ELSE
  | EQUALS
  | LT
  | COMMA
  | SEMI
  | TILDE
  | NIL
  | LBRACKET
  | RBRACKET
  | CONS
  | DARROW
  | LET
  | IN
  | END
  | FN
  | VAL
  | EOF
  | INT of (
# 33 "ml_parse.mly"
        int
# 38 "ml_parse.mli"
)
  | ID of (
# 34 "ml_parse.mly"
        string
# 43 "ml_parse.mli"
)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Mlish_ast.exp
