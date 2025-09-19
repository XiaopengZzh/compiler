type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | EQEQ
  | NEQ
  | LTE
  | GTE
  | LT
  | GT
  | EQ
  | BANG
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | WHILE
  | FOR
  | LET
  | COMMA
  | MALLOC
  | INT of (
# 42 "cish_parse.mly"
        int
# 33 "cish_parse.mli"
)
  | ID of (
# 43 "cish_parse.mly"
        string
# 38 "cish_parse.mli"
)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Cish_ast.program
