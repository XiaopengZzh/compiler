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
  | INT of (
# 41 "parse.mly"
        int
# 32 "parse.mli"
)
  | ID of (
# 42 "parse.mly"
        string
# 37 "parse.mli"
)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
