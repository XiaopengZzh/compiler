type token =
  | INT of (
# 34 "parse.mly"
        int
# 6 "parse.mli"
)
  | EOF
  | VAR of (
# 36 "parse.mly"
        string
# 12 "parse.mli"
)
  | LBRACE
  | RBRACE
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | EQ
  | NEQ
  | LT
  | LTE
  | GT
  | GTE
  | SEMICOLON
  | ASSIGN
  | LPAREN
  | RPAREN
  | LCOMMENT
  | RCOMMENT
  | NOT
  | AND
  | OR
  | IF
  | ELSE
  | WHILE
  | FOR
  | RETURN

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
