type token =
  | INT of (
# 33 "parse.mly"
        int
# 6 "parse.mli"
)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
