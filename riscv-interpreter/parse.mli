type token =
  | ADD
  | BEQ
  | JAL
  | JALR
  | LI
  | LW
  | SW
  | LUI
  | ORI
  | COMMA
  | LPAREN
  | RPAREN
  | INT of (
# 21 "parse.mly"
        int32
# 18 "parse.mli"
)
  | ID of (
# 22 "parse.mly"
        string
# 23 "parse.mli"
)
  | REG of (
# 23 "parse.mly"
        string
# 28 "parse.mli"
)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Riscv_ast.program
