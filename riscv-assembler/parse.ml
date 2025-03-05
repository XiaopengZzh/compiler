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
# 18 "parse.ml"
)
  | ID of (
# 22 "parse.mly"
        string
# 23 "parse.ml"
)
  | REG of (
# 23 "parse.mly"
        string
# 28 "parse.ml"
)
  | EOF

open Parsing
let _ = parse_error;;
# 2 "parse.mly"
open Riscv_ast
open Lexing

let rh n = 
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum
let parse_error s =
  let pos = Parsing.symbol_end_pos () in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n")
# 45 "parse.ml"
let yytransl_const = [|
  257 (* ADD *);
  258 (* BEQ *);
  259 (* JAL *);
  260 (* JALR *);
  261 (* LI *);
  262 (* LW *);
  263 (* SW *);
  264 (* LUI *);
  265 (* ORI *);
  266 (* COMMA *);
  267 (* LPAREN *);
  268 (* RPAREN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  269 (* INT *);
  270 (* ID *);
  271 (* REG *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\006\000\006\000\004\000\006\000\004\000\
\004\000\006\000\007\000\007\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\006\000\000\000\008\000\000\000\
\000\000\009\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\000\005\000\007\000\000\000\000\000\010\000\011\000\
\012\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000"

let yysindex = "\008\000\
\255\254\000\000\251\254\252\254\253\254\254\254\000\255\001\255\
\002\255\003\255\004\255\000\000\014\000\255\254\010\255\011\255\
\012\255\013\255\014\255\015\255\016\255\017\255\018\255\000\000\
\000\000\019\255\020\255\023\255\022\255\025\255\026\255\027\255\
\028\255\029\255\021\255\032\255\000\000\033\255\000\000\034\255\
\035\255\000\000\037\255\036\255\039\255\040\255\041\255\042\255\
\045\255\000\000\000\000\000\000\038\255\043\255\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\016\000\000\000"

let yytablesize = 58
let yytable = "\003\000\
\004\000\005\000\006\000\007\000\008\000\009\000\010\000\011\000\
\001\000\015\000\016\000\017\000\018\000\024\000\019\000\020\000\
\021\000\022\000\023\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\002\000\025\000\044\000\000\000\
\000\000\035\000\036\000\037\000\038\000\039\000\040\000\041\000\
\042\000\045\000\046\000\043\000\047\000\048\000\049\000\000\000\
\000\000\056\000\050\000\051\000\052\000\000\000\057\000\053\000\
\054\000\055\000"

let yycheck = "\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\001\000\015\001\015\001\015\001\015\001\000\000\015\001\015\001\
\015\001\015\001\015\001\010\001\010\001\010\001\010\001\010\001\
\010\001\010\001\010\001\010\001\000\000\014\000\010\001\255\255\
\255\255\015\001\015\001\013\001\015\001\013\001\013\001\013\001\
\013\001\010\001\010\001\015\001\011\001\011\001\010\001\255\255\
\255\255\012\001\015\001\013\001\013\001\255\255\012\001\015\001\
\015\001\013\001"

let yynames_const = "\
  ADD\000\
  BEQ\000\
  JAL\000\
  JALR\000\
  LI\000\
  LW\000\
  SW\000\
  LUI\000\
  ORI\000\
  COMMA\000\
  LPAREN\000\
  RPAREN\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  ID\000\
  REG\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Riscv_ast.program) in
    Obj.repr(
# 29 "parse.mly"
              ( _1 )
# 162 "parse.ml"
               : Riscv_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Riscv_ast.inst) in
    Obj.repr(
# 32 "parse.mly"
       ( [_1] )
# 169 "parse.ml"
               : Riscv_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Riscv_ast.inst) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Riscv_ast.program) in
    Obj.repr(
# 33 "parse.mly"
               ( _1::_2 )
# 177 "parse.ml"
               : Riscv_ast.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 36 "parse.mly"
                              ( Add(str2reg _2,str2reg _4,str2reg _6) )
# 186 "parse.ml"
               : Riscv_ast.inst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int32) in
    Obj.repr(
# 37 "parse.mly"
                              ( Beq(str2reg _2,str2reg _4,_6) )
# 195 "parse.ml"
               : Riscv_ast.inst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int32) in
    Obj.repr(
# 38 "parse.mly"
                    ( Jal(str2reg _2, _4) )
# 203 "parse.ml"
               : Riscv_ast.inst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int32) in
    Obj.repr(
# 39 "parse.mly"
                               ( Jalr(str2reg _2, str2reg _4, _6) )
# 212 "parse.ml"
               : Riscv_ast.inst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int32) in
    Obj.repr(
# 40 "parse.mly"
                   ( Li(str2reg _2,_4) )
# 220 "parse.ml"
               : Riscv_ast.inst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int32) in
    Obj.repr(
# 41 "parse.mly"
                    ( Lui(str2reg _2,_4) )
# 228 "parse.ml"
               : Riscv_ast.inst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int32) in
    Obj.repr(
# 42 "parse.mly"
                              ( Ori(str2reg _2,str2reg _4,_6) )
# 237 "parse.ml"
               : Riscv_ast.inst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : int32) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 43 "parse.mly"
                                     ( Lw(str2reg _2,str2reg _6,_4) )
# 246 "parse.ml"
               : Riscv_ast.inst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : int32) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 44 "parse.mly"
                                     ( Sw(str2reg _6,str2reg _2,_4) )
# 255 "parse.ml"
               : Riscv_ast.inst))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Riscv_ast.program)
