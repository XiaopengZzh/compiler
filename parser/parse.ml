type token =
  | INT of (
# 34 "parse.mly"
        int
# 6 "parse.ml"
)
  | EOF
  | VAR of (
# 36 "parse.mly"
        string
# 12 "parse.ml"
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

open Parsing
let _ = parse_error;;
# 4 "parse.mly"
open Ast
open Lexing
(* use this to get the line number for the n'th token *)
let rhs n =
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum
let parse_error s =
    let pos = Parsing.symbol_end_pos () in
    let l = pos.pos_lnum in
    print_string ("line "^(string_of_int l)^": "^s^"\n")
# 54 "parse.ml"
let yytransl_const = [|
    0 (* EOF *);
  259 (* LBRACE *);
  260 (* RBRACE *);
  261 (* PLUS *);
  262 (* MINUS *);
  263 (* TIMES *);
  264 (* DIV *);
  265 (* EQ *);
  266 (* NEQ *);
  267 (* LT *);
  268 (* LTE *);
  269 (* GT *);
  270 (* GTE *);
  271 (* SEMICOLON *);
  272 (* ASSIGN *);
  273 (* LPAREN *);
  274 (* RPAREN *);
  275 (* LCOMMENT *);
  276 (* RCOMMENT *);
  277 (* NOT *);
  278 (* AND *);
  279 (* OR *);
  280 (* IF *);
  281 (* ELSE *);
  282 (* WHILE *);
  283 (* FOR *);
  284 (* RETURN *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\004\000\004\000\004\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\000\000\001\000\002\000\002\000\003\000\003\000\007\000\
\005\000\005\000\009\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\003\000\003\000\003\000\002\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\012\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\030\000\003\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\005\000\000\000\000\000\001\000\004\000\
\000\000\007\000\029\000\000\000\000\000\000\000\006\000\000\000\
\000\000\016\000\017\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
\000\000\000\000\000\000\008\000\000\000\000\000\011\000"

let yydgoto = "\002\000\
\013\000\014\000\015\000\016\000"

let yysindex = "\255\255\
\068\255\000\000\000\000\246\254\068\255\015\255\015\255\015\255\
\249\254\251\254\254\254\015\255\000\000\000\000\111\255\009\000\
\015\255\040\255\065\255\130\255\116\000\015\255\015\255\015\255\
\149\255\015\255\015\255\015\255\015\255\015\255\015\255\015\255\
\015\255\015\255\015\255\000\000\015\255\015\255\000\000\000\000\
\052\000\000\000\000\000\168\255\187\255\206\255\000\000\065\255\
\065\255\000\000\000\000\186\000\186\000\122\255\122\255\122\255\
\122\255\070\255\116\000\068\255\068\255\015\255\010\255\000\000\
\225\255\068\255\015\255\000\000\033\000\068\255\000\000"

let yyrindex = "\000\000\
\037\000\000\000\000\000\092\255\034\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\067\000\000\000\227\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\252\254\000\000\000\000\000\000\000\000\000\000\000\000\082\000\
\097\000\000\000\000\000\030\255\234\255\122\000\137\000\152\000\
\167\000\247\255\187\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\245\255\252\255\042\000"

let yytablesize = 466
let yytable = "\001\000\
\009\000\019\000\020\000\021\000\040\000\017\000\040\000\025\000\
\039\000\022\000\027\000\023\000\041\000\027\000\024\000\003\000\
\004\000\044\000\045\000\046\000\006\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\007\000\
\058\000\059\000\066\000\008\000\002\000\002\000\018\000\018\000\
\003\000\004\000\005\000\042\000\018\000\006\000\018\000\018\000\
\063\000\064\000\000\000\018\000\018\000\000\000\068\000\000\000\
\007\000\065\000\071\000\000\000\008\000\000\000\069\000\009\000\
\000\000\010\000\011\000\012\000\003\000\004\000\005\000\028\000\
\029\000\006\000\026\000\027\000\028\000\029\000\030\000\031\000\
\032\000\033\000\034\000\035\000\007\000\000\000\000\000\000\000\
\008\000\000\000\000\000\009\000\000\000\010\000\011\000\012\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\000\000\000\000\013\000\000\000\000\000\
\000\000\013\000\013\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\026\000\027\000\
\028\000\029\000\000\000\000\000\037\000\038\000\026\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\000\000\000\000\000\000\043\000\000\000\000\000\000\000\037\000\
\038\000\026\000\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\034\000\035\000\047\000\000\000\000\000\000\000\000\000\
\000\000\000\000\037\000\038\000\026\000\027\000\028\000\029\000\
\030\000\031\000\032\000\033\000\034\000\035\000\000\000\000\000\
\000\000\060\000\000\000\000\000\000\000\037\000\038\000\026\000\
\027\000\028\000\029\000\030\000\031\000\032\000\033\000\034\000\
\035\000\000\000\000\000\000\000\061\000\000\000\000\000\000\000\
\037\000\038\000\026\000\027\000\028\000\029\000\030\000\031\000\
\032\000\033\000\034\000\035\000\062\000\000\000\000\000\000\000\
\000\000\000\000\000\000\037\000\038\000\026\000\027\000\028\000\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\067\000\
\000\000\024\000\019\000\019\000\024\000\000\000\037\000\038\000\
\019\000\024\000\000\000\019\000\000\000\000\000\000\000\019\000\
\019\000\009\000\009\000\009\000\009\000\025\000\009\000\000\000\
\025\000\003\000\004\000\005\000\025\000\025\000\006\000\000\000\
\000\000\009\000\000\000\000\000\000\000\009\000\000\000\000\000\
\009\000\007\000\009\000\009\000\009\000\008\000\000\000\000\000\
\009\000\000\000\010\000\011\000\012\000\026\000\027\000\028\000\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\000\000\
\000\000\000\000\070\000\000\000\000\000\000\000\037\000\038\000\
\026\000\027\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000\000\000\000\000\000\000\000\000\000\000\028\000\
\028\000\037\000\038\000\028\000\028\000\028\000\028\000\028\000\
\028\000\028\000\000\000\000\000\028\000\000\000\014\000\014\000\
\028\000\028\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\000\000\000\000\014\000\000\000\015\000\015\000\014\000\
\014\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\000\000\000\000\015\000\000\000\000\000\000\000\015\000\015\000\
\026\000\027\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\037\000\000\000\020\000\000\000\000\000\000\000\020\000\
\020\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
\000\000\000\000\021\000\000\000\000\000\000\000\021\000\021\000\
\022\000\022\000\022\000\022\000\022\000\022\000\022\000\000\000\
\000\000\022\000\000\000\000\000\000\000\022\000\022\000\023\000\
\023\000\023\000\023\000\023\000\023\000\023\000\000\000\000\000\
\023\000\000\000\000\000\000\000\023\000\023\000\026\000\027\000\
\028\000\029\000\000\000\000\000\032\000\033\000\034\000\035\000\
\000\000\026\000\000\000\000\000\026\000\000\000\000\000\000\000\
\000\000\026\000"

let yycheck = "\001\000\
\000\000\006\000\007\000\008\000\016\000\016\001\018\000\012\000\
\000\000\017\001\015\001\017\001\017\000\018\001\017\001\001\001\
\002\001\022\000\023\000\024\000\006\001\026\000\027\000\028\000\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\017\001\
\037\000\038\000\025\001\021\001\000\000\004\001\009\001\010\001\
\001\001\002\001\003\001\004\001\015\001\006\001\005\000\018\001\
\060\000\061\000\255\255\022\001\023\001\255\255\066\000\255\255\
\017\001\062\000\070\000\255\255\021\001\255\255\067\000\024\001\
\255\255\026\001\027\001\028\001\001\001\002\001\003\001\007\001\
\008\001\006\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\017\001\255\255\255\255\255\255\
\021\001\255\255\255\255\024\001\255\255\026\001\027\001\028\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\255\255\255\255\018\001\255\255\255\255\
\255\255\022\001\023\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\005\001\006\001\
\007\001\008\001\255\255\255\255\022\001\023\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\255\255\255\255\255\255\018\001\255\255\255\255\255\255\022\001\
\023\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\255\255\255\255\255\255\255\255\
\255\255\255\255\022\001\023\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\255\255\255\255\
\255\255\018\001\255\255\255\255\255\255\022\001\023\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\255\255\255\255\255\255\018\001\255\255\255\255\255\255\
\022\001\023\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\255\255\255\255\255\255\
\255\255\255\255\255\255\022\001\023\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\255\255\015\001\009\001\010\001\018\001\255\255\022\001\023\001\
\015\001\023\001\255\255\018\001\255\255\255\255\255\255\022\001\
\023\001\001\001\002\001\003\001\004\001\015\001\006\001\255\255\
\018\001\001\001\002\001\003\001\022\001\023\001\006\001\255\255\
\255\255\017\001\255\255\255\255\255\255\021\001\255\255\255\255\
\024\001\017\001\026\001\027\001\028\001\021\001\255\255\255\255\
\024\001\255\255\026\001\027\001\028\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\255\255\
\255\255\255\255\018\001\255\255\255\255\255\255\022\001\023\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\255\255\255\255\255\255\255\255\255\255\005\001\
\006\001\022\001\023\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\255\255\255\255\018\001\255\255\005\001\006\001\
\022\001\023\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\255\255\255\255\018\001\255\255\005\001\006\001\022\001\
\023\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\018\001\255\255\255\255\255\255\022\001\023\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\022\001\255\255\018\001\255\255\255\255\255\255\022\001\
\023\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\018\001\255\255\255\255\255\255\022\001\023\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\255\255\
\255\255\018\001\255\255\255\255\255\255\022\001\023\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\255\255\255\255\
\018\001\255\255\255\255\255\255\022\001\023\001\005\001\006\001\
\007\001\008\001\255\255\255\255\011\001\012\001\013\001\014\001\
\255\255\015\001\255\255\255\255\018\001\255\255\255\255\255\255\
\255\255\023\001"

let yynames_const = "\
  EOF\000\
  LBRACE\000\
  RBRACE\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LTE\000\
  GT\000\
  GTE\000\
  SEMICOLON\000\
  ASSIGN\000\
  LPAREN\000\
  RPAREN\000\
  LCOMMENT\000\
  RCOMMENT\000\
  NOT\000\
  AND\000\
  OR\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  RETURN\000\
  "

let yynames_block = "\
  INT\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 58 "parse.mly"
            ( _1 )
# 306 "parse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parse.mly"
                            ( (Ast.skip, 0) )
# 312 "parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 62 "parse.mly"
                              ( _1 )
# 319 "parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 63 "parse.mly"
                              ( Seq(_1, _2), rhs 1 )
# 327 "parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 66 "parse.mly"
                  ((Exp _1, rhs 1))
# 334 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 67 "parse.mly"
                         ( (Return (_2), rhs 1) )
# 341 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 68 "parse.mly"
                        (_2)
# 348 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 69 "parse.mly"
                                        ((If (_3, _5, _7), rhs 1))
# 357 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 70 "parse.mly"
                              ((If(_3, _5, (Exp(Int 0,0),rhs 1)),rhs 1))
# 365 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 71 "parse.mly"
                                 ((While(_3, _5), rhs 1))
# 373 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : Ast.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : Ast.exp) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 72 "parse.mly"
                                                           ((For (_3, _5, _7, _9), rhs 1))
# 383 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 75 "parse.mly"
        ((Int _1, rhs 1))
# 390 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 76 "parse.mly"
        ((Var _1, rhs 1))
# 397 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 77 "parse.mly"
                   ( Binop (_1, Plus, _3), rhs 1 )
# 405 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 78 "parse.mly"
                   ( Binop (_1, Minus, _3), rhs 1 )
# 413 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 79 "parse.mly"
                   ( Binop (_1, Times, _3), rhs 1 )
# 421 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 80 "parse.mly"
                   ( Binop (_1, Div, _3), rhs 1 )
# 429 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 81 "parse.mly"
               ( Binop (_1, Eq, _3), rhs 1 )
# 437 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 82 "parse.mly"
                ( Binop (_1, Neq, _3), rhs 1 )
# 445 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 83 "parse.mly"
               ( Binop (_1, Lt, _3), rhs 1 )
# 453 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 84 "parse.mly"
                ( Binop (_1, Lte, _3), rhs 1 )
# 461 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 85 "parse.mly"
               ( Binop (_1, Gt, _3), rhs 1 )
# 469 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 86 "parse.mly"
                ( Binop (_1, Gte, _3), rhs 1 )
# 477 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 87 "parse.mly"
            ((Not _2, rhs 1))
# 484 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 88 "parse.mly"
                ((And (_1, _3), rhs 1))
# 492 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 89 "parse.mly"
               ((Or (_1, _3), rhs 1))
# 500 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 90 "parse.mly"
                   ((Assign (_1, _3), rhs 1))
# 508 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 91 "parse.mly"
              ((Binop ((Int 0, rhs 1), Minus, _2), rhs 1))
# 515 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 92 "parse.mly"
                      (_2)
# 522 "parse.ml"
               : Ast.exp))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
