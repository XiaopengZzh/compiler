/* Parser for Fish --- TODO */

%{
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
%}

/* Tells us which non-terminal to start the grammar with. */
%start program

/* This specifies the non-terminals of the grammar and specifies the
 * types of the values they build. Don't forget to add any new non-
 * terminals here.
 */
%type <Ast.program> program
%type <Ast.stmt> stmt
%type <Ast.exp> exp

/* The %token directive gives a definition of all of the terminals
 * (i.e., tokens) in the grammar. This will be used to generate the
 * tokens definition used by the lexer. So this is effectively the
 * interface between the lexer and the parser --- the lexer must
 * build values using this datatype constructor to pass to the parser.
 * You will need to augment this with your own tokens...
 */
%token <int> INT 
%token EOF
%token <string> VAR
%token LBRACE RBRACE 
%token PLUS MINUS TIMES DIV EQ NEQ LT LTE GT GTE SEMICOLON ASSIGN
%token LPAREN RPAREN LCOMMENT RCOMMENT
%token NOT AND OR
%token IF ELSE WHILE FOR RETURN

%nonassoc ELSE
%right ASSIGN
%left OR
%right NOT
%left AND
%left EQ NEQ
%left LT LTE GT GTE
%left PLUS MINUS
%left TIMES DIV

/* Here's where the real grammar starts -- you'll need to add 
 * more rules here... Do not remove the 2%'s!! */
%%

program:
  stmts EOF { $1 }

stmts:
  /* empty */               { (Ast.skip, 0) }
  | stmt                      { $1 }
  | stmts stmt                { Seq($1, $2), rhs 1 }

stmt :
  | exp SEMICOLON {(Exp $1, rhs 1)}
  | RETURN exp SEMICOLON { (Return ($2), rhs 1) }
  | LBRACE stmts RBRACE {$2}
  | IF LPAREN exp RPAREN stmt ELSE stmt {(If ($3, $5, $7), rhs 1)}
  | IF LPAREN exp RPAREN stmt {(If($3, $5, (Exp(Int 0,0),rhs 1)),rhs 1)}
  | WHILE LPAREN exp RPAREN stmt {(While($3, $5), rhs 1)}
  | FOR LPAREN exp SEMICOLON exp SEMICOLON exp RPAREN stmt {(For ($3, $5, $7, $9), rhs 1)}

exp : 
  | INT {(Int $1, rhs 1)}
  | VAR {(Var $1, rhs 1)}
  | exp PLUS exp   { Binop ($1, Plus, $3), rhs 1 }
  | exp MINUS exp  { Binop ($1, Minus, $3), rhs 1 }
  | exp TIMES exp  { Binop ($1, Times, $3), rhs 1 }
  | exp DIV exp    { Binop ($1, Div, $3), rhs 1 }
  | exp EQ exp { Binop ($1, Eq, $3), rhs 1 }
  | exp NEQ exp { Binop ($1, Neq, $3), rhs 1 }
  | exp LT exp { Binop ($1, Lt, $3), rhs 1 }
  | exp LTE exp { Binop ($1, Lte, $3), rhs 1 }
  | exp GT exp { Binop ($1, Gt, $3), rhs 1 }
  | exp GTE exp { Binop ($1, Gte, $3), rhs 1 }
  | NOT exp {(Not $2, rhs 1)}
  | exp AND exp {(And ($1, $3), rhs 1)}
  | exp OR exp {(Or ($1, $3), rhs 1)}
  | VAR ASSIGN exp {(Assign ($1, $3), rhs 1)}
  | MINUS exp {(Binop ((Int 0, rhs 1), Minus, $2), rhs 1)}
  | LPAREN exp RPAREN {$2}
