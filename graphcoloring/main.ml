(* This magic is used to glue the generated lexer and parser together.
 * Expect one command-line argument, a file to parse.
 * You do not need to understand this interaction with the system. *)
let parse_file() =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv != 3
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse] [output file]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Cish_parse.program Cish_lex.lexer (Lexing.from_channel ch)

let parse_stdin() = 
  Cish_parse.program Cish_lex.lexer (Lexing.from_channel stdin)

let _ =
  let prog = parse_file() in
  let riscvcode = Cish_compile.compile prog in
  let ch = open_out Sys.argv.(2) in
  let strs = List.map (fun x -> (Riscv.inst2string x) ^ "\n") riscvcode in
  let res = 
    "\t.text\n" ^
    "\t.align\t2\n" ^
    "\t.globl main\n" ^
    (String.concat "" strs) ^
    "\n\n" ^
    "\t.data\n" ^
    "\t.align 0\n"^
    "\n" in
  print_newline ();
  print_string res;
  output_string ch res
