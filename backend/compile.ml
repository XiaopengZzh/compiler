(* Compile Fish AST to RISC-V AST *)
open Riscv
open Ast

exception IMPLEMENT_ME

type result = { code : Riscv.inst list;
                data : Riscv.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

(* sets of variables -- Ocaml Set and Set.S *)
module VarSet = Set.Make(struct
                           type t = Ast.var
                           let compare = String.compare
                         end)

(* a table of variables that we need for the code segment *)
let variables : VarSet.t ref = ref (VarSet.empty)

(* generate a fresh temporary variable and store it in the variables set. *)
let rec new_temp() = 
    let t = "T" ^ (string_of_int (new_int())) in
    (* make sure we don't already have a variable with the same name! *)
    if VarSet.mem t (!variables) then new_temp()
    else (variables := VarSet.add t (!variables); t)

(* reset internal state *)
let reset() = (label_counter := 0; variables := VarSet.empty)

let process_var(v : Ast.var) : unit = 
    if not (VarSet.mem v (!variables)) then
        variables := VarSet.add v (!variables)

let rec process_exp (e : Ast.exp) : unit = 
    let r = fst e in
    match r with
    | Var v -> process_var v
    | Binop (e1, binop, e2) -> (process_exp e1; process_exp e2)
    | Not e1 -> process_exp e1
    | And (e1, e2) -> (process_exp e1; process_exp e2)
    | Or (e1, e2) -> (process_exp e1; process_exp e2)
    | Assign (v, e1) -> (process_var v; process_exp e1)
    | _ -> ()


(* find all of the variables in a program and add them to
 * the set variables *)
let rec collect_vars (p : Ast.program) : unit = 
    (*************************************************************)
    let rsmt = fst p in
    match rsmt with
    | Exp e -> process_exp e
    | Seq (smt1, smt2) -> (collect_vars smt1; collect_vars smt2)
    | If (e, smt1, smt2) -> (process_exp e; collect_vars smt1; collect_vars smt2)
    | While (e, smt) -> (process_exp e; collect_vars smt)
    | For (e1, e2, e3, smt) -> (process_exp e1; process_exp e2; process_exp e3; collect_vars smt)
    | Return e -> process_exp e
    (*************************************************************)

let rec exp2riscv((e, _) : Ast.exp) : inst list = 
    match e with
    | Int j -> [Li (R5, Word32.fromInt j)]
    | Var x -> [La (R5, x); Lw(R5, R5, 0l)]
    | Binop (e1, b, e2) -> 
        (let t = new_temp() in
         (exp2riscv e1) @ [La (R7, t); Sw (R7, R5, 0l)] @
         (exp2riscv e2) @ [La (R7, t); Lw (R6, R7, 0l)] @ 
         (match b with
            | Plus -> [Add (R5, R5, Reg R6)]
            | Minus -> [Sub (R5, R6, R5)]
            | Times -> [Mul (R5, R6, R5)]
            | Div -> [Div (R5, R6, R5)]
            | Eq -> [Sub (R5, R5, R6); Seqz (R5, R5)]
            | Neq -> [Sub (R5, R5, R6); Snez (R5, R5)]
            | Lt -> [Slt (R5, R6, R5)]
            | Lte -> [Slt (R5, R5, R6); Xor (R5, R5, Immed 1l)]
            | Gt -> [Slt (R5, R5, R6)]
            | Gte -> [Slt (R5, R6, R5); Xor (R5, R5, Immed 1l)]
            ))
    | Assign (x, e) -> 
        (exp2riscv e) @ [La (R7, x); Sw (R7, R5, 0l)]
    | Not e -> 
        (exp2riscv e) @ [Seqz (R5, R5)]
    | And (e1, e2) -> 
        (let t = new_temp() in
         (exp2riscv e1) @ [La (R7, t); Sw (R7, R5, 0l)] @
         (exp2riscv e2) @ [La (R7, t); Lw (R6, R7, 0l); And (R5, R5, Reg R6); Snez (R5, R5)])
    | Or (e1, e2) -> 
        (let t = new_temp() in
         (exp2riscv e1) @ [La (R7, t); Sw (R7, R5, 0l)] @
         (exp2riscv e2) @ [La (R7, t); Lw (R6, R7, 0l); Or (R5, R5, Reg R6); Snez (R5, R5)])


(* compiles a Fish statement down to a list of RISC-V instructions.
 * Note that a "Return" is accomplished by placing the resulting
 * value in x10 and then doing a jr x1.
 *)
let rec compile_stmt ((s,_):Ast.stmt) : inst list = 
    (*************************************************************)
    match s with
    | Exp e -> exp2riscv e
    | Seq (s1, s2) -> (compile_stmt s1) @ (compile_stmt s2)
    | If (e, s1, s2) -> 
        (let else_l = new_label() in
         let end_l = new_label() in
         (exp2riscv e) @ [Beq (R5, R0, else_l)] @ (compile_stmt s1) @
         [J end_l; Label else_l] @ (compile_stmt s2) @ [Label end_l])
    | While (e, s) ->
        (let test_l = new_label() in
         let top_l = new_label() in
         [J test_l; Label top_l] @ (compile_stmt s) @
         [Label test_l] @ (exp2riscv e) @ 
         [Bne (R5, R0, top_l)])
    | For (e1, e2, e3, s) -> 
        (let test_l = new_label() in
         let top_l = new_label() in
         (exp2riscv e1) @ [J test_l; Label top_l] @
         (compile_stmt s) @ (exp2riscv e3) @ [Label test_l] @
         (exp2riscv e2) @ [Bne (R5, R0, top_l)])
    | Return e -> 
        (exp2riscv e) @ [Add (R10, R5, Reg R0)] @ [Jalr (R0, R1, 0l)]
    (*************************************************************)

(* compiles Fish AST down to RISC-V instructions and a list of global vars *)
let compile (p : Ast.program) : result = 
    let _ = reset() in
    let _ = collect_vars(p) in
    let insts = (Label "main") :: (compile_stmt p) in
    { code = insts; data = VarSet.elements (!variables) }

(* converts the output of the compiler to a big string which can be 
 * dumped into a file, assembled, and run in qemu *)
let result2string ({code;data}:result) : string = 
    let strs = List.map (fun x -> (Riscv.inst2string x) ^ "\n") code in
    let var2decl x = x ^ ":\t.word 0\n" in
    "\t.text\n" ^
    "\t.align\t2\n" ^
    "\t.globl main\n\n" ^
    (String.concat "" strs) ^
    "\n\n" ^
    "\t.data\n" ^
    "\t.align 0\n"^
    (String.concat "" (List.map var2decl data)) ^
    "\n"
