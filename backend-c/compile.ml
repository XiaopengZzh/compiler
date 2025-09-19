(* Compile Cish AST to RISC-V AST *)
open Riscv
open Ast

exception IMPLEMENT_ME
exception VARIABLE_NOTFOUND

type result = { code : Riscv.inst list;
                data : Riscv.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

(*======================================================================*)
(* consider a chunk of stack is of 16 bytes = storages of 4 integer variables *)
let calc_chunk_num (count_vars : int) : int =
    match count_vars with
    | 0 -> 0
    | _ ->
        let quotient = count_vars / 4 in
        let remainder = count_vars mod 4 in
        (match remainder with
        | 0 -> quotient
        | _ -> (quotient + 1))

(* argument register of i -> a'i' -> R1'i' *)
let argument_reg_of (num : int) : reg = 
    match num with
    | 0 -> R10 | 1 -> R11 | 2 -> R12 | 3 -> R13 | 4-> R14
    | 5 -> R15 | 6 -> R16 | 7 -> R17 | _ -> R0


(* ==================================================================== *)
(* Definitions of varmap *)
type varmap = (string, int) Hashtbl.t

let empty_varmap () : varmap = 
    Hashtbl.create 10

(* ==================================================================== *)

(* map variables name to its offset wrt fp *)
let process_var(v : Ast.var) (variables : varmap) (fargs : varmap) : unit = 
    let sz = Hashtbl.length variables in
    match Hashtbl.find_opt fargs v with
    | Some num -> ()
    | None ->
        (match Hashtbl.find_opt variables v with
        | Some num -> ()
        | None -> Hashtbl.add variables v (-(sz + 1) * 4 - 16))

let rec process_exp((re, _) : Ast.exp) (variables : varmap) (fargs : varmap) : unit = 
    match re with
    | Var v -> process_var v variables fargs
    | Binop (e1, binop, e2) -> (process_exp e1 variables fargs; process_exp e2 variables fargs)
    | Not e1 -> process_exp e1 variables fargs
    | And (e1, e2) -> (process_exp e1 variables fargs; process_exp e2 variables fargs)
    | Or (e1, e2) -> (process_exp e1 variables fargs; process_exp e2 variables fargs)
    | Assign (v, e1) -> (process_var v variables fargs; process_exp e1 variables fargs)
    (* call of var (exp list) generally does not make new variable ? So skip it here*)
    | _ -> ()

let rec collect_local_variables ((rsmt, _) : Ast.stmt) (variables : varmap) (fargs : varmap): unit = 
    match rsmt with
    | Exp e -> process_exp e variables fargs
    | Seq (smt1, smt2) -> (collect_local_variables smt1 variables fargs; collect_local_variables smt2 variables fargs)
    | If (e, smt1, smt2) -> (process_exp e variables fargs; collect_local_variables smt1 variables fargs; collect_local_variables smt2 variables fargs)
    | While(e, smt) -> (process_exp e variables fargs; collect_local_variables smt variables fargs)
    | For (e1, e2, e3, smt) -> (process_exp e1 variables fargs; process_exp e2 variables fargs; process_exp e3 variables fargs; collect_local_variables smt variables fargs)
    | Return e -> process_exp e variables fargs
    | Let (v, e, smt) -> (process_var v variables fargs; process_exp e variables fargs; collect_local_variables smt variables fargs)


(* map variables name to its offset wrt fp*)
let process_var_in_fargs (v : Ast.var) (fargs : varmap) (local_vars_chunk_sz : int): unit = 
    let sz = Hashtbl.length fargs in
    match Hashtbl.find_opt fargs v with
    | Some num -> ()
    | None -> 
        (match sz < 8 with
        | true -> Hashtbl.add fargs v (-(sz + 1) * 4 - 16 * (local_vars_chunk_sz + 1))
        | false -> Hashtbl.add fargs v (4 * (sz - 8)))


let rec collect_function_arguments (args : Ast.var list) (fargs : varmap) (local_vars_chunk_sz : int) : unit = 
    match args with
    | arg :: tail -> (process_var_in_fargs arg fargs local_vars_chunk_sz; collect_function_arguments tail fargs local_vars_chunk_sz)
    | [] -> ()

(*================================================================*)

(* generate riscv assembly of saving argument register in stack *)
let rec save_input_registers (prev_chunk_sz : int) (num_inputs : int) : Riscv.inst list = 
    let num_inputs = min num_inputs 8 in
    let offset = (prev_chunk_sz) * 16 + 4 * num_inputs in
    match num_inputs with
    | 0 -> []
    | _ -> 
        (Sw (R8, (argument_reg_of (num_inputs - 1)), (Int32.of_int (-offset))))::(save_input_registers prev_chunk_sz (num_inputs - 1))

let merge_hashtb (ht1 : varmap) (ht2 : varmap) : varmap = 
    let result = Hashtbl.copy ht1 in
    Hashtbl.iter (fun key value -> Hashtbl.replace result key value) ht2;
    result;;

let rec split_n n lst =
    if n <= 0 then ([], lst)
    else match lst with
        | [] -> ([], [])
        | x :: xs -> 
            let left, right = split_n (n - 1) xs in
            (x :: left, right);;
      
(* whenever an expression is fully evaluated, put the result in t0 register*)
let rec exp2riscv ((e, _) : Ast.exp) (vars : varmap) : Riscv.inst list = 
    match e with
    | Int j -> [Li (R5, Word32.fromInt j)]
    | Var x -> (let offset = Hashtbl.find_opt vars x in 
                match offset with
                | Some num -> [Lw (R5, R8, (Int32.of_int num))]
                | None -> raise VARIABLE_NOTFOUND)
    | Binop (e1, b, e2) ->
        ((exp2riscv e1 vars) @ [Add (R2, R2, Immed (Int32.of_int (-4))); Sw (R2, R5, 0l)] @ 
        (exp2riscv e2 vars) @ [Lw (R16, R2, 0l); Add (R2, R2, Immed (Int32.of_int 4))] @
        (match b with
        | Plus -> [Add (R5, R16, Reg R5)]
        | Minus -> [Sub (R5, R16, R5)]
        | Times -> [Mul (R5, R16, R5)]
        | Div -> [Div (R5, R16, R5)]
        | Eq -> [Sub (R5, R5, R16); Seqz (R5, R5)]
        | Neq -> [Sub (R5, R5, R16); Snez (R5, R5)]
        | Lt -> [Slt (R5, R16, R5)]
        | Lte -> [Slt (R5, R5, R16); Xor (R5, R5, Immed 1l)]
        | Gt -> [Slt (R5, R5, R16)]
        | Gte -> [Slt (R5, R16, R5); Xor (R5, R5, Immed 1l)]
        ))
    | Assign (x, e) ->
        (let offset = Hashtbl.find_opt vars x in
        match offset with
        | Some num -> (exp2riscv e vars) @ [Sw (R8, R5, (Int32.of_int num))]
        | None -> raise VARIABLE_NOTFOUND)
    | Not e ->
        (exp2riscv e vars) @ [Seqz (R5, R5)]
    | And (e1, e2) ->
        (exp2riscv e1 vars) @ [Add (R2, R2, Immed (Int32.of_int (-4))); Sw (R2, R5, 0l)] @ 
        (exp2riscv e2 vars) @ [Lw (R16, R2, 0l); Add (R2, R2, Immed (Int32.of_int 4)); And (R5, R5, Reg R16); Snez (R5, R5)]
    | Or (e1, e2) ->
        (exp2riscv e1 vars) @ [Add (R2, R2, Immed (Int32.of_int (-4))); Sw (R2, R5, 0l)] @ 
        (exp2riscv e2 vars) @ [Lw (R16, R2, 0l); Add (R2, R2, Immed (Int32.of_int 4)); Or (R5, R5, Reg R16); Snez (R5, R5)]
    | Call (v, e_list) ->
        (let num_args = List.length e_list in
         let reg_args, stack_args = if num_args > 8 then (split_n 8 e_list) else (e_list, []) in
         let reg_insts_head = 
            List.mapi (fun i e -> 
                (exp2riscv e vars) @ [Add (R2, R2, Immed (Int32.of_int (-4))); Sw (R2, R5, 0l)]) reg_args
            |> List.flatten
         in
         let reg_insts_tail = 
            List.mapi (fun i e ->
                [Lw (argument_reg_of i, R2, Int32.of_int (-4 * (i + 1)))]) reg_args
            |> List.flatten
         in
         let augmented_sz = (List.length stack_args) * 4 in
         let stack_insts = 
            List.mapi (fun i e -> 
                (exp2riscv e vars) @ [Sw (R2, R5, Int32.of_int (i * 4))]) stack_args
            |> List.flatten
        in
        reg_insts_head @ [Add (R2, R2, Immed (Int32.of_int (4 * (List.length reg_args))))] @reg_insts_tail @ [Add (R2, R2, Immed (Int32.of_int (-augmented_sz)))] @ stack_insts @ 
        [Jal (R1, v); Add (R2, R2, Immed (Int32.of_int augmented_sz)) ;Add (R5, R10, Reg R0)])

let rec compile_function_body ((body, _) : Ast.stmt) (vars : varmap) : Riscv.inst list = 
    match body with
    | Exp e -> exp2riscv e vars
    | Seq (s1, s2) -> (compile_function_body s1 vars) @ (compile_function_body s2 vars)
    | If (e, s1, s2) ->
        (let else_l = new_label() in
         let end_l = new_label() in
         (exp2riscv e vars) @ [Beq (R5, R0, else_l)] @ (compile_function_body s1 vars) @
         [J end_l; Label else_l] @ (compile_function_body s2 vars) @ [Label end_l])
    | While (e, s) ->
        (let test_l = new_label() in
         let top_l = new_label() in
         [J test_l; Label top_l] @ (compile_function_body s vars) @ 
         [Label test_l] @ (exp2riscv e vars) @ [Bne (R5, R0, top_l)])
    | For (e1, e2, e3, s) -> 
        (let test_l = new_label() in
         let top_l = new_label() in
         (exp2riscv e1 vars) @ [J test_l; Label top_l] @ (compile_function_body s vars) @ 
         (exp2riscv e3 vars) @ [Label test_l] @ (exp2riscv e2 vars) @ [Bne (R5, R0, top_l)])
    | Return e -> 
        (exp2riscv e vars)
    | Let (v, e, smt) ->
        (let offset = Hashtbl.find_opt vars v in
        match offset with
        | None -> raise VARIABLE_NOTFOUND
        | Some num -> (exp2riscv e vars) @ [Sw (R8, R5, (Int32.of_int num))] @ 
                        compile_function_body smt vars)

(* ra = x1 = R1 *)
(* sp = x2 = R2 *)
(* fp = s0 = x8 = R8 *)
(* a0~a7 = x10~x17 = R10~R17 *)
(* t0 = x5 = R5 *)

  (* sp-> *)(* --------------------- *) (* lower address *)
            (* |   func arguments  | *) (* max 32 bytes *)
            (* --------------------- *)
            (* |  local variables  | *) (* 16n bytes *)
            (* --------------------- *)
            (* |      ra, fp       | *) (* 16 bytes *)
            (* --------------------- *) (* higher address *)
            (* <- s0(fp) *)
let compile_function (fsig : Ast.funcsig) : Riscv.inst list = 
    let tmp_f_args_vmap = (empty_varmap ()) in
    let _ = collect_function_arguments fsig.args tmp_f_args_vmap 0 in
    let local_vars_map = (empty_varmap ()) in
    let _ = collect_local_variables fsig.body local_vars_map tmp_f_args_vmap in
    let f_args_vmap = (empty_varmap ()) in
    let _ = collect_function_arguments fsig.args f_args_vmap (calc_chunk_num (Hashtbl.length local_vars_map))in
    let stack_size = 16 + (min 2 (calc_chunk_num (Hashtbl.length f_args_vmap))) * 16
                    + (calc_chunk_num (Hashtbl.length local_vars_map)) * 16 in
    let prologue = [Label fsig.name; Add (R2, R2, Immed (Int32.of_int (-stack_size)));
        Sw (R2, R1, (Int32.of_int (stack_size - 4)));
        Sw (R2, R8, (Int32.of_int (stack_size - 8)));
        Sw (R2, R9, (Int32.of_int (stack_size - 12)));
        Add (R8, R2, Immed (Int32.of_int stack_size))] @ 
        (save_input_registers (1 + (calc_chunk_num (Hashtbl.length local_vars_map))) (Hashtbl.length f_args_vmap)) in
    let vars = merge_hashtb local_vars_map f_args_vmap in
    let funcbody = compile_function_body fsig.body vars in
    let epilogue = [Add (R10, R5, Reg R0); 
                    Lw (R1, R2, Int32.of_int (stack_size - 4));
                    Lw (R8, R2, Int32.of_int (stack_size - 8));
                    Lw (R9, R2, Int32.of_int (stack_size - 12));
                    Add (R2, R2, Immed (Int32.of_int stack_size));
                    Jalr (R0, R1, 0l)] in
    prologue @ funcbody @ epilogue

let rec compile (p:Ast.program) : result =
    match p with 
    | [] -> {code = []; data = []}
    | (Fn fsig) :: tail -> 
        (let head_rlist = compile_function fsig in
        let tail_result = compile tail in
        {code = head_rlist @ tail_result.code; data = []})


let result2string (res:result) : string = 
    let code = res.code in
    let data = res.data in
    let strs = List.map (fun x -> (Riscv.inst2string x) ^ "\n") code in
    let vaR8decl x = x ^ ":\t.word 0\n" in
    "\t.text\n" ^
    "\t.align\t2\n" ^
    "\t.globl main\n" ^
    (String.concat "" strs) ^
    "\n\n" ^
    "\t.data\n" ^
    "\t.align 0\n"^
    (String.concat "" (List.map vaR8decl data)) ^
    "\n"

