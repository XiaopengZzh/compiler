open Riscv_ast
open Riscv_assem
open Byte

exception TODO
exception FatalError of string

(* load the word that the current program counter points to *)
let load_next_word (st : state) : int32 = 
  let pos = st.pc in
  let bt0 = (b2i32 (mem_lookup pos st.m)) in
  let bt1 = (b2i32 (mem_lookup (Int32.add pos 1l) st.m)) in 
  let bt2 = (b2i32 (mem_lookup (Int32.add pos 2l) st.m)) in 
  let bt3 = (b2i32 (mem_lookup (Int32.add pos 3l) st.m)) in
  (* since we're using little endian, it means the byte in the lower address should be located at the lower position of the word *)
  let bl = [(bt3, 8); (bt2, 8); (bt1, 8); (bt0, 8)] in
  (* First write down a word in a natural manner, e.g 1101 0100(0b11010100) *)
  (* For little endian, we store the byte 0100 in lower address, say 0x40000000 *)
  (* Then the byte 1101 should be stored in 0x40000001 *)
  (* But in terms of what the sequence to store these 8 bits of this byte in the memory, it's totally a black box *)
  (* Only thing we care about is when we give the memory an address, it outputs this 0100 or 1101 or we store it in this address *)
  combine_bits bl;;

(* Given a word, decode it to instruction, e.g. Add (reg1, reg2, reg3) *)
let decode (wd : int32) : inst = 
  let opcode = (Int32.logand wd 0b1111111l) in
  match opcode with
  | 0b0110011l -> 
    let funct3 = (Int32.logand (Int32.shift_right_logical wd 12) 0b111l) in 
    let funct7 = (Int32.logand (Int32.shift_right_logical wd 25) 0b1111111l) in
    if (funct3 <> 0b0l || funct7 <> 0b0000000l) then
      raise (FatalError "wrong funct3 of add")
    else
      let rd = (ind2reg (Int32.logand (Int32.shift_right_logical wd 7) 0b11111l)) in
      let rs1 = (ind2reg (Int32.logand (Int32.shift_right_logical wd 15) 0b11111l)) in
      let rs2 = (ind2reg (Int32.logand (Int32.shift_right_logical wd 20) 0b11111l)) in
      Add (rd, rs1, rs2)
  | 0b0010011l -> 
    let funct3 = (Int32.logand (Int32.shift_right_logical wd 12) 0b111l) in
    let rd = (ind2reg (Int32.logand (Int32.shift_right_logical wd 7) 0b11111l)) in
    let rs1 = (ind2reg (Int32.logand (Int32.shift_right_logical wd 15) 0b11111l)) in 
    let imm = (Int32.logand (Int32.shift_right_logical wd 20) 0b111111111111l) in
    (* according to risc-v manual, we should sign-extend this 12 bit immediate since this imm is a complement *)
    let imm_signed = if (Int32.logand imm 0x800l) <> 0b0l then Int32.logor imm 0xFFFFF000l else imm in
    if funct3 = 0b000l then Addi (rd, rs1, imm_signed)
    else if funct3 = 0b110l then Ori (rd, rs1, imm) else raise (FatalError "wrong funct3 of addi or ori")
  | 0b1100111l -> 
    let funct3 = (Int32.logand (Int32.shift_right_logical wd 12) 0b111l) in
    let rd = (ind2reg (Int32.logand (Int32.shift_right_logical wd 7) 0b11111l)) in
    let rs1 = (ind2reg (Int32.logand (Int32.shift_right_logical wd 15) 0b11111l)) in 
    let imm = (Int32.logand (Int32.shift_right_logical wd 20) 0b111111111111l) in
    (* according to risc-v manual, we should sign-extend this 12 bit immediate since this imm is a complement *)
    let imm_signed = if (Int32.logand imm 0x800l) <> 0b0l then Int32.logor imm 0xFFFFF000l else imm in
    if funct3 = 0b000l then Jalr (rd, rs1, imm_signed)
    else raise (FatalError "wrong funct3 of jalr")
  | 0b0000011l ->
    let funct3 = (Int32.logand (Int32.shift_right_logical wd 12) 0b111l) in
    let rd = (ind2reg (Int32.logand (Int32.shift_right_logical wd 7) 0b11111l)) in
    let rs1 = (ind2reg (Int32.logand (Int32.shift_right_logical wd 15) 0b11111l)) in 
    let imm = (Int32.logand (Int32.shift_right_logical wd 20) 0b111111111111l) in
    (* according to risc-v manual, we should sign-extend this 12 bit immediate since this imm is a complement *)
    let imm_signed = if (Int32.logand imm 0x800l) <> 0b0l then Int32.logor imm 0xFFFFF000l else imm in
    if funct3 = 0b010l then Lw (rd, rs1, imm_signed)
    else
      raise (FatalError "wrong funct3 or lw")
  | 0b1100011l ->
    let funct3 = (Int32.logand (Int32.shift_right_logical wd 12) 0b111l) in
    let rs1 = (ind2reg (Int32.logand (Int32.shift_right_logical wd 15) 0b11111l)) in
    let rs2 = (ind2reg (Int32.logand (Int32.shift_right_logical wd 20) 0b11111l)) in
    let imm_ls = (Int32.logand (Int32.shift_right_logical wd 8) 0b1111l) in
    let imm_ms = (Int32.logand (Int32.shift_right_logical wd 25) 0b111111l) in
    let imm_11 = (Int32.logand (Int32.shift_right_logical wd 7) 0b1l) in
    let imm_12 = (Int32.logand (Int32.shift_right_logical wd 31) 0b1l) in
    (* take a look on how the immediate is constructed for beq, in page 43 of risc-v manual *)
    (* btw, when using combine_bits, make sure number of all bits sum up to 32 *)
    let imm = (combine_bits [(0l, 20); (imm_12, 1); (imm_11, 1); (imm_ms, 6); (imm_ls, 4)]) in
    if funct3 = 0b000l 
      then 
        if (Int32.logand imm 0x800l) <> 0b0l then Beq (rs1, rs2, (Int32.logor imm 0xFFFFF000l))
        else Beq (rs1, rs2, imm)
    else raise (FatalError "wrong funct3 of beq")
  | 0b0100011l -> 
    let funct3 = (Int32.logand (Int32.shift_right_logical wd 12) 0b111l) in
    let rs1 = (ind2reg (Int32.logand (Int32.shift_right_logical wd 15) 0b11111l)) in
    let rs2 = (ind2reg (Int32.logand (Int32.shift_right_logical wd 20) 0b11111l)) in
    let imm_ls = (Int32.logand (Int32.shift_right_logical wd 7) 0b11111l) in
    let imm_ms = (Int32.logand (Int32.shift_right_logical wd 25) 0b1111111l) in
    (* make sure number of all bits sum up to 32 *)
    let imm = (combine_bits [(0l, 20); (imm_ms, 7); (imm_ls, 5)]) in
    (* according to risc-v manual, we should sign-extend this 12 bit immediate since this imm is a complement *)
    let imm_signed = if (Int32.logand imm 0x800l) <> 0b0l then Int32.logor imm 0xFFFFF000l else imm in
    if funct3 = 0b010l then Sw (rs1, rs2, imm_signed)
    else raise (FatalError "wrong funct3 of sw")
  | 0b1101111l ->
    let rd = (ind2reg (Int32.logand (Int32.shift_right_logical wd 7) 0b11111l)) in
    let imm_ls = (Int32.logand (Int32.shift_right_logical wd 21) 0b1111111111l) in
    let imm_ms = (Int32.logand (Int32.shift_right_logical wd 12) 0b11111111l) in
    let imm_20 = (Int32.logand (Int32.shift_right_logical wd 31) 0b1l) in
    let imm_11 = (Int32.logand (Int32.shift_right_logical wd 20) 0b1l) in
    (* take a look on how the immediate is constructed for jar *)
    let imm = (combine_bits [(0l, 12); (imm_20, 1); (imm_ms, 8); (imm_11, 1); (imm_ls, 10)]) in
    let imm_signed = if (Int32.logand imm 0x80000l) <> 0b0l then Int32.logor imm 0xFFF00000l else imm in
    Jal (rd, imm_signed)
  | 0b0110111l ->
    let rd = (ind2reg (Int32.logand (Int32.shift_right_logical wd 7) 0b11111l)) in
    (* by upper immediate, it contains 12 bit upper significant, not 16 *)
    let imm_ms = (Int32.logand (Int32.shift_right_logical wd 12) 0b11111111111111111111l) in
    let imm = (Int32.shift_left imm_ms 12) in
    (* in the imm, all the 20 lower bits are set to 0 *)
    Lui (rd, imm)
  | _ -> 
    raise (FatalError "fail to decode")


let step_add (ist : inst) (st : state) : state = 
  match ist with 
  | Add (rd, rs1, rs2) -> 
    let rf = st.r in
    let num1 = rf_lookup (reg2ind rs1) rf in
    let num2 = rf_lookup (reg2ind rs2) rf in
    let result = Int32.add num1 num2 in
    let newrf = rf_update (reg2ind rd) result rf in
    {r = newrf; pc = Int32.add st.pc 4l; m = st.m}
  | _ -> raise (FatalError "not add")

let step_addi (ist : inst) (st : state) : state = 
  match ist with 
  | Addi (rd, rs1, imm) -> 
    let rf = st.r in
    let num1 = rf_lookup (reg2ind rs1) rf in
    let result = Int32.add num1 imm in
    let newrf = rf_update (reg2ind rd) result rf in
    {r = newrf; pc = Int32.add st.pc 4l; m = st.m}
  | _ -> raise (FatalError "not addi")

let step_jalr (ist : inst) (st : state) : state =
  match ist with
  | Jalr (rd, rs, imm) ->
    let rf = st.r in
    let num = rf_lookup (reg2ind rs) rf in
    (* according to manual, we should set the last digit to 0 after adding imm with value of rs*)
    (* new address = imm + val(rs)*)
    let newpc = Int32.logand 0xFFFFFFFEl (Int32.add num imm) in
    let ret = (Int32.add st.pc 4l) in
    let newrf = rf_update (reg2ind rd) ret rf in
    {r = newrf; pc = newpc; m = st.m}
  | _ -> raise (FatalError "not jalr")

let step_ori (ist : inst) (st : state) : state = 
  match ist with
  | Ori (rd, rs, imm) ->
    let rf = st.r in
    let num = rf_lookup (reg2ind rs) rf in
    let result = Int32.logor num imm in
    let newrf = rf_update (reg2ind rd) result rf in
    {r = newrf; pc = Int32.add st.pc 4l; m = st.m}
  | _ -> raise (FatalError "not ori")

let step_lw (ist : inst) (st : state) : state = 
  match ist with
  | Lw (rd, rs, imm) ->
    let rf = st.r in
    let mem = st.m in
    let num = rf_lookup (reg2ind rs) rf in
    let addr = Int32.add num imm in
    (* again, little endian *)
    let val0 = b2i32 (mem_lookup addr mem) in
    let val1 = b2i32 (mem_lookup (Int32.add addr 4l) mem) in
    let val2 = b2i32 (mem_lookup (Int32.add addr 8l) mem) in
    let val3 = b2i32 (mem_lookup (Int32.add addr 12l) mem) in
    let wd = combine_bits [(val3, 8); (val2, 8); (val1, 8); (val0, 8)] in
    let newrf = rf_update (reg2ind rd) wd rf in
    {r = newrf; pc = Int32.add st.pc 4l; m = st.m}
  | _ -> raise (FatalError "not lw")

let step_beq (ist : inst) (st : state) : state = 
  match ist with 
  | Beq (rs1, rs2, imm) ->
    let rf = st.r in
    let num1 = rf_lookup (reg2ind rs1) rf in
    let num2 = rf_lookup (reg2ind rs2) rf in
    if num1 = num2 then
      (* according to manual, numerical shift left the imm 1 bit *)
      let imm_shifted = Int32.shift_left imm 1 in
      (* note that we should add it to st.pc, not (st.pc + 4l) *)
      let newpc = Int32.add st.pc imm_shifted in
      {r = rf; pc = newpc; m = st.m}
    else
      {r = rf; pc = (Int32.add st.pc 4l); m = st.m}
  | _ -> raise (FatalError "not beq")

let step_sw (ist : inst) (st : state) : state = 
  match ist with
  | Sw (rs1, rs2, imm) ->
    let rf = st.r in
    let wd = rf_lookup (reg2ind rs2) rf in
    let pos = Int32.add imm (rf_lookup (reg2ind rs1) rf) in
    (* again, little endian *)
    let mem_0 = mem_update pos (getByte wd 0) st.m in
    let mem_1 = mem_update (Int32.add pos 1l) (getByte wd 1) mem_0 in
    let mem_2 = mem_update (Int32.add pos 2l) (getByte wd 2) mem_1 in
    let mem = mem_update (Int32.add pos 3l) (getByte wd 3) mem_2 in
    {r = rf; pc = (Int32.add st.pc 4l); m = mem}
  | _ -> raise (FatalError "not sw")

let step_jal (ist : inst) (st : state) : state = 
  match ist with
  | Jal (rd, imm) -> 
    let rf = st.r in
    let newrf = rf_update (reg2ind rd) (Int32.add st.pc 4l) rf in
    (* according to manual, shift imm left 1 bit *)
    let imm_shifted = Int32.shift_left imm 1 in
    (* adding it to st.pc, not (st.pc + 4l) *)
    let newpc = Int32.add st.pc imm_shifted in
    {r = newrf; pc = newpc; m = st.m}
  | _ -> raise (FatalError "not jal")
  
let step_lui (ist : inst) (st : state) : state = 
  match ist with
  | Lui (rd, imm) ->
    let rf = st.r in
    let newrf = rf_update (reg2ind rd) imm rf in
    {r = newrf; pc = (Int32.add st.pc 4l); m = st.m}
  | _ -> raise (FatalError "not lui")



(* Take a look at the definition of the RISC-V AST and machine state in mips_ast.ml *)

(* Given a starting state, simulate the RISC-V machine code to get a final state;
   a final state is reached if the the next instruction pointed to by the PC is
   all 0s.
 *)
let rec interp (init_state : state) : state = 
  let wd = load_next_word init_state in
  match wd with
  (* if 0 encountered, program ends *)
  | 0b0l -> init_state
  | _ -> 
    let ist = decode wd in
    match ist with
    | Add (r1, r2, r3) -> interp (step_add ist init_state)
    | Addi (r1, r2, imm) -> interp (step_addi ist init_state)
    | Beq (r1, r2, imm) -> interp (step_beq ist init_state)
    | Jal (rd, imm) -> interp (step_jal ist init_state)
    | Jalr (rd, rs, imm) -> interp (step_jalr ist init_state)
    | Lui (r, imm) -> interp (step_lui ist init_state)
    | Ori (r1, r2, imm) -> interp (step_ori ist init_state)
    | Lw (r1, r2, imm) -> interp (step_lw ist init_state)
    | Sw (r1, r2, imm) -> interp (step_sw ist init_state)
    | _ -> raise (FatalError "not recognized")


(*
  Here are a few details/assumptions about the assembler and interpreter that the autograder makes:
  * > Little Endian Encoding
  * > Program Data is stored starting at 0x400000
  * > Constants that occur as input to the assembler are passed directly as 32bit immediates in the AST,
      without any shifting or masking. The assembler then takes subsets of these bits when actually encoding
      an instruction into memory. E.g. an addi can be passed an immediate that 15 bits, but when we encode
      that instruction the encoding only uses bits 0 through 11.
*)
