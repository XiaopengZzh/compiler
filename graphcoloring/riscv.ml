(* This structure defines the abstract syntax for RISC-V assembly
 * language that we will be using.  It should be fairly self-
 * explanatory.
 *
 *)

type label = string

type reg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 
           | R10 | R11 | R12 | R13 | R14 | R15 | R16 | R17 | R18 | R19
           | R20 | R21 | R22 | R23 | R24 | R25 
           | R26 | R27 | R28
           | R29 | R30 | R31

type operand = Reg of reg | Immed of Word32.word

type inst =
  Add of reg * reg * operand  
| Sub of reg * reg * reg      
| Div of reg * reg * reg      
| Mul of reg * reg * reg     
| Or of reg * reg * operand   
| And of reg * reg * operand  
| Not of reg * reg
| Xor of reg * reg * operand
| Sll of reg * reg * operand  
| Sra of reg * reg * operand  
| Srl of reg * reg * operand  
| Li of reg * Word32.word
| Slt of reg * reg * reg
| Sltu of reg * reg * reg
| Seqz of reg * reg
| Snez of reg * reg
| Beq of reg * reg * label
| Blt of reg * reg * label
| Bltu of reg * reg * label
| Bge of reg * reg * label
| Bgeu of reg * reg * label
| Bgez of reg * label
| Bgtz of reg * label
| Blez of reg * label
| Bltz of reg * label
| Bne of reg * reg * label
| J of label
| Jal of reg * label
| Jalr of reg * reg * int32
| La of reg * label
| Lb of reg * reg * Word32.word
| Lbu of reg * reg * Word32.word
| Lh of reg * reg * Word32.word
| Lhu of reg * reg * Word32.word
| Lw of reg * reg * Word32.word
(* The first component of the payload to store instructions is rs1 (the base address to write to)
   and the second is rs2 *)
(* Note that most assemblers write this as sw rs2, imm(rs1) *)
| Sb of reg * reg * Word32.word
| Sh of reg * reg * Word32.word
| Sw of reg * reg * Word32.word
| Label of label

(* Encodings of some helpful pseudo-instructions *)
let jr r = Jalr (R0, r, Int32.zero)
let seq (rd, r1, r2) = [Sub (rd, r1, r2); Seqz (rd, rd)]
let sne (rd, r1, r2) = [Sub (rd, r1, r2); Snez (rd, rd)]

(* The functions below are used to convert RISC-V instructions
 * into strings, suitable for output to an assembly file. *)
let reg2string r =
  match r with
    R0 -> "x0" | R1 -> "x1" | R2 -> "x2" | R3 -> "x3"
  | R4 -> "x4" | R5 -> "x5" | R6 -> "x6" | R7 -> "x7"
  | R8 -> "x8" | R9 -> "x9" | R10 -> "x10" | R11 -> "x11"
  | R12 -> "x12" | R13 -> "x13" | R14 -> "x14" | R15 -> "x15"
  | R16 -> "x16" | R17 -> "x17" | R18 -> "x18" | R19 -> "x19"
  | R20 -> "x20" | R21 -> "x21" | R22 -> "x22" | R23 -> "x23"
  | R24 -> "x24" | R25 -> "x25" | R26 -> "x26" | R27 -> "x27"
  | R28 -> "x28" | R29 -> "x29" | R30 -> "x30" | R31 -> "x31"
    
let string2reg (s:string):reg = 
 match s with
    "x0"   ->  R0| "x1"  ->  R1  | "x2"  -> R2  | "x3"  ->  R3  | "x4" -> R4
  | "x5"   ->  R5| "x6"  ->  R6  | "x7"  -> R7  | "x8"  ->  R8  | "x9" -> R9
  |  "x10" -> R10| "x11" ->  R11 | "x12" -> R12 | "x13" ->  R13
  |  "x14" -> R14| "x15" ->  R15 | "x16" -> R16
  |  "x17" -> R17| "x18" ->  R18 | "x19" -> R19 | "x20" ->  R20
  |  "x21" -> R21| "x22" ->  R22 | "x23" -> R23 | "x24" ->  R24
  |  "x25" -> R25| "x26" ->  R26 | "x27" -> R27 | "x28" ->  R28
  |  "x29" -> R29| "x30" ->  R30 | "x31" -> R31
  | _ -> failwith "failed string2reg"

type fmt = R of reg | L of label | W of Word32.word

let fmt2string(f:fmt):string = 
  match f with
     R r -> reg2string r
  | W w -> (Word32.toString w)
  | L x -> x 

let i2s (i:string) (x:fmt list):string = 
    "\t" ^ i ^ "\t" ^ (String.concat ", " (List.map fmt2string x)) 

let w2s(w:Word32.word):string =
    if Word32.andb (w,0x80000000l) = 0x80000000l then
      let wneg = Word32.neg w in
      "-" ^ (Word32.toString wneg)
    else Word32.toString w

let i2as (i:string) ((r1:reg),(r2:reg),(w:Word32.word)):string = 
    "\t" ^ i ^ "\t" ^ (reg2string r1) ^ ", "^(w2s w)^"(" ^ (reg2string r2) ^ ")"

(* Converts an instruction to a string *)
let inst2string(i:inst):string = 
  match i with
    Add (r1,r2,Reg r3) -> i2s "add" [R r1;R r2;R r3]
  | Add (r1,r2,Immed w) -> i2s "addi" [R r1;R r2;W w]
  | And (r1,r2,Reg r3) -> i2s "and" [R r1;R r2;R r3]
  | And (r1,r2,Immed w) -> i2s "andi" [R r1;R r2;W w]
  | Div (r1,r2,r3) -> i2s "div" [R r1;R r2;R r3] 
  | Mul (r1,r2,r3) -> i2s "mul" [R r1;R r2;R r3]
  | Or (r1,r2,Reg r3) -> i2s "or" [R r1;R r2;R r3] 
  | Or (r1,r2,Immed w) -> i2s "ori" [R r1;R r2;W w]
  | Not (r1,r2) -> i2s "not" [R r1;R r2]
  | Sll (r1,r2,Reg r3) -> i2s "sllv" [R r1;R r2;R r3]
  | Sll (r1,r2,Immed w) -> i2s "sll" [R r1;R r2;W w]
  | Sra (r1,r2,Reg r3) -> i2s "srav" [R r1;R r2;R r3]
  | Sra (r1,r2,Immed w) -> i2s "sra" [R r1;R r2;W w]
  | Srl (r1,r2,Reg r3) -> i2s "srlv" [R r1;R r2;R r3]
  | Srl (r1,r2,Immed w) -> i2s "srl" [R r1;R r2;W w]
  | Sub (r1,r2,r3) -> i2s "sub" [R r1;R r2;R r3]
  | Xor (r1,r2,Reg r3) -> i2s "xor" [R r1;R r2;R r3]
  | Xor (r1,r2,Immed w) -> i2s "xori" [R r1;R r2;W w]
  | Li (r,w) -> i2s "li" [R r;W w]
  | Slt (r1,r2,r3) -> i2s "slt" [R r1;R r2;R r3]
  | Sltu (r1,r2,r3) -> i2s "sltu" [R r1;R r2;R r3]
  | Seqz (r1,r2) -> i2s "seqz" [R r1;R r2]
  | Snez (r1,r2) -> i2s "snez" [R r1;R r2]
  | Beq (r1,r2,x) -> i2s "beq" [R r1;R r2;L x]
  | Blt (r1,r2,x) -> i2s "blt" [R r1;R r2;L x]
  | Bltu (r1,r2,x) -> i2s "bltu" [R r1;R r2;L x]
  | Bge (r1,r2,x) -> i2s "bge" [R r1;R r2;L x]
  | Bgeu (r1,r2,x) -> i2s "bgeu" [R r1;R r2;L x]
  | Bgez (r1,x) -> i2s "bgez" [R r1; L x]
  | Bgtz (r1,x) -> i2s "bgtz" [R r1; L x]
  | Blez (r1,x) -> i2s "blez" [R r1; L x]
  | Bltz (r1,x) -> i2s "bltz" [R r1; L x]
  | Bne (r1,r2,x) -> i2s "bne" [R r1; R r2;L x]
  | J x -> "\tj "^x
  | Jal (r, x) -> "\tjal "^ reg2string r ^ "," ^ x
  | Jalr (r1,r2,imm) -> i2s "jalr" [R r1; R r2; W imm]
  | La (r1,x) -> i2s "la" [R r1;L x]
  | Lb (r1,r2,w) -> i2as "lb" (r1,r2,w)
  | Lbu (r1,r2,w) -> i2as "lbu" (r1,r2,w)
  | Lh (r1,r2,w) -> i2as "lh" (r1,r2,w)
  | Lhu (r1,r2,w) -> i2as "lhu" (r1,r2,w)
  | Lw (r1,r2,w) -> i2as "lw" (r1,r2,w)
  | Sb (r1,r2,w) -> i2as "sb" (r2,r1,w)
  | Sh (r1,r2,w) -> i2as "sh" (r2,r1,w)
  | Sw (r1,r2,w) -> i2as "sw" (r2,r1,w)
  | Label x -> x ^ ":" 
;;
      

let prog2string (p:inst list) : string =
  List.fold_right (fun i s -> ((inst2string i)^"\n"^s)) p ""
