(*========================================================================================*)
(* Register file definitions. A register file is a map from a register
   number to a 32-bit quantity. *)

(*regfile is essentially a Map<int, int32>, map the tag of register to its actual value*)
module IntMap = Map.Make(struct type t = int let compare = compare end)
type regfile = int32 IntMap.t

(*create a empty regfile*)
let empty_rf = IntMap.empty

(*update some register in rf regfile*)
let rf_update (r : int) (v : int32) (rf : regfile) : regfile =
  IntMap.add r v rf

(*loop up the value of a register in rf regfile*)
let rf_lookup (r : int) (rf : regfile) : int32 =
  try IntMap.find r rf with Not_found -> Int32.zero

(*print the current values of all registers in rf regfile*)
let string_of_rf (rf : regfile) : string =
  IntMap.fold (fun key v s ->
    s^(string_of_int key)^" -> "^(Int32.to_string v)^"\n") rf ""
(*========================================================================================*)

open Int32
open Byte

type label = string

type reg = R0
     | R1
     | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9
     | R10 | R11 | R12 | R13 | R14 | R15 | R16 | R17 | R18 | R19
     | R20 | R21 | R22 | R23 | R24 | R25
     | R26 | R27
     | R28
     | R29 | R30 | R31

let reg2str r =
  match r with
    R0 -> "x0" | R1 -> "x1" | R2 -> "x2" | R3 -> "x3"
  | R4 -> "x4" | R5 -> "x5" | R6 -> "x6" | R7 -> "x7"
  | R8 -> "x8" | R9 -> "x9" | R10 -> "x10" | R11 -> "x11"
  | R12 -> "x12" | R13 -> "x13" | R14 -> "x14" | R15 -> "x15"
  | R16 -> "x16" | R17 -> "x17" | R18 -> "x18" | R19 -> "x19"
  | R20 -> "x20" | R21 -> "x21" | R22 -> "x22" | R23 -> "x23"
  | R24 -> "x24" | R25 -> "x25" | R26 -> "x26" | R27 -> "x27"
  | R28 -> "x28" | R29 -> "x29" | R30 -> "x30" | R31 -> "x31"

let str2reg s =
  match s with
    "x0" -> R0 | "x1" -> R1 | "x2" -> R2 | "x3" -> R3 | "x4" -> R4
  | "x5" -> R5 | "x6" -> R6 | "x7" -> R7 | "x8" -> R8 | "x9" -> R9
  | "x10" -> R10 | "x11" -> R11 | "x12" -> R12 | "x13" -> R13
  | "x14" -> R14 | "x15" -> R15 | "x16" -> R16 | "x17" -> R17
  | "x18" -> R18 | "x19" -> R19 | "x20" -> R20 | "x21" -> R21
  | "x22" -> R22 | "x23" -> R23 | "x24" -> R24 | "x25" -> R25
  | "x26" -> R26 | "x27" -> R27 | "x28" -> R28 | "x29" -> R29
  | "x30" -> R30 | "x31" -> R31
  | _ -> R0

let reg2ind r =
  match r with
    R0 -> 0 | R1 -> 1 | R2 -> 2 | R3 -> 3
  | R4 -> 4 | R5 -> 5 | R6 -> 6 | R7 -> 7
  | R8 -> 8 | R9 -> 9 | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15
  | R16 -> 16 | R17 -> 17 | R18 -> 18 | R19 -> 19
  | R20 -> 20 | R21 -> 21 | R22 -> 22 | R23 -> 23
  | R24 -> 24 | R25 -> 25 | R26 -> 26 | R27 -> 27
  | R28 -> 28 | R29 -> 29 | R30 -> 30 | R31 -> 31

(* A small subset of the Mips assembly language *)
(* add : reg1 = reg2 + reg3 ? *)
(* addi : reg1 = reg2 + int32(12 bit immed) *)
type inst =
  Add of reg * reg * reg
| Addi of reg * reg * int32
(* The imm argument to beq is assumed to already be shifted by 1 bit *)
(* branch if equal, pc = pc + imm not pc + 4 + imm *)
| Beq of reg * reg * int32
(* jal, jump and link, first reg = PC + 4, then jump, which is PC += int32*)
| Jal of reg * int32
(* reg1 = PC + 4, jump, PC = reg2 + imm, don't forget to set the lsb to be zero *)
| Jalr of reg * reg * int32
(* load a 32 bit immediate to reg *)
| Li of reg * int32
(* The imm argument to lui is assumed to already be shifted by 12 bits *)
(* lui, load upper immediate,  *)
| Lui of reg * int32
(* do or operation on reg2 and immediate, output to reg1*)
| Ori of reg * reg * int32
(* reg1 = mem[reg2 + int32] *)
| Lw of reg * reg * int32
  (* Here, the first component of the payload to Sw is rs1 and the second is rs2 *)
  (* Note that most assemblers write this as sw rs2, imm(rs1) *)
(* mem[reg1 + int32] = reg2 *)
| Sw of reg * reg * int32

type program = inst list

(*=================================================================================*)
(* Memory definitions. A memory is a map from 32-bit addresses to bytes. *)
module Int32Map = Map.Make(struct type t = int32 let compare = Int32.compare end)
type memory = byte Int32Map.t
let empty_mem = Int32Map.empty
let mem_update (a : int32) (v : byte) (m : memory) : memory =
  Int32Map.add a v m
let mem_lookup (a : int32) (m : memory) : byte =
  try (Int32Map.find a m) with Not_found -> mk_byte Int32.zero
let string_of_mem (m : memory) : string =
  Int32Map.fold (fun key v s ->
    s^(Int32.to_string key)^" -> "^(Int32.to_string (b2i32 v))^"\n") m ""
(*=================================================================================*)


(* State *)
type state = { r : regfile; pc : int32; m : memory }

(* convert a (int * int32) list to regfile*)
let reg_of_list (rl : (int*int32) list) : regfile =
  List.fold_right (fun (r,v) rf -> rf_update r v rf) rl empty_rf
;;
let init_regfile = reg_of_list []
;;
let init_pc = 0x400000l
;;

let inst2str i =
  match i with
    Add (r1,r2,r3) -> "add "^(reg2str r1)^", "^(reg2str r2)^", "^(reg2str r3)^"\n"
  |  Addi (r1,r2,imm) -> "addi "^(reg2str r1)^", "^(reg2str r2)^", "^(Int32.to_string imm)^"\n"
  | Beq (r1,r2,imm) -> "beq "^(reg2str r1)^", "^(reg2str r2)^", "^(Int32.to_string imm)^"\n"
  | Jal (rd, imm) -> "jal "^(reg2str rd)^", "^(Int32.to_string imm)^"\n"
  | Jalr (rd, rs, imm) -> "jalr "^(reg2str rd)^", "^(reg2str rs)^", "^(Int32.to_string imm)^"\n"
  | Li (r,imm) -> "li "^(reg2str r)^", "^(Int32.to_string imm)^"\n"
  | Lui (r,imm) -> "lui "^(reg2str r)^", "^(Int32.to_string imm)^"\n"
  | Ori (r1,r2,imm) -> "ori "^(reg2str r1)^", "^(reg2str r2)^", "^(Int32.to_string imm)^"\n"
  | Lw (r1,r2,imm) -> "lw "^(reg2str r1)^", "^(Int32.to_string imm)^"("^(reg2str r2)^")\n"
  | Sw (r1,r2,imm) -> "sw "^(reg2str r1)^", "^(Int32.to_string imm)^"("^(reg2str r2)^")\n"

let rec prog2str p =
  match p with
    [] -> "\n"
  | hd::tl -> (inst2str hd)^(prog2str tl)
