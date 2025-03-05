open Riscv_ast
open Byte

exception TODO
exception FatalError
exception PError of string

(* instformat is an intermediate format between ast instructions and bytes *)
type rformat_args =  {r_opcode : int32; r_rs1: int32; r_rs2: int32; r_rd: int32; r_funct3: int32; r_funct7: int32}
type iformat_args = {i_opcode : int32; i_rd: int32; i_rs1: int32; i_funct3 : int32; i_imm: int32}
type sformat_args = {s_opcode : int32; s_imm: int32; s_rs1: int32; s_rs2 : int32; s_funct3 : int32}
type bformat_args = {b_opcode : int32; b_imm: int32; b_rs1: int32; b_rs2 : int32; b_funct3 : int32}
type uformat_args =  {u_opcode : int32; u_rd : int32; u_imm: int32}
type jformat_args =  {j_opcode : int32; j_rd : int32; j_imm: int32}
type instformat =
  | R of rformat_args | I of iformat_args | S of sformat_args | U of uformat_args
  | B of bformat_args | J of jformat_args
;;

(* generate a int32 number with n ones from little end *)
(* 00001111111 with n ones *)
let rec ones n : int32 =
  if (n=0) then Int32.zero else
  Int32.add
    (Int32.shift_left Int32.one (n-1))
    (ones (n-1))
;;

(* rewrite all pseudo instructions *)
(* remove all pseudo instructions like li *)
let rec rem_pseudo (prog : program) : program = List.fold_right
  (fun (i:inst) (p:program) ->
    (match i with
     | Li (rd, imm) ->
       (* m ends up being the sign-extended version of the bottom 12 bits of imm *)
      let m = Int32.shift_right (Int32.shift_left imm 20) 20 in
      let k = Int32.sub imm m in
      (Lui (rd,k) :: (Addi (rd,rd,m)::p)
      )
    | x -> x::p
    )
  )
  prog []
;;

let reg2ind32 r = Int32.of_int (reg2ind r)
;;

(* AST instruction -> instformat record *)
let ins2instformat (i: inst) : instformat =
  match i with
  | Add (rd, rs1, rs2) ->
      R {r_opcode=0b0110011l; r_rs1=reg2ind32 rs1;r_rs2=reg2ind32 rs2;r_rd=reg2ind32 rd;
         r_funct3=0b0l; r_funct7=0b0000000l}
  | Addi (rd, rs1, imm) ->
      I {i_opcode=0b0010011l; i_rd=reg2ind32 rd;i_rs1=reg2ind32 rs1;i_imm=imm;
         i_funct3=0b000l}
  | Beq (rs1, rs2, offset) ->
      B {b_opcode=0b1100011l;b_rs1=reg2ind32 rs1;b_rs2=reg2ind32 rs2;b_imm=offset;
         b_funct3=0b000l}
  | Jal (rd, imm) ->
      J {j_opcode=0b1101111l;j_rd=reg2ind32 rd;j_imm=imm}
  | Jalr (rd, rs1, imm) ->
      I {i_opcode=0b1100111l;i_rd=reg2ind32 rd;i_rs1=reg2ind32 rs1;i_imm=imm;
         i_funct3=0b000l;}
  | Li (rd, imm) -> raise (PError "Li encountered")
  | Lui (rd, imm) ->
      U {u_opcode=0b0110111l;u_rd=reg2ind32 rd;u_imm=imm}
  | Ori (rd, rs1, imm) ->
      I {i_opcode=0b0010011l;i_rd=reg2ind32 rd;i_rs1=reg2ind32 rs1;i_imm=imm;
         i_funct3=0b110l;}
  | Lw (rd, rs1, imm) ->
      I {i_opcode=0b0000011l;i_rd=reg2ind32 rd;i_rs1=reg2ind32 rs1;i_imm=imm;
         i_funct3=0b010l;}
  | Sw (rs1, rs2, imm) ->
      S {s_opcode=0b0100011l;s_rs1=reg2ind32 rs1;s_rs2=reg2ind32 rs2;s_imm=imm;
         s_funct3=0b010l;}
;;

let ind2reg (i:int32) : reg =
  str2reg ("x"^(Int32.to_string i))
;;
let instformat2ins (f: instformat) : inst =
  match f with
  | R rfor -> let rd = ind2reg rfor.r_rd in
              let rs1 = ind2reg rfor.r_rs1 in
              let rs2 = ind2reg rfor.r_rs2 in
    (match rfor.r_opcode, rfor.r_funct3, rfor.r_funct7 with
     | 0b0110011l, 0b000l, 0b0000000l -> (Add (rd, rs1, rs2))
     | _ -> raise FatalError)
  | I ifor -> let rd = ind2reg ifor.i_rd in
              let rs1 = ind2reg ifor.i_rs1 in
              let imm = ifor.i_imm in
    (match ifor.i_opcode, ifor.i_funct3 with
    | 0b0010011l, 0b000l -> Addi (rd, rs1, imm)
    | 0b1100111l, 0b000l -> Jalr (rd, rs1, imm)
    | 0b0010011l, 0b110l -> Ori (rd, rs1, imm)
    | 0b0000011l, 0b010l -> Lw (rd, rs1, imm)
    | _ -> raise FatalError
  )
  | B bfor -> let rs1 = ind2reg bfor.b_rs1 in
              let rs2 = ind2reg bfor.b_rs2 in
              let imm = bfor.b_imm in
    (match bfor.b_opcode, bfor.b_funct3 with
     | 0b1100011l, 0b000l -> Beq (rs1, rs2, imm)
     | _ -> raise FatalError)
  | J jfor -> let rd = ind2reg jfor.j_rd in
              let imm = jfor.j_imm in
    (match jfor.j_opcode with
     | 0b1101111l -> Jal (rd, imm)
     | _ -> raise FatalError)
  | U ufor -> let rd = ind2reg ufor.u_rd in
              let imm = ufor.u_imm in
    (match ufor.u_opcode with
     | 0b0110111l -> Lui (rd, imm)
     | _ -> raise FatalError)
  | S sfor -> let rs1 = ind2reg sfor.s_rs1 in
              let rs2 = ind2reg sfor.s_rs2 in
              let imm = sfor.s_imm in
    (match sfor.s_opcode, sfor.s_funct3 with
     | 0b0100011l, 0b010l -> Sw (rs1, rs2, imm)
     | _ -> raise FatalError)
;;

(*Fuse together a list of ints into a word by squeezing them into bit segments*)
(*We need to mask all numbers to have the right width before squeezing*)
(*let bl = [(0b101l, 3); (0b1101l, 4); (0b11110000l, 8); (0b1000000000000000l, 17)]*)
(* 101, 3 *)
(* 1101, 4 *)
(* 11110000, 8 *)
(* 1000000000000000, 17 *)
(* result : 1011101111100001000000000000000 *)
let combine_bits (bl: (int32*int) list) : int32 (*bits, bit width*) =
  assert (List.fold_left (+) 0 (List.map snd bl) = 32);
  let rec combine_bits_offset (bl': (int32*int) list) (offset:int) : int32 =
    match bl' with
    | (b1,l)::tl -> (Int32.add
      (Int32.shift_left (Int32.logand (ones l) b1) (offset-l))
      (combine_bits_offset tl (offset-l))
    )
    | [] -> 0l
  in
  combine_bits_offset bl 32
;;


(* Returns an int32 which contains the bits from the interval [lo : hi] from the input i *)
let bitrange (i : int32) (lo: int) (hi : int) : int32 =
  assert (lo <= hi);
  Int32.logand (ones ((hi + 1) - lo)) (Int32.shift_right i lo)

let instformat2word (f: instformat) : int32 =
  match f with
  | R rfor ->
    combine_bits[(rfor.r_funct7,7);(rfor.r_rs2,5);(rfor.r_rs1,5);
                 (rfor.r_funct3,3);(rfor.r_rd,5);(rfor.r_opcode,7)]
  | I ifor ->
    combine_bits[(ifor.i_imm,12);(ifor.i_rs1,5);
                 (ifor.i_funct3,3);(ifor.i_rd,5);(ifor.i_opcode,7)]
  | S sfor ->
    combine_bits[(bitrange sfor.s_imm 5 11,7);(sfor.s_rs2,5);(sfor.s_rs1,5);
                 (sfor.s_funct3,3);(bitrange sfor.s_imm 0 4,5);(sfor.s_opcode,7)]
  | B bfor ->
    combine_bits[(bitrange bfor.b_imm 12 12, 1);(bitrange bfor.b_imm 5 10,6);(bfor.b_rs2,5);(bfor.b_rs1,5);
                 (bfor.b_funct3,3);(bitrange bfor.b_imm 1 4,4);(bitrange bfor.b_imm 11 11, 1);(bfor.b_opcode,7)]
  | U ufor ->
    combine_bits[(bitrange ufor.u_imm 12 31,20);(ufor.u_rd,5);(ufor.u_opcode,7)]
  | J jfor ->
    combine_bits[(bitrange jfor.j_imm 20 20, 1);
                 (bitrange jfor.j_imm 1 10, 10);
                 (bitrange jfor.j_imm 11 11, 1);
                 (bitrange jfor.j_imm 12 19, 8);
                 (jfor.j_rd,5);(jfor.j_opcode,7)]
;;

let ins2word (i: inst) : int32 = instformat2word (ins2instformat i)
;;

(*
let instformat_to_string f = match f with
| R rs -> ("R"^(Int32.to_string rs.r_opcode))
| I is -> ("I"^(Int32.to_string is.i_opcode))
| J js -> ("J"^(Int32.to_string js.j_opcode))
| J js -> ("J"^(Int32.to_string js.j_opcode))
;;
   *)


let inst2str i =
  match i with
    Add (r1,r2,r3) -> "add "^(reg2str r1)^", "^(reg2str r2)^", "^(reg2str r3)^"\n"
  | Addi (r1,r2,imm) -> "add "^(reg2str r1)^", "^(reg2str r2)^", "^(Int32.to_string imm)^"\n"
  | Beq (r1,r2,imm) -> "beq "^(reg2str r1)^", "^(reg2str r2)^", "^(Int32.to_string imm)^"\n"
  | Jal (r, imm) -> "jal "^(reg2str r)^", "^(Int32.to_string imm)^"\n"
  | Jalr (rd, rs1, imm) -> "jalr "^(reg2str rd)^", "^(reg2str rd)^", "^(Int32.to_string imm)^"\n"
  | Li (r,imm) -> "li "^(reg2str r)^", "^(Int32.to_string imm)^"\n"
  | Lui (r,imm) -> "lui "^(reg2str r)^", "^(Int32.to_string imm)^"\n"
  | Ori (r1,r2,imm) -> "ori "^(reg2str r1)^", "^(reg2str r2)^", "^(Int32.to_string imm)^"\n"
  | Lw (r1,r2,imm) -> "lw "^(reg2str r1)^", "^(Int32.to_string imm)^"("^(reg2str r2)^")\n"
  | Sw (r1,r2,imm) -> "sw "^(reg2str r1)^", "^(Int32.to_string imm)^"("^(reg2str r2)^")\n"
;;

(*get byte 0,1,2,3 of a word*)

(* byte 0 is the lsbyte, 3 is the msbyte *)
(* lsbyte : least significant byte, msbyte : most significant byte *)
let getByte (w: int32) (j: int) : byte =
  mk_byte (Int32.shift_right_logical w (8*j))
;;

assert (getByte 0xFEEDFACEl 0 = mk_byte 0xCEl);;
assert (getByte 0xFEEDFACEl 1 = mk_byte 0xFAl);;
assert (getByte 0xFEEDFACEl 2 = mk_byte 0xEDl);;
assert (getByte 0xFEEDFACEl 3 = mk_byte 0xFEl);;

(* load program code into 'memory'*)
let rec assem_code (prog : program) (offset : int32)
  (init_mem: memory) : memory = snd (List.fold_left
  (fun (s:(int32*memory)) (i:inst)->
    let curWord = ins2word i in
    let curPos = fst s in
    let curMem = snd s in
    (Int32.add curPos 4l,
      mem_update (curPos) (getByte curWord  0) (
      mem_update (Int32.add curPos 1l) (getByte curWord 1) (
      mem_update (Int32.add curPos 2l) (getByte curWord 2) (
      mem_update (Int32.add curPos 3l) (getByte curWord 3) (
        curMem))))
      )
  )
  (init_pc,init_mem) (rem_pseudo prog))
;;

let rec assem (prog : program) : state =
  {r=init_regfile;pc=init_pc;m=assem_code prog init_pc empty_mem}
;;

(*********** Assembler Tests ***************)

open Format

(* Encoding tests *)
let e_add = Add(R1,R2,R3)
let e_addi = Addi(R10,R2,-2029l)
let e_beq = Beq(R4,R5,0xcl)
let e_beq2 = Beq(R4,R5,-16l)
let e_jal = Jal(R10,0x10l)
let e_jal2 = Jal(R10,-0x20l)
let e_jalr = Jalr(R1,R6,0x12cl)
let e_lui = Lui(R9,267309056l)
let e_ori = Ori(R27,R20,-0x7edl)
let e_lw = Lw(R27,R20,-0x7c1l)
let e_sw = Sw(R15,R14,0x7a7l)
let e_li = Li(R9,0xFEEDFACEl)
let e_li2 = Li(R3,0x8F3DFADEl)

(* Expected results *)
let e_add_res = 0x003100b3l
let e_addi_res = 0x81310513l
let e_beq_res = 0x00520663l
let e_beq2_res = 0xfe5208e3l
let e_jal_res = 0x0100056fl
let e_jal2_res = 0xfe1ff56fl
let e_jalr_res = 0x12c300e7l
let e_lui_res = 0x0feed4b7l
let e_ori_res = 0x813a6d93l
let e_lw_res = 0x83fa2d83l
let e_sw_res = 0x7ae7a3a3l
let e_li_res = (0xfeee04b7l, 0xace48493l)
let e_li2_res = (0x8f3e01b7l, 0xade18193l)

(* 4 bytes to Int32 *)
(* b1 is the least signifcant, b4 is least significant *)
let b2i b4 b3 b2 b1 =
  let t1 = b2i32 b1 in
  let t2 = Int32.add (Int32.shift_left (b2i32 b2) 8) t1 in
  let t3 = Int32.add (Int32.shift_left (b2i32 b3) 16) t2 in
  Int32.add (Int32.shift_left (b2i32 b4) 24) t3
;;

assert (b2i (mk_byte 0xFEl) (mk_byte 0xEDl) (mk_byte 0xFAl) (mk_byte 0xCEl) = 0xFEEDFACEl) ;;
assert (b2i (mk_byte 0x00l) (mk_byte 0x00l) (mk_byte 0x00l) (mk_byte 0x12l) = 0x00000012l) ;;

(* read a word from memory (big-endian) *)
let read_word_big_endian (mem : memory) (addr : int32) =
  let b4 = mem_lookup addr mem in
  let b3 = mem_lookup (Int32.add addr 0x1l) mem in
  let b2 = mem_lookup (Int32.add addr 0x2l) mem in
  let b1 = mem_lookup (Int32.add addr 0x3l) mem in
  b2i b4 b3 b2 b1

let read_word_little_endian (mem : memory) (addr : int32) =
  let b1 = mem_lookup addr mem in
  let b2 = mem_lookup (Int32.add addr 0x1l) mem in
  let b3 = mem_lookup (Int32.add addr 0x2l) mem in
  let b4 = mem_lookup (Int32.add addr 0x3l) mem in
  b2i b4 b3 b2 b1

(* reverse the endianness of an Int32 *)
let rev_endianess w =
  let b1 = Int32.shift_right (Int32.logand w 0xFF000000l) 24 in
  let b2 = Int32.shift_right (Int32.logand w 0x00FF0000l) 16 in
  let b3 = Int32.shift_right (Int32.logand w 0x0000FF00l) 8 in
  let b4 = Int32.logand w 0x000000FFl in
  b2i (mk_byte b1) (mk_byte b2) (mk_byte b3) (mk_byte b4)

(* encode a single instruction *)
let encode inst =
  let state = assem [inst] in
  read_word_little_endian state.m state.pc

(* testing a pseudo instruction that will expand 2 words *)
let encode2 inst =
  let state = assem [inst] in
  (read_word_little_endian state.m state.pc,
   read_word_little_endian state.m (Int32.add state.pc 4l))

(* test an encoding *)
let teste i s =
  let encoding = encode i in
  if (encoding = s)
  then print_string ((format_string "passed: " Bright Green)^prog2str([i]))
  else print_string ((format_string "failed: " Bright Red)^(tostring encoding)^"   "^prog2str([i]))

let teste2 i s =
  let encoding = encode2 i in
  if (encoding = s)
  then print_string ((format_string "passed: " Bright Green)^prog2str([i]))
  else print_string ((format_string "failed: " Bright Red)^(tostring (fst encoding))^" "^(tostring (snd encoding))
                           ^"  "^prog2str([i])^" "^prog2str (rem_pseudo [i]))

let test_encoding () =
  let _ = teste e_add e_add_res in
  let _ = teste e_addi e_addi_res in
  let _ = teste e_beq e_beq_res in
  let _ = teste e_beq2 e_beq2_res in
  let _ = teste e_jal e_jal_res in
  let _ = teste e_jal2 e_jal2_res in
  let _ = teste e_jalr e_jalr_res in
  let _ = teste e_lui e_lui_res in
  let _ = teste e_ori e_ori_res in
  let _ = teste e_lw e_lw_res in
  let _ = teste e_sw e_sw_res in
  let _ = teste2 e_li e_li_res in
  let _ = teste2 e_li2 e_li2_res in
();;
