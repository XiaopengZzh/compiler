open Cfg_ast
open Cfg

exception Implement_Me
exception FatalError

(************************************)

(** Register allocation pass with simplified caller‑saved‑only policy
    plus a peephole optimisation that删除多余自拷贝指令。*)

(** ----------  Helper definitions  ---------------------------------- *)

module VarMap = Map.Make (String)
module RegSet = Set.Make (struct type t = Riscv.reg let compare = compare end)

(** Only *caller‑saved* registers are allocated:
    - t0‑t6  : x5,x6,x7,x28,x29,x30,x31
    - a0‑a7  : x10‑x17 *)
let allocatable_regs : Riscv.reg list =
  [ Riscv.R5; R6; R7;            (* t0‑t2 *)
    R28; R29; R30; R31;          (* t3‑t6 *)
    R10; R11; R12; R13;          (* a0‑a3 *)
    R14; R15; R16; R17 ]         (* a4‑a7 *)

let scratch_pool = ref [ Riscv.R5; Riscv.R6; Riscv.R7 ]      (* x5 x6 x7 *)

let fresh_scratch () =
  match !scratch_pool with
  | r :: rs -> scratch_pool := rs; r
  | [] -> failwith "ran out of scratch registers"

let release_scratch r =
  scratch_pool := r :: !scratch_pool

(** ----------  Greedy colouring ------------------------------------- *)

let colour (igraph : Cfg.interfere_graph) : Riscv.reg option VarMap.t =
  let initial_map =
    NodeSet.fold
      (fun n acc -> match n with
         | RegNode r -> VarMap.add ("$" ^ Riscv.reg2string r) (Some r) acc
         | _ -> acc)
      (IUGraph.nodes igraph) VarMap.empty
  in
  let vars =
    IUGraph.nodes igraph
    |> NodeSet.elements
    |> List.filter_map (function VarNode v -> Some v | _ -> None)
    |> List.sort (fun v1 v2 ->
        let d v = NodeSet.cardinal (IUGraph.adj (VarNode v) igraph) in
        let dv1, dv2 = d v1, d v2 in
        if dv1 = dv2 then compare v1 v2 else compare dv2 dv1)
  in
  let rec colour_vars ls cmap =
    match ls with
    | [] -> cmap
    | v :: rest ->
        let occupied =
          let adj = IUGraph.adj (VarNode v) igraph in
          NodeSet.fold
            (fun n set -> match n with
               | RegNode r -> RegSet.add r set
               | VarNode w ->
                   (match VarMap.find_opt w cmap with
                    | Some (Some r) -> RegSet.add r set
                    | _ -> set))
            adj RegSet.empty
        in
        let choice = List.find_opt (fun r -> not (RegSet.mem r occupied)) allocatable_regs in
        colour_vars rest (VarMap.add v choice cmap)
  in
  colour_vars vars initial_map

(** ----------  Spill rewrite ---------------------------------------- *)

type spill_info = { offset : int }

let rewrite (blocks : block list) (cmap : Riscv.reg option VarMap.t) : block list =
  let word = 4 in
  let tbl : (var, spill_info) Hashtbl.t = Hashtbl.create 17 in
  let next = ref (-word) in
  let fresh_slot () = let o = !next in next := o - word; o in
  let slot v =
    match Hashtbl.find_opt tbl v with
    | Some { offset } -> offset
    | None ->
        let off = fresh_slot () in
        Hashtbl.add tbl v { offset = off }; off
  in
  (* Return: pre‑inst list, substituted operand, post‑inst list *)
  let subst (op : operand) : inst list * operand * inst list =
    match op with
    | Var v ->
        (match VarMap.find_opt v cmap with
         | Some (Some r) -> ([], Reg r, [])                           (* 已着色 *)
         | _ ->                                                       (* spill *)
             let off = slot v in
             let r = fresh_scratch() in
             ([ Load  (Reg r, Reg Riscv.R3, off) ],
              Reg r,
              [ Store (Reg Riscv.R3, off, Reg r) ] @ (release_scratch r; [])))
    | _ -> ([], op, [])
  in
  let rewrite_inst (i : inst) : inst list =
    match i with
    | Label _ | Return | Jump _ -> [ i ]
    | Move (dst, src) ->
        let (pre_d, dst', post_d) = subst dst in
        let (pre_s, src', post_s) = subst src in
        let body = if dst' = src' then [] else [ Move (dst', src') ] in
        pre_d @ pre_s @ body @ post_d @ post_s
    | Arith (dst, o1, op, o2) ->
        let (p_d, dst', q_d) = subst dst in
        let (p1, o1', q1)    = subst o1  in
        let (p2, o2', q2)    = subst o2  in
        p_d @ p1 @ p2 @ [ Arith (dst', o1', op, o2') ] @ q_d @ q1 @ q2
    | Load (dst, addr, off) ->
        let (p_d, dst', q_d)   = subst dst in
        let (p_a, addr', q_a)  = subst addr in
        p_d @ p_a @ [ Load (dst', addr', off) ] @ q_d @ q_a
    | Store (addr, off, src) ->
        let (p_a, addr', q_a)  = subst addr in
        let (p_s, src', q_s)   = subst src  in
        p_a @ p_s @ [ Store (addr', off, src') ] @ q_a @ q_s
    | Call (f, n) ->
        let (p, f', q) = subst f in
        p @ [ Call (f', n) ] @ q
    | If (o1, cmp, o2, l1, l2) ->
        let (p1, o1', q1) = subst o1 in
        let (p2, o2', q2) = subst o2 in
        p1 @ p2 @ [ If (o1', cmp, o2', l1, l2) ] @ q1 @ q2
  in
  let rewrite_block blk = List.flatten (List.map rewrite_inst blk) in
  List.map rewrite_block blocks

(** ----------  Peephole:删除 Move r,r ------------------------------- *)

let remove_moves blocks =
  let ok = function Move (Reg r1, Reg r2) when r1 = r2 -> false | _ -> true in
  List.map (List.filter ok) blocks

(** ----------  reg_alloc 主入口 ------------------------------------ *)

let reg_alloc (blocks : block list) : block list =
  let ig = Cfg.build_interfere_graph blocks in
  let cmap = colour ig in
  let spill = VarMap.exists (fun _ v -> v = None) cmap in
  let new_blocks = if spill then rewrite blocks cmap else (
    let subst op = match op with Var v -> (match VarMap.find_opt v cmap with Some (Some r) -> Reg r | _ -> raise FatalError) | _ -> op in
    let map_inst = function
      | Move (d,s) -> if subst d = subst s then None else Some (Move (subst d,subst s))
      | Arith (d,o1,p,o2) -> Some (Arith (subst d, subst o1, p, subst o2))
      | Load (d,a,i) -> Some (Load (subst d, subst a, i))
      | Store (a,i,s) -> Some (Store (subst a, i, subst s))
      | Call (f,n) -> Some (Call (subst f, n))
      | If (o1,c,o2,l1,l2) -> Some (If (subst o1,c,subst o2,l1,l2))
      | other -> Some other in
    List.map (fun blk -> List.filter_map map_inst blk) blocks) in
  remove_moves new_blocks

(**********************************************************************)
(**                   Prologue / Epilogue insertion                  *)
(**********************************************************************)

(* size (bytes) we push. 16 is enough for ra plus alignment. *)
let frame_size = 16

let prologue : inst list =
  [ (* sp := sp - frame_size *)
    Arith (Reg Riscv.R2, Reg Riscv.R2, Minus, Int frame_size);
    (* sw ra, frame_size-4(sp)  (offset 12) *)
    Store (Reg Riscv.R2, frame_size - 4, Reg Riscv.R1) ]

let epilogue : inst list =
  [ (* lw ra, frame_size-4(sp) *)
    Load (Reg Riscv.R1, Reg Riscv.R2, frame_size - 4);
    (* sp := sp + frame_size *)
    Arith (Reg Riscv.R2, Reg Riscv.R2, Plus, Int frame_size) ]

let add_pro_epi (fn_blocks : block list) : block list =
  match fn_blocks with
  | [] -> []
  | first :: rest ->
      (* put prologue right after the first label (or at block head) *)
      let first' =
        match first with
        | Label l :: tl -> Label l :: (prologue @ tl)
        | _ -> prologue @ first
      in
      let rewrite_block blk =
        blk
        |> List.map (function
             | Return -> epilogue @ [ Return ]
             | inst   -> [ inst ])
        |> List.flatten
      in
      first' :: List.map rewrite_block rest

(**********************************************************************)
(**                     compile driver (demo)                        *)
(**********************************************************************)

let is_main_fn = function Cish_ast.Fn {Cish_ast.name="main"; _} -> true | _ -> false

let reorder prog =
  let mains, others = List.partition is_main_fn prog in
  mains @ others

let process_fn (fn : Cish_ast.func) : block list =
  fn |> Cfg_ast.fn2blocks |> reg_alloc |> add_pro_epi

let compile prog =
  let ordered = reorder prog in
  let body_blocks = List.flatten (List.map process_fn ordered) in
  Cfg_compile.cfg_to_riscv body_blocks
