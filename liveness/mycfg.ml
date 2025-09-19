(*******************************************************************)
(* interference‑graph construction                                 *)
(*******************************************************************)

(* ---------- 一些小工具 ---------- *)

let rec take n xs =
  if n <= 0 then [] else
  match xs with
  | []   -> []
  | y::ys -> y :: take (n-1) ys

let node_of_operand (op : operand) : igraph_node option =
  match op with
  | Var v -> Some (VarNode v)
  | Reg r -> Some (RegNode r)
  | _     -> None                      (* Int / Lab 不参与寄存器分配 *)

let nodes_of_oplst lst =
  List.fold_left
    (fun s op -> match node_of_operand op with
                 | None   -> s
                 | Some n -> NodeSet.add n s)
    NodeSet.empty lst

(* 仅涵盖调用协议里会用到的寄存器 *)
let reg_of_string = function
  | "x0"  -> Riscv.R0  | "x1"  -> Riscv.R1  | "x2"  -> Riscv.R2
  | "x3"  -> Riscv.R3  | "x4"  -> Riscv.R4  | "x5"  -> Riscv.R5
  | "x6"  -> Riscv.R6  | "x7"  -> Riscv.R7  | "x8"  -> Riscv.R8
  | "x9"  -> Riscv.R9  | "x10" -> Riscv.R10 | "x11" -> Riscv.R11
  | "x12" -> Riscv.R12 | "x13" -> Riscv.R13 | "x14" -> Riscv.R14
  | "x15" -> Riscv.R15 | "x16" -> Riscv.R16 | "x17" -> Riscv.R17
  | "x18" -> Riscv.R18 | "x19" -> Riscv.R19 | "x20" -> Riscv.R20
  | "x21" -> Riscv.R21 | "x22" -> Riscv.R22 | "x23" -> Riscv.R23
  | "x24" -> Riscv.R24 | "x25" -> Riscv.R25 | "x26" -> Riscv.R26
  | "x27" -> Riscv.R27 | "x28" -> Riscv.R28 | "x29" -> Riscv.R29
  | "x30" -> Riscv.R30 | "x31" -> Riscv.R31
  | _ -> raise FatalError

let node_of_regname s = RegNode (reg_of_string s)

let nodes_of_regnames lst =
  List.fold_left (fun acc s -> NodeSet.add (node_of_regname s) acc)
                 NodeSet.empty lst

(* ---------- gen / kill 计算 ---------- *)

let gen_kill_of_inst (i : inst) : NodeSet.t * NodeSet.t =
  match i with
  | Label _ -> (NodeSet.empty, NodeSet.empty)

  | Move (dst, src) ->
      let gen  = nodes_of_oplst [src] in
      let kill = nodes_of_oplst [dst] in
      (gen, kill)

  | Arith (dst, op1, _, op2) ->
      (nodes_of_oplst [op1; op2], nodes_of_oplst [dst])

  | Load (dst, addr, _) ->
      (nodes_of_oplst [addr]      , nodes_of_oplst [dst])

  | Store (addr, _, src) ->
      (nodes_of_oplst [addr; src] , NodeSet.empty)

  | Call (_, n_args) ->
      let gen  = nodes_of_regnames (take n_args call_gen_list) in
      let kill = nodes_of_regnames call_kill_list in
      (gen, kill)

  | Jump _ -> (NodeSet.empty, NodeSet.empty)

  | If (op1, _, op2, _, _) ->
      (nodes_of_oplst [op1; op2]  , NodeSet.empty)

  | Return ->
      (* 函数返回值在 x10(R10) 中；不规定 kill *)
      (NodeSet.singleton (RegNode Riscv.R10), NodeSet.empty)

(* ---------- 主过程 ---------- *)

let build_interfere_graph (f : func) : interfere_graph =
  (* 1) 线性展平基本块，记录每条指令的索引 *)
  let insts = List.flatten f in
  let n     = List.length insts in
  if n = 0 then IUGraph.empty else

  (* 2) 为 block 首指令建立 label -> index 映射 *)
  let label2idx = Hashtbl.create 17 in
  List.iteri
    (fun idx ins -> match ins with
                    | Label l -> Hashtbl.replace label2idx l idx
                    | _ -> ())
    insts;

  (* 3) 为每条指令预计算 succ / gen / kill *)
  let succ   = Array.make n []          in
  let gen    = Array.make n NodeSet.empty in
  let kill   = Array.make n NodeSet.empty in

  List.iteri
    (fun i ins ->
       let (g,k) = gen_kill_of_inst ins in
       gen.(i)  <- g;  kill.(i) <- k;
       succ.(i) <-
         (match ins with
          | Jump l ->
              [Hashtbl.find label2idx l]
          | If (_,_,_, l1, l2) ->
              [Hashtbl.find label2idx l1; Hashtbl.find label2idx l2]
          | Return ->
              []
          | _ ->
              if i+1 < n then [i+1] else []))
    insts;

  (* 4) 求 live‑in / live‑out 不动点 *)
  let live_in  = Array.make n NodeSet.empty in
  let live_out = Array.make n NodeSet.empty in
  let changed  = ref true in
  while !changed do
    changed := false;
    for i = n-1 downto 0 do
      let out_i =
        List.fold_left
          (fun s j -> NodeSet.union s live_in.(j))
          NodeSet.empty succ.(i) in
      let in_i  =
        NodeSet.union gen.(i) (NodeSet.diff out_i kill.(i)) in
      if not (NodeSet.equal out_i live_out.(i) &&
              NodeSet.equal in_i  live_in.(i)) then
        begin
          changed := true;
          live_out.(i) <- out_i;
          live_in.(i)  <- in_i
        end
    done
  done;

  (* 5) 构造干涉图 *)
  let g = ref IUGraph.empty in

  (* 5a. 先把出现过的所有节点都插进去 *)
  Array.iteri
    (fun _ s -> NodeSet.iter (fun n -> g := IUGraph.addNode n !g) s)
    gen;
  Array.iteri
    (fun _ s -> NodeSet.iter (fun n -> g := IUGraph.addNode n !g) s)
    kill;

  (* 5b. 根据 def × live‑out 加边（Move 要排除 src） *)
  List.iteri
    (fun idx ins ->
       let defs = kill.(idx) in
       let lives =
         match ins with
         | Move (_, src) ->
             (* Move: 不把 src 与 dst 视为干涉 *)
             let lives0 = live_out.(idx) in
             (match node_of_operand src with
              | Some n -> NodeSet.remove n lives0
              | None   -> lives0)
         | _ -> live_out.(idx)
       in
       NodeSet.iter
         (fun d ->
            NodeSet.iter
              (fun l -> g := specialAddEdge d l !g)
              lives)
         defs)
    insts;

  !g
