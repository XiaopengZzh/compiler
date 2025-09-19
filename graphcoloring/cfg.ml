open Cfg_ast
exception Implement_Me
exception FatalError


type igraph_node = RegNode of Riscv.reg | VarNode of var

let string_of_node (n: igraph_node) : string =
  match n with
  | RegNode r -> "$" ^ Riscv.reg2string r
  | VarNode v -> v
;;

module IGraphNode =
  struct
    type t = igraph_node
    let compare = compare
  end

module NodeSet = Set.Make(IGraphNode)                                                   

(* These are the registers that must be generated / killed as part of
   liveness analysis for call instructions to reflect RISC-V calling
   conventions *)

(* Note that for call_gen_list, if the number of arguments n in the
   call is less than 8, then only the first n of these are actually
   used *)
let call_gen_list = ["x10";"x11";"x12";"x13";"x14";"x15";"x16";"x17";]
let call_kill_list = ["x1";"x5";"x6";"x7";"x10";"x11";"x12";"x13";"x14";"x15";"x16";"x17";"x28";"x29";"x30";"x31"]

(* Undirected graphs where nodes are identified by igraph_node type above. Look at
   graph.ml for the interface description.  *)

module IUGraph = Graph.UndirectedGraph(IGraphNode)

(* this is a wrapper to addEdge that prevents adding self edges.
   to do all sorts of other complicated stuff for eg coloring *)
let specialAddEdge u v g =
  if (u = v) then
    g
  else
    IUGraph.addEdge u v g

(* An interference graph is an SUGraph where a node is temp variable
   or a register (to be able to handle pre-colored nodes)

   The adjacency set of variable x should be the set of variables
   y such that x and y are live at the same point in time. *)
type interfere_graph = IUGraph.graph

(* To help you printing an igraph for debugging *)
let string_of_igraph (g: interfere_graph) : string =
  let rec string_of_row (n: IUGraph.node) =
    let ns = IUGraph.adj n g in
    Printf.sprintf "  %s\t: {%s}"
      (string_of_node n)
      (String.concat "," (List.map string_of_node (NodeSet.elements ns)))
  in
  let rows = String.concat "\n" (List.map string_of_row (NodeSet.elements (IUGraph.nodes g))) in
  Printf.sprintf "{\n%s\n}\n" rows


(*******************************************************************)
(* PS7 TODO:  interference graph construction *)

(* given a function (i.e., list of basic blocks), construct the
 * interference graph for that function.  This will require that
 * you build a dataflow analysis for calculating what set of variables
 * are live-in and live-out for each program point. *)
 let rec take n xs =
  if n <= 0 then [] else
  match xs with
  | []   -> []
  | y::ys -> y :: take (n-1) ys
  
let string2reg = function
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
 | _ -> raise Implement_Me

(* Given a string list of reg names, return corresponding node set *)
let strlst2NodeSet lst =
  List.fold_left (fun acc s -> NodeSet.add (RegNode (string2reg s)) acc)
                 NodeSet.empty lst

(* convert operand to igraph_node, skip label and int *)
let operand2node (op : operand) : igraph_node option =
  match op with
  | Var v -> Some (VarNode v)
  | Reg r -> Some (RegNode r)
  | _     -> None

(* convert list of operand to NodeSet *)
let operandLst2NodeSet lst =
  List.fold_left
    (fun s op -> match operand2node op with
                 | None   -> s
                 | Some n -> NodeSet.add n s)
    NodeSet.empty lst

(* Given an instruction, calculate its gens and kills *)
let calc_Gens_and_Kills_of_inst (i : inst) : NodeSet.t * NodeSet.t = 
  match i with
  | Label _ -> (NodeSet.empty, NodeSet.empty)
  | Move (dst, src) -> 
    (operandLst2NodeSet [src], operandLst2NodeSet [dst])
  | Arith (dst, op1, _, op2) ->
    (operandLst2NodeSet [op1; op2], operandLst2NodeSet [dst])
  | Load (dst, addr, _) ->
    (operandLst2NodeSet [addr], operandLst2NodeSet [dst])
  | Store (addr, _, src) ->
    (operandLst2NodeSet [addr; src], NodeSet.empty)
  | Call (_, n_args) ->
    let gen  = strlst2NodeSet (take n_args call_gen_list) in
    (gen, NodeSet.empty)
  | Jump _ -> (NodeSet.empty, NodeSet.empty)
  | If (op1, _, op2, _, _) ->
    (operandLst2NodeSet [op1; op2], NodeSet.empty)
  | Return -> 
    (NodeSet.singleton (RegNode Riscv.R10), NodeSet.empty)


let liveness_analysis (succ : int list array) (gen : NodeSet.t array) (kill : NodeSet.t array) : NodeSet.t array * NodeSet.t array = 
  let n = Array.length succ in
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
  (live_in, live_out)


let build_interfere_graph_internal (insts : inst list) (live_out : NodeSet.t array) (gen : NodeSet.t array) (kill : NodeSet.t array) : interfere_graph = 
  let g = ref IUGraph.empty in
  let add_all_nodes (tbl : NodeSet.t array) =
    Array.iter
      (fun s -> NodeSet.iter (fun n -> g := IUGraph.addNode n !g) s)
      tbl
  in
  add_all_nodes gen;
  add_all_nodes kill;
  List.iteri
    (fun idx ins ->
       let defs = kill.(idx) in
       let lives =
         match ins with
         | Move (_, src) ->
             let lives0 = live_out.(idx) in
             (match operand2node src with
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

let build_interfere_graph (f : func) : interfere_graph = 
  let insts = List.flatten f in
  let n = List.length insts in
  if n = 0 then IUGraph.empty else
  
  let label2idx = Hashtbl.create 17 in
  List.iteri
    (fun idx ins -> match ins with
                    | Label l -> Hashtbl.replace label2idx l idx
                    | _ -> ())
    insts;
  (* for each instruction, calculate its successors, gens and kills *)
  let succ   = Array.make n []          in
  let gen    = Array.make n NodeSet.empty in
  let kill   = Array.make n NodeSet.empty in
  List.iteri
    (fun i ins ->
       let (g,k) = calc_Gens_and_Kills_of_inst ins in
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
  (* calculate livein and liveout for all instructions *)
  let (_, live_out) = liveness_analysis succ gen kill in
  (* Given the result of liveness analysis, build interference graph *)
  build_interfere_graph_internal insts live_out gen kill
