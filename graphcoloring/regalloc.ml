open Riscv
open Cfg_ast
open Cfg

exception AllocError of string
exception Implement_Me

module NodeMap = Map.Make(Cfg.IGraphNode)                                                   

module RegSet = Set.Make(struct
                  type t = reg
                  let compare = fun x y -> (String.compare (reg2string x) (reg2string y))
                end)
let reglist2sset ls =
  List.fold_left (fun ls var -> RegSet.add var ls) RegSet.empty ls

let validreglist = 
  R5 ::   R6 ::  R7  ::  R8 :: R9 ::  R10 :: 
  R11 :: R12 :: R13 :: R14 :: R15 :: R16 :: R17 :: R18 :: R19 :: R20 ::
  R21 :: R22 :: R23 ::R24::R25::R26::R27::R28::R29 :: R30 :: R31 :: []
let regcount = List.length validreglist
let validregset = reglist2sset validreglist 

let rewriteOp colormap o =
  match o with
    | Var x -> 
        (match NodeMap.find (Cfg.VarNode x) colormap with
          | None -> raise (AllocError "No color assigned for that variable.") 
          | Some r -> Cfg_ast.Reg r)
    | _ -> o

let rewriteInst colormap (i: Cfg_ast.inst) : Cfg_ast.inst = 
  let rewrite = rewriteOp colormap in
    match i with
      | Label x -> Label x
      | Move (x, y) -> Move (rewrite x, rewrite y)
      | Arith (x, y, p, z) -> Arith (rewrite x, rewrite y, p, rewrite z)
      | Load (x, y, i) -> Load (rewrite x, rewrite y, i)
      | Store (x, i, y) -> Store (rewrite x, i, rewrite y)
      | Call (x, n) -> Call (rewrite x, n)
      | Jump x -> Jump x
      | If (x,b,y,t,f) -> If (rewrite x, b, rewrite y, t, f)
      | Return -> Return


type colormap = ((RegSet.elt option) NodeMap.t)

type assign_result = 
    | Success of colormap
    | Fail of var list

(*******************************************************************)
(* PS8 TODO:  graph coloring *)

(* given an inteference graph, return an assign_result, which consists of either
   (1) Success cm, where cm is a coloring, i.e. a map from variables to registers
   (2) Fail vs, where vs is a list of variables to spill, when a graph coloring could not be found

 *)

 exception NodeTypeError
let print_stack_list lst =  List.iter (fun x -> print_endline (string_of_node x)) lst

let print_cmap (cmap:(RegSet.elt option) NodeMap.t) = 
  NodeMap.fold (fun key value acc ->
    print_string ((string_of_node key)^": {");
    (match value with
    | Some r -> print_string (reg2string r)
    | None -> print_string "None");
    print_endline "}") cmap ()

let assign_colors (ig: Cfg.interfere_graph) f : assign_result =
  let regSet,varSet = NodeSet.fold (fun n (accReg,accVar) -> (
    match n with
    | RegNode(_) -> (NodeSet.add n accReg,accVar)
    | VarNode(_) -> (accReg,NodeSet.add n accVar)))
     (IUGraph.nodes ig) (NodeSet.empty,NodeSet.empty)
  in
  let rmReg = NodeSet.fold (fun n acc -> (
    match n with
    | RegNode(_) -> IUGraph.rmNode n acc
    | VarNode(_) -> acc)) (IUGraph.nodes ig) ig
  in
  print_endline (string_of_igraph rmReg);
  let genstack_help subig = 
    NodeSet.fold (fun n (acc_stack,acc_g) -> (
      if NodeSet.cardinal (IUGraph.adj n subig) < regcount then
        (n::acc_stack,IUGraph.rmNode n acc_g)
      else
        (acc_stack,acc_g)
    )) (IUGraph.nodes subig) ([],subig)
  in
  let rec genstack substack subig = 
    let (upd_stack, upd_subig) = genstack_help subig in
    (* let upd_stack = upd_stack@substack *)
    if (List.length upd_stack) == 0 then
      (upd_stack@substack,upd_subig)
    else
      genstack (upd_stack@substack) upd_subig
  in
  let (stack,checkig) = genstack [] rmReg in
  print_endline (string_of_igraph checkig);
  print_endline "stack:";print_int (List.length stack);print_stack_list stack;
  if NodeSet.is_empty (IUGraph.nodes checkig) then
    let cmap = 
      let regcolormap = 
        (NodeSet.fold (fun n acc -> 
          (match n with
          | RegNode(r) -> NodeMap.add n (Some r) acc
          | _ -> raise NodeTypeError)
          )
        regSet NodeMap.empty)
      in
      (* print_cmap regcolormap; *)
      let initcolormap =
        NodeSet.fold (fun n acc->
          NodeMap.add n None acc) varSet regcolormap
      in
      let colormap = 
        (List.fold_left (fun acc_cm node -> 
          let neighborRegs = 
            (NodeSet.fold (fun n acc ->
              let c_neighbor = NodeMap.find n (acc_cm) in
              match c_neighbor with
              | Some(reg) -> RegSet.add reg acc
              | None -> acc) (IUGraph.adj node ig) RegSet.empty)
          in
          let valreg = RegSet.diff validregset neighborRegs in
          let chooseReg = RegSet.choose valreg in
          NodeMap.add node (Some chooseReg) acc_cm) 
          initcolormap stack)
      in
      print_cmap colormap;
      colormap
      (* raise Implement_Me *)
    in
    Success(cmap)
  else
    Fail(NodeSet.fold (fun n acc -> (string_of_node n)::acc) (IUGraph.nodes checkig) [])

let rec reg_alloc_spill (fraw : func) (sl: var list): func = 
  (*First spill all of the vars in sl by adding loads/stores to fraw*)
  let f = Spill.spill fraw sl in
  let ig = Cfg.build_interfere_graph f in
  let colormapopt = assign_colors ig f in
    match colormapopt with
      | Success colormap ->
          let allocatedf = List.map (fun x -> List.map (rewriteInst colormap) x) f in
            (*Get rid of trivial moves*)
            List.map 
              (fun b -> List.filter
                          (fun i ->
                             match i with
                               | Move (o1,o2) -> (if(o1=o2) then false else true)
                               | _ -> true
                          ) b
              )
              allocatedf
      | Fail spilllist ->
            reg_alloc_spill fraw (List.append spilllist sl)

let reg_alloc (f:func) : func =
  reg_alloc_spill f []



(*========*)



(************************************)

(** This is the hook for your compiler. Please keep your
    implementation contained to this file, plus, optionally the file
    cfg.ml *)

let process_fn fn =
  let curfblocks = (Cfg_ast.fn2blocks fn) in
  reg_alloc curfblocks

let compile prog =
  let blocks = List.flatten (List.map (fun fn -> process_fn fn) prog) in
  Cfg_compile.cfg_to_riscv blocks

(***********************************)

(**
   Here is a template for one strategy to get a basic implementation
   working, which would just require implementing reg_alloc to map
   temporaries to registers.

   For each function, it calls Cfg_ast.fn2blocks to convert it into a
   Cfg representation of basic blocks, then calls a register allocator
   to map temporaries to registers, and finally uses Cfg_compile to
   convert the Cfg blocks to RISC-V.

   But you don't have to use this approach!
*)

(*

let reg_alloc blocks =
  exception FatalError

let process_fn fn =
  let curfblocks = (Cfg_ast.fn2blocks fn) in
  reg_alloc curfblocks

let compile prog =
  let blocks = List.flatten (List.map (fun fn -> process_fn fn) prog) in
  Cfg_compile.cfg_to_riscv blocks

*)