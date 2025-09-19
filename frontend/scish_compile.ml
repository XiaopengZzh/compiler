(* TODO:  your job is to map ScishAst expressions to CishAst functions. 
   The file sample_input.scish shows a sample Scish expression and the
   file sample_output.cish shows the output I get from my compiler.
   You will want to do your own test cases...
 *)

exception Unimplemented
exception Unbound_Variable of string
exception UV1
exception UV2
exception UV3


open Cish_ast
open Scish_ast

let pos0 = 0 (* Dummy position *)

type compiler_state = {
  mutable counter : int;
  mutable functions : Cish_ast.func list
}

let add_function (state : compiler_state) (smt : Cish_ast.stmt) (funcname : var) = 
  state.functions <- (Fn {name = funcname; args = ["dynenv"]; body = smt; pos = pos0})::state.functions

(* ======================================================== *)
type env = (Cish_ast.var * int) list

let increment (ev : env) : env = 
  List.map (fun (v, i) -> (v, i + 1)) ev

let add_member (ev : env) (str : var) (num : int) : env = 
  (str, num)::ev

let rec lookup (ev : env) (name : var) : int option = 
  match ev with
  | [] -> None
  | (v, i) :: rest -> 
    (
      if v = name then Some i
      else lookup rest name
    )

(* ======================================================== *)

let new_temp (state : compiler_state) : string = 
  let name = "t" ^ (Int.to_string state.counter) in
  state.counter <- state.counter + 1;
  name

let rec sequencing (smtl : Cish_ast.stmt list) : Cish_ast.stmt = 
  match smtl with
  | smt::[] -> smt
  | smt::rest -> (Seq (smt, sequencing rest), pos0)
  | _ -> raise Unimplemented

let rec access_var (depth : int) : Cish_ast.exp = 
  match depth with
  | 0 -> (Var "dynenv", pos0)
  | _ -> (Load (Binop ((access_var (depth - 1)), Plus, (Int 4, pos0)), pos0), pos0)

let rec process_exp (e : Scish_ast.exp) (state : compiler_state) (ev : env) : Cish_ast.stmt = 
  match e with
  | Int num -> 
    (Exp (Assign ("result", (Int num, pos0)), pos0), pos0)
  | Var v ->
    let depth = lookup ev v in
    (match depth with
    | Some d -> (Exp (Assign ("result", (Load (access_var d), pos0)), pos0), pos0)
    | _ -> raise (Unbound_Variable "1"))
  | PrimApp (op, elist) ->
    (
      match op with
      | Plus -> (let t = new_temp state in
          (match elist with
            | [e1;e2] -> 
              let tmp = sequencing [
                process_exp e1 state ev;
                (Exp (Assign (t, (Var "result", pos0)), pos0), pos0);
                process_exp e2 state ev;
                (Exp (Assign ("result", (Binop ((Var t, pos0), Plus, (Var "result", pos0)), pos0)), pos0), pos0)
                ] in
              (Let (t, (Int 0, pos0), tmp), pos0)
            | _ -> raise (Unbound_Variable "2")
          )
        )
      | Minus -> (let t = new_temp state in
          (match elist with
            | [e1;e2] -> 
                let tmp = sequencing [
                process_exp e1 state ev;
                (Exp (Assign (t, (Var "result", pos0)), pos0), pos0);
                process_exp e2 state ev;
                (Exp (Assign ("result", (Binop ((Var t, pos0), Minus, (Var "result", pos0)), pos0)), pos0), pos0)
                ] in
              (Let (t, (Int 0, pos0), tmp), pos0)
            | _ -> raise (Unbound_Variable "3")
          )
        )
      | Times -> (let t = new_temp state in
          (match elist with
            | [e1;e2] -> 
                let tmp = sequencing [
                process_exp e1 state ev;
                (Exp (Assign (t, (Var "result", pos0)), pos0), pos0);
                process_exp e2 state ev;
                (Exp (Assign ("result", (Binop ((Var t, pos0), Times, (Var "result", pos0)), pos0)), pos0), pos0)
                ] in
              (Let (t, (Int 0, pos0), tmp), pos0)
            | _ -> raise (Unbound_Variable "4")
          )
        )
      | Div -> (let t = new_temp state in
            (match elist with
              | [e1;e2] -> 
                  let tmp = sequencing [
                  process_exp e1 state ev;
                  (Exp (Assign (t, (Var "result", pos0)), pos0), pos0);
                  process_exp e2 state ev;
                  (Exp (Assign ("result", (Binop ((Var t, pos0), Div, (Var "result", pos0)), pos0)), pos0), pos0)
                  ] in
                (Let (t, (Int 0, pos0), tmp), pos0)
              | _ -> raise (Unbound_Variable "5")
            )
          )
      | Eq -> (let t = new_temp state in
            (match elist with
              | [e1;e2] -> 
                  let tmp = sequencing [
                  process_exp e1 state ev;
                  (Exp (Assign (t, (Var "result", pos0)), pos0), pos0);
                  process_exp e2 state ev;
                  (Exp (Assign ("result", (Binop ((Var t, pos0), Eq, (Var "result", pos0)), pos0)), pos0), pos0)
                  ] in
                (Let (t, (Int 0, pos0), tmp), pos0)
              | _ -> raise (Unbound_Variable "6")
            )
          )
      | Lt -> (let t = new_temp state in
            (match elist with
              | [e1;e2] -> 
                  let tmp = sequencing [
                  process_exp e1 state ev;
                  (Exp (Assign (t, (Var "result", pos0)), pos0), pos0);
                  process_exp e2 state ev;
                  (Exp (Assign ("result", (Binop ((Var t, pos0), Lt, (Var "result", pos0)), pos0)), pos0), pos0)
                  ] in
                (Let (t, (Int 0, pos0), tmp), pos0)
              | _ -> raise (Unbound_Variable "7")
            )
          )
      | Cons -> (let t = new_temp state in
            (match elist with
                | [e1;e2] -> 
                  Let (t, (Int 0, pos0), (sequencing [
                    (Exp (Assign (t, (Malloc (Int 8, pos0), pos0)), pos0), pos0);
                    process_exp e1 state ev;
                    (Exp (Store ((Var t, pos0), (Var "result", pos0)), pos0), pos0);
                    process_exp e2 state ev;
                    (Exp (Store ((Binop ((Var t, pos0), Plus, (Int 4, pos0)), pos0), (Var "result", pos0)), pos0), pos0);
                    (Exp (Assign ("result", (Var t, pos0)), pos0), pos0)
                  ])), pos0
                | _ -> raise UV1
            )
          )
      | Fst -> (match elist with
                  | [e] -> sequencing [
                    process_exp e state ev;
                    (Exp (Assign ("result", (Load (Var "result", pos0), pos0)), pos0), pos0)
                    ]
                  | _ -> raise UV2
          )
      | Snd -> (match elist with
                  | [e] -> sequencing [
                    process_exp e state ev;
                    (Exp (Assign ("result", (Load (Binop ((Var "result", pos0), Plus, (Int 4, pos0)), pos0), pos0)), pos0), pos0)
                  ]
                  | _ -> raise UV3
          )
    )
  | Lambda (arg, body) -> 
    let t = new_temp state in
    let tmp_env = increment ev in
    let new_env = add_member tmp_env arg 0 in
    let func_stmt = (Let ("result", (Int 0, pos0), 
      (Seq (process_exp body state new_env, (Return (Var "result", pos0), pos0)), pos0)), pos0) in
    let _ = add_function state func_stmt t in
    sequencing [
      (Exp (Assign ("result", (Malloc (Int 8, pos0), pos0)), pos0), pos0);
      (Exp (Store ((Var "result", pos0), (Var t, pos0)), pos0), pos0);
      (Exp (Store ((Binop ((Var "result", pos0), Plus, (Int 4, pos0)), pos0), (Var "dynenv", pos0)), pos0), pos0)
    ]
  | App (f, arg) -> 
    let t0 = new_temp state in
    let t1 = new_temp state in
    let t2 = new_temp state in
    let tmp = sequencing [
      process_exp f state ev;
      (Exp (Assign (t0, (Load (Var "result", pos0), pos0)), pos0), pos0);
      (Exp (Assign (t1, (Load (Binop ((Var "result", pos0), Plus, (Int 4, pos0)), pos0), pos0)), pos0), pos0);
      process_exp arg state ev;
      (Exp (Assign (t2, (Var "result", pos0)), pos0), pos0);
      (Exp (Assign ("result", (Malloc (Int 8, pos0), pos0)), pos0), pos0);
      (Exp (Store ((Var "result", pos0), (Var t2, pos0)), pos0), pos0);
      (Exp (Store ((Binop ((Var "result", pos0), Plus, (Int 4, pos0)), pos0), (Var t1, pos0)), pos0), pos0);
      (Exp (Assign ("result", (Call ((Var t0, pos0), [(Var "result", pos0)]), pos0)), pos0), pos0)
    ]in
    (Let (t0, (Int 0, pos0), (Let (t1, (Int 0, pos0), (Let (t2, (Int 0, pos0), tmp), pos0)), pos0)), pos0)
  | If (e1, e2, e3) ->
    sequencing [
      process_exp e1 state ev;
      (If ((Var "result", pos0), process_exp e2 state ev, process_exp e3 state ev), pos0)
    ]

let rec compile_exp (e:Scish_ast.exp) : Cish_ast.program = 
  let state = {counter = 0; functions = []} in
  let stmt_body = process_exp e state [] in
  let main_stmt = (Let ("result", (Int 0, pos0), (
    sequencing [
      (Let ("dynenv", (Int 0, pos0), stmt_body), pos0);
      (Return (Var "result", pos0), pos0)
    ]
  )), pos0) in
  let main_func = Fn {name = "main"; args = []; body = main_stmt; pos = pos0 } in
  main_func::state.functions




