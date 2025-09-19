module ML = Mlish_ast
module S = Scish_ast

exception ImplementMe

let rec compile_exp ((e,_):ML.exp) : S.exp = 
  match e with 
  | Var(v) -> Var(v)
  | Fn(v, e') -> Lambda(v, compile_exp e')
  | App(e1, e2) -> App(compile_exp e1, compile_exp e2)
  | If(e1, e2, e3) -> If(compile_exp e1, compile_exp e2, compile_exp e3)
  | Let(v, e1, e2) -> App(Lambda(v, compile_exp e2), compile_exp e1)
  | PrimApp(p, elst) -> 
    (
      let clst = List.map compile_exp elst in
      match p with
      | Int(i) -> Int(i)
      | Bool(b) -> if b then Int(1) else Int(0)
      | Unit -> Int(0)
      | Plus ->   PrimApp(Plus, clst)
      | Minus ->  PrimApp(Minus, clst)
      | Times ->  PrimApp(Times, clst)
      | Div ->    PrimApp(Div, clst)
      | Eq ->     PrimApp(Eq, clst)
      | Lt ->     PrimApp(Lt, clst)
      | Pair ->  PrimApp(Cons, clst)
      | Fst ->  PrimApp(Fst, clst)
      | Snd ->  PrimApp(Snd, clst) 
      | Nil -> PrimApp(Cons,[Int(0);Int(0)])
      | Cons -> (match clst with
                  | [e1; e2] -> PrimApp(Cons,[PrimApp(Plus,[Int(1);PrimApp(Fst,[e2])]); PrimApp(Cons,[e1; PrimApp(Snd,[e2])])])
                  | _ -> raise ImplementMe)
      | IsNil -> (match clst with
                  | [l] -> If(PrimApp(Eq,[PrimApp(Fst,clst);Int(0)]),Int(1),Int(0))
                  | _ -> raise ImplementMe)
      | Hd -> PrimApp(Fst, [PrimApp(Snd, clst)])
      | Tl -> PrimApp(Cons,[PrimApp(Minus,[PrimApp(Fst,clst);Int(1)]); PrimApp(Snd,[PrimApp(Snd,clst)])])
    )