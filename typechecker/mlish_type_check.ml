open Mlish_ast

exception TypeError
let type_error(s:string) = (print_string s; raise TypeError)

type environment = (var * tipe_scheme) list

let extend (env : environment) (v : var) (t : tipe_scheme) = 
  [(v, t)] @ env

let rec lookup (env : environment) (v : var) : tipe_scheme = 
  match env with
  | [] -> raise TypeError
  | (v', t')::re -> 
    if v' = v then t' else (lookup re v)


let rec find_variable (b : (string * tipe) list) (v : var) : tipe = 
  match b with
  | (v', t)::re -> (if v' = v then t else (find_variable re v))
  | _ -> (type_error "find variable")

let rec substitute (b : (string * tipe) list) (t : tipe) : tipe = 
  match t with
  | Tvar_t(str) -> find_variable b str
  | Int_t -> Int_t
  | Bool_t -> Bool_t
  | Unit_t -> Unit_t
  | Fn_t (t1, t2) -> Fn_t(substitute b t1, substitute b t2)
  | Pair_t (t1, t2) -> Pair_t(substitute b t1, substitute b t2)
  | List_t(t') -> List_t(substitute b t')
  | Guess_t(t') -> (
    match !t' with
    | Some t'' -> substitute b t''
    | None -> t
  )

let guess() = Guess_t(ref None)

let instantiate (s : tipe_scheme) : tipe = 
  match s with
  | Forall (vlst, t) -> 
    let b = List.map (fun a -> (a, guess())) vlst in
    substitute b t

let rec occurs (t1 : tipe) (t2 : tipe) : bool = 
  match t1 with
    | Guess_t (t1' : tipe option ref) -> (
      match t2 with
        | Guess_t (t2') -> (
          match !t2' with
            | Some tmp -> (if t1' == t2' then true else occurs t1 tmp)
            | None -> t1' == t2'
        )
        | Pair_t (t21, t22) -> (occurs t1 t21) || (occurs t1 t22)
        | Fn_t (t21, t22) -> (occurs t1 t21) || (occurs t1 t22)
        | List_t (t) -> (occurs t1 t)
        | _ -> false
    )
    | _ -> false

let rec occurs2 (t1 : tipe) (t2 : tipe) : bool = 
  match t2 with
  | Guess_t t2' ->
      (match !t2' with
      | None -> t1 == t2
      | Some t2'' -> occurs t1 t2'')
  | Fn_t (t21, t22) -> occurs t1 t21 || occurs t1 t22
  | Pair_t (t21, t22) -> occurs t1 t21 || occurs t1 t22
  | List_t t -> occurs t1 t
  | _ -> false

let rec unify (t1 : tipe) (t2 : tipe) : bool = 
  match t1, t2 with
    | Int_t, Int_t -> true
    | Bool_t, Bool_t -> true
    | Unit_t, Unit_t -> true
    | Fn_t(t11, t12), Fn_t(t21, t22) -> unify t11 t21 && unify t12 t22
    | Pair_t(t11, t12), Pair_t(t21, t22) -> unify t11 t21 && unify t12 t22
    | List_t(t11), List_t(t21) -> unify t11 t21
    | Guess_t (t1'), _ -> (
      match !t1' with
      | Some tmp -> unify tmp t2
      | None -> 
        (match t2 with
          | Guess_t(t2') -> if t2'==t1' then true else 
            (match !t2' with
              | None -> if t2'==t1' then true else (t1':=Some t2;true)
              | Some t2'' -> unify t2 t1)
          | _ -> if occurs t1 t2 then (type_error "unify") else (t1':=Some t2;true))
    )
    | _, Guess_t(_) -> unify t2 t1
    | _, _ -> raise TypeError


(* ================ *)

let rec find_var_ref (gx_vs:(tipe*var)list) (t:tipe):var option= 
  match t with 
  | Guess_t(t') -> 
    (match gx_vs with 
    | (Guess_t(gs'), fvar)::res -> if gs' == t' then Some fvar else (find_var_ref res t)
    | [] -> None
    | _ -> type_error "error type in gx_vs")
  | _ -> type_error "should be guess"

let rec subst_guess (gx_vs:(tipe*var)list) (t:tipe):tipe = 
  match t with 
  | Tvar_t(tv) -> Tvar_t(tv)
  | Int_t -> Int_t
  | Bool_t -> Bool_t
  | Unit_t -> Unit_t
  | Fn_t(t1,t2) -> Fn_t(subst_guess gx_vs t1,subst_guess gx_vs t2)
  | Pair_t(t1,t2) -> Pair_t(subst_guess gx_vs t1,subst_guess gx_vs t2)
  | List_t(t1) -> List_t(subst_guess gx_vs t1)
  | Guess_t(t1) -> 
    match !t1 with
    | Some t1' -> 
      (subst_guess gx_vs t1')
    | None -> 
      match find_var_ref gx_vs t with
      | Some tvar -> 
        Tvar_t(tvar)
      | None -> t

let rec guess_of_tipe t:tipe list=
  match t with 
  | Tvar_t(_)| Int_t| Bool_t| Unit_t -> []
  | Fn_t(t1,t2) -> (guess_of_tipe t1)@(guess_of_tipe t2)
  | Pair_t(t1,t2) -> (guess_of_tipe t1)@(guess_of_tipe t2)
  | List_t(t1) -> guess_of_tipe t1
  | Guess_t(t1) -> 
    match !t1 with
    | Some t1' -> guess_of_tipe t1'
    | None -> [t]

(*simulate set on guess list, cannot construct guess set because no compare rule*)
let rec check_exit a_list (element:tipe)=
  match element with
  | Guess_t(element') ->
    (match a_list with
    | [] -> false
    | Guess_t(a)::res -> if a==element' then true else check_exit res element
    | _ -> type_error "only guesses in the list but find other type")
  | _ -> type_error "only guesses in the list but find other type"

let rec union list1 list2 = 
  match list1 with
  | ele::res -> if check_exit list2 ele then union res list2 else ele::(union res list2)
  | [] -> list2

let rec minus list1 list2 =
  match list1 with
  | [] -> []
  | a::res -> if check_exit list2 a then 
    (minus res list2) else
    a::(minus res list2)

let rec guess_of_env s = 
  match s with
  | Forall(_,t) -> guess_of_tipe t

let generalize (e:environment) (t:tipe) : tipe_scheme =
  let t_gs = guess_of_tipe t in
  let env_list_gs =
    List.map (fun (x,s) -> guess_of_env s) e in
  let env_gs = List.fold_left union [] env_list_gs in
  let diff = minus t_gs env_gs in
  let gs_vs =
    List.map (fun g -> (g,ppfreshtvar())) diff in
  let tc = subst_guess gs_vs t
  in
  Forall(List.map snd gs_vs, tc) 

(* ============== *)

let rec tc (env : environment) ((e, _) : Mlish_ast.exp) : tipe = 
  match e with 
    | Var x -> instantiate (lookup env x)
    | Fn (x, exp) -> 
      let g = guess() in
      Fn_t(g, tc (extend env x (Forall([], g))) exp)
    | App (lmd, arg) -> 
      let (t1, t2, g) = (tc env lmd, tc env arg, guess()) in
      if unify t1 (Fn_t(t2, g)) then g else raise TypeError
    | If (e1, e2, e3) -> 
      let (t1, t2, t3) = (tc env e1, tc env e2, tc env e3) in
      if unify t1 Bool_t && unify t2 t3 then t2 else (type_error "if")
    | Let (arg, e1, e2) -> 
      let s = generalize env (tc env e1) in
      tc (extend env arg s) e2
    | PrimApp (p, elst) -> tc_PrimApp env p elst

and tc_arithmetic (env : environment) (elst : exp list) : bool = 
  match elst with
  | e1::e2::[] -> (
    match (tc env e1, tc env e2) with
    | t1, t2 -> (unify t1 Int_t) && (unify Int_t t2)
  )
  | _ -> (type_error "arithmetic")

and tc_PrimApp (env : environment) (p : prim) (elst : exp list) : tipe = 
  match p with
  | Int(i) -> Int_t
  | Bool(b) -> Bool_t
  | Unit -> Unit_t
  | Plus -> if tc_arithmetic env elst then Int_t else (type_error "plus")
  | Minus -> if tc_arithmetic env elst then Int_t else (type_error "minus")
  | Times -> if tc_arithmetic env elst then Int_t else (type_error "times")
  | Div -> if tc_arithmetic env elst then Int_t else (type_error "div")
  | Eq -> 
    (match elst with 
      | [e1; e2] -> let t1 = tc env e1 in let t2 = tc env e2 in
        if unify t1 t2 then Bool_t else (type_error "eq")
      | _ -> (type_error "eq"))
  | Lt -> if tc_arithmetic env elst then Bool_t else (type_error "lt")
  | Pair ->
    (match elst with
    | e1::e2::[] -> Pair_t(tc env e1, tc env e2)
    | _ -> (type_error "pair"))
  | Fst -> (
    match elst with
      | [e] ->
        let g1 = guess () in
        let g2 = guess () in
        let t = tc env e in
        if unify t (Pair_t(g1, g2)) then g1 else (type_error "fst")
      | _ -> (type_error "fst")
  )
  | Snd -> 
    (match elst with
    | [e] ->
      let g1 = guess () in
      let g2 = guess () in
      let t = tc env e in
      if unify t (Pair_t(g1, g2)) then g2 else (type_error "Snd")
    | _ -> (type_error "Snd"))
  | Nil -> let g = guess() in if elst=[] then List_t(g) else (type_error "Nil")
  | Cons -> (
    match elst with
    | [e1; e2] ->
      let t1 = tc env e1 in
      let t2 = tc env e2 in
      if unify t2 (List_t(t1)) then List_t(t1) else (type_error "Cons")
    | _ -> (type_error "Cons")
  )
  | IsNil -> (
    match elst with
    | [e] ->
      let g = guess () in
      let t = tc env e in
      if unify t (List_t(g)) then Bool_t else (type_error "IsNil")
    | _ -> (type_error "IsNil")
  )
  | Hd -> (
    match elst with
    | [e] ->
      let g = guess () in
      let t = tc env e in
      if unify t (List_t(g)) then g else (type_error "Hd")
    | _ -> (type_error "Hd")
  )
  | Tl -> (
    match elst with
    | [e] ->
      let g = guess () in
      let t = tc env e in
      if unify t (List_t(g)) then List_t(g) else (type_error "Tl")
    | _ -> (type_error "Tl")
  )

let type_check_exp (e:Mlish_ast.exp) : tipe = 
  tc [] e
