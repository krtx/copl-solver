open Types

let bderiv exp =
  (fun (op, i1, i2) ->
   match op with
     Plus -> (exp, VInt (i1 + i2), Base Plus)
   | Minus -> (exp, VInt (i1 - i2), Base Minus)
   | Times -> (exp, VInt (i1 * i2), Base Times)
   | Lt -> (exp, VBool (i1 < i2), Base Lt))
    exp

let rec infer_int env i = (env, Int i, VInt i, EInt, [])

and infer_bool env b = (env, Bool b, VBool b, EBool, [])

and infer_var env x  =
  match env with
    [] -> failwith ("env: " ^ x)
  | (id, v) :: rest ->
     if id = x then (env, Var x, v, EVar1, [])
     else let (_, _, v, _, _) as d = deriv rest (Var x) in
          (env, Var x, v, EVar2, [Dd d])

and infer_if env (e1, e2, e3) =
  let d1 = deriv env e1 in
  match d1 with
  | (_, _, VBool b, _, _) ->
     let (_, _, v, _, _) as d2 = deriv env (if b then e2 else e3) in
     (env, If (e1, e2, e3), v, (if b then EIfT else EIfF), [Dd d1; Dd d2])
  | _ -> failwith "type"

and infer_binop env (op, e1, e2) =
  let d1 = deriv env e1 in
  match d1 with
  | (_, _, VInt i1, _, _) ->
     let d2 = deriv env e2 in
     begin
       match d2 with
       | (_, _, VInt i2, _, _) ->
          let v = match op with
            | Plus -> VInt (i1 + i2)
            | Minus -> VInt (i1 - i2)
            | Times -> VInt (i1 * i2)
            | Lt -> VBool (i1 < i2) in
          let d3 = bderiv (op, i1, i2) in
          (env, Binop (op, e1, e2), v, Arith op, [Dd d1; Dd d2; Bd d3])
       | _ -> failwith "type"
     end
  | _ -> failwith "type"

and infer_let env (id, e1, e2) =
  let (_, _, v1, _, _) as d1 = deriv env e1 in
  let (_, _, v2, _, _) as d2 = deriv ((id, v1) :: env) e2 in
  (env, Let (id, e1, e2), v2, ELet, [Dd d1; Dd d2])

and infer_app env (e1, e2) =
  let (_, _, v2, _, _) as d2 = deriv env e2 in
  let (_, _, vf, _, _) as d1 = deriv env e1 in
  match vf with
  | Closure (env2, id, e0) ->
     let (_, _, v, _, _) as d3 = deriv ((id, v2) :: env2) e0 in
     (env, App (e1, e2), v, EApp, [Dd d1; Dd d2; Dd d3])
  | ReClosure (env2, fn, id, e0) ->
     let (_, _, v, _, _) as d3 =
       deriv ((id, v2) :: (fn, ReClosure (env2, fn, id, e0)) :: env2) e0 in
     (env, App (e1, e2), v, EAppRec, [Dd d1; Dd d2; Dd d3])
  | _ -> failwith "type"

and infer_fun env (id, e) =
  (env, Fun (id, e), Closure (env, id, e), EFun, [])

and infer_letrec env (fn, id, e1, e2) =
  let (_, _, v, _, _) as d = deriv ((fn, ReClosure (env, fn, id, e1)) :: env) e2 in
  (env, LetRec (fn, id, e1, e2), v, ELetRec, [Dd d]);

and deriv env e =
  match e with
  | Int i -> infer_int env i
  | Bool b -> infer_bool env b
  | Var x -> infer_var env x
  | If (e1, e2, e3) -> infer_if env (e1, e2, e3)
  | Binop (op, e1, e2) -> infer_binop env (op, e1, e2)
  | Let (id, e1, e2) -> infer_let env (id, e1, e2)
  | App (e1, e2) -> infer_app env (e1, e2)
  | Fun (id, e) -> infer_fun env (id, e)
  | LetRec (fn, id, e1, e2) -> infer_letrec env (fn, id, e1, e2)
