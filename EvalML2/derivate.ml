open Types

let rec infer_int env i = D (env, Int i, VInt i, EInt, [])

and infer_bool env b = D (env, Bool b, VBool b, EBool, [])

and infer_var env x =
  match env with
    [] -> failwith ("env: " ^ x)
  | (id, v) :: rest ->
     if id = x then D (env, Var x, v, EVar1, [])
     else let d = deriv rest (Var x) in
          begin
            match d with
              D (_, _, v, _, _) -> D (env, Var x, v, EVar2, [d])
            | _ -> failwith "deriv"
          end

and infer_if env (e1, e2, e3) =
  let d1 = deriv env e1 in
  match d1 with
    B _ -> failwith "deriv"
  | D (_, _, VInt _, _, _) -> failwith "type"
  | D (_, _, VBool b, _, _) ->
     let d2 = deriv env (if b then e2 else e3) in
     begin
       match d2 with
         B _ -> failwith "deriv"
       | D (_, _, v, _, _) -> 
          D (env, If (e1, e2, e3), v, (if b then EIfT else EIfF), [d1; d2])
     end

and infer_binop env (op, e1, e2) =
  let d1 = deriv env e1 in
  match d1 with
    B _ -> failwith "deriv"
  | D (_, _, VBool _, _, _) -> failwith "type"
  | D (_, _, VInt i1, _, _) ->
     let d2 = deriv env e2 in
     begin
       match d2 with
         B _ -> failwith "deriv"
       | D (_, _, VBool _, _, _) -> failwith "type"
       | D (_, _, VInt i2, _, _) ->
          let v = match op with
            | Plus -> VInt (i1 + i2)
            | Minus -> VInt (i1 - i2)
            | Times -> VInt (i1 * i2)
            | Lt -> VBool (i1 < i2) in
          let d3 = B ((op, i1, i2), v, Base op) in
          D (env, Binop (op, e1, e2), v, Arith op, [d1; d2; d3])
     end

and infer_let env (id, e1, e2) =
  let d1 = deriv env e1 in
  match d1 with
    D (_, _, v1, _, _) ->
    let d2 = deriv ((id, v1) :: env) e2 in
    begin
      match d2 with
        D (_, _, v2, _, _) -> D (env, Let (id, e1, e2), v2, ELet, [d1; d2])
      | _ -> failwith "deriv"
    end
  | _ -> failwith "deriv"

and deriv env e =
  match e with
  | Int i -> infer_int env i
  | Bool b -> infer_bool env b
  | Var x -> infer_var env x
  | If (e1, e2, e3) -> infer_if env (e1, e2, e3)
  | Binop (op, e1, e2) -> infer_binop env (op, e1, e2)
  | Let (id, e1, e2) -> infer_let env (id, e1, e2)
