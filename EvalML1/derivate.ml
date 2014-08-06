open Types

let rec deriv e =
  match e with
  | Int x -> D (e, VInt x, EInt, [])
  | Bool b -> D (e, VBool b, EBool, [])
  | If (e1, e2, e3) ->
     let d1 = deriv e1 in
     begin
       match d1 with
         B _ -> failwith "deriv error"
       | D (_, VInt _, _, _) -> failwith "type error"
       | D (_, VBool b, _, _) ->
          let d2 = deriv (if b then e2 else e3) in
          begin
            match d2 with
              B _ -> failwith "deriv error"
            | D (_, v, _, _) -> 
               D (e, v, (if b then EIfT else EIfF), [d1; d2])
          end
     end
  | Binop (op, e1, e2) ->
     let d1 = deriv e1 in
     begin
       match d1 with
         B _ -> failwith "deriv error"
       | D (_, VBool _, _, _) -> failwith "type error"
       | D (_, VInt i1, _, _) ->
          let d2 = deriv e2 in
          begin
            match d2 with
              B _ -> failwith "deriv error"
            | D (_, VBool _, _, _) -> failwith "type error"
            | D (_, VInt i2, _, _) ->
               let v = match op with
                 | Plus -> VInt (i1 + i2)
                 | Minus -> VInt (i1 - i2)
                 | Times -> VInt (i1 * i2)
                 | Lt -> VBool (i1 < i2) in
               let d3 = B ((op, i1, i2), v, Base op) in
               D (e, v, Arith op, [d1; d2; d3])
          end
     end
