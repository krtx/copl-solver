type op = Plus | Minus | Times | Lt

type judgement = exp * value
 and value = VInt of int | VBool of bool
 and exp = Int of int
         | Bool of bool
         | Binop of op * exp * exp
         | If of exp * exp * exp

type bexp = op * int * int

type rule = EInt | EBool | EIfT | EIfF
            | Arith of op
            | Base of op

type derivation = D of exp * value * rule * derivation list
                | B of bexp * value * rule
