type op = Plus | Minus | Times | Lt

type judgement = exp * value
 and value = VInt of int | VBool of bool
 and var = string
 and exp = Int of int
         | Bool of bool
         | Var of var
         | Binop of op * exp * exp
         | If of exp * exp * exp
         | Let of var * exp * exp

type env = (var * value) list

type bexp = op * int * int

type rule = EInt | EBool | EIfT | EIfF
            | EVar1 | EVar2 | ELet
            | Arith of op
            | Base of op

type derivation = D of env * exp * value * rule * derivation list
                | B of bexp * value * rule
