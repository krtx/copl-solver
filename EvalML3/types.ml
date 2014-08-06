type op = Plus | Minus | Times | Lt

type judgement = exp * value
 and value = VInt of int
           | VBool of bool
           | Closure of env * var * exp
           | ReClosure of env * var * var * exp
 and var = string
 and exp = Int of int
         | Bool of bool
         | Var of var
         | Binop of op * exp * exp
         | If of exp * exp * exp
         | Let of var * exp * exp
         | Fun of var * exp
         | App of exp * exp
         | LetRec of var * var * exp * exp
and env = (var * value) list

type bexp = op * int * int

type rule = EInt
          | EBool
          | EIfT
          | EIfF
          | EVar1
          | EVar2
          | ELet
          | EFun
          | EApp
          | ELetRec
          | EAppRec
          | Arith of op
          | Base of op

type derivation = Dd of dderivation | Bd of bderivation
and dderivation = env * exp * value * rule * derivation list
and bderivation = bexp * value * rule

