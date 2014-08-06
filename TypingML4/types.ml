
type op = Plus | Minus | Times | Lt

type judgement = exp * ty
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
         | Nil
         | Cons of exp * exp
         | Match of exp * exp * var * var * exp

 and ty = PBool | PInt | PFun of ty * ty | PList of ty | PVar of int

and env = (var * ty) list

type rule = TInt
          | TBool
          | TIf
          | T of op
          | TVar
          | TLet
          | TFun
          | TApp
          | TLetRec
          | TNil
          | TCons
          | TMatch

type derivation = D of env * exp * ty * rule * derivation list

