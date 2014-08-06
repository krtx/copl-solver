
open MParser
open Tokens

(* let parens p = char '(' >> p << char ')' *)
let symbol1 s = string s << spaces1

type exp = Value of value
         | Binop of op * exp * exp
         | If of exp * exp * exp
and op = Plus | Minus | Times | Lt
and value = Int of int | Bool of bool
and judgement = exp * value

let infix p o =
  Infix (spaces >> p >> spaces |>> (fun _ a b -> (Binop (o, a, b))), Assoc_left)

let operators =
  [ [ infix (char '*') Times ];
    [ infix (char '+') Plus;
      infix (char '-') Minus ];
    [ infix (char '<') Lt ] ]

let int = integer |>> (fun i -> Int i)
let bool s = ((symbol1 "true"  >> return (Bool  true)) <|>
              (symbol1 "false" >> return (Bool false))) s
    
let rec exp s = expression operators term s
and term s = (Tokens.parens exp <|> (value |>> fun v -> Value v) <|> ifexp) s
and value s = (int <|> bool) s
and ifexp s = (
    (symbol1 "if")   >>
    exp              >>= fun e1 ->
    (symbol1 "then") >>
    exp              >>= fun e2 ->
    (symbol1 "else") >>
    exp              >>= fun e3 ->
    return (If (e1, e2, e3))
  ) s

let judgement : (judgement , unit) parser =
  exp >>= fun e -> (symbol1 "evalto") >> value >>= fun v ->
                   return (e, v)

let parser s = parse_string judgement s ()

(*
type exp = Value of value
         | Binop of op * exp * exp
         | If of exp * exp * exp
 *)

type rule =
    EInt   of int
  | EBool  of bool
  | EIfT   of exp * exp * exp * rule * rule
  | EIfF   of exp * exp * exp * rule * rule
  | EPlus  of exp * exp * int * rule * rule * rule
  | EMinus of exp * exp * int * rule * rule * rule
  | ETimes of exp * exp * int * rule * rule * rule
  | ELt    of exp * exp * bool * rule * rule * rule
  | BPlus  of int * int * int
  | BMinus of int * int * int
  | BTimes of int * int * int
  | BLt    of int * int * int

let rec deriv e = match e with
  | Value v ->
     begin
       match v with
       | Int i  -> EInt i
       | Bool b -> EBool b
     end
  | Binop (op, e1, e2) ->
     let d1 = deriv e1 and d2 = deriv e2 in
     begin
       match op with
       | Plus  -> 
       | Minus ->
       | Times ->
       | Lt    ->
     end
  | If (e1, e2, e3) ->
