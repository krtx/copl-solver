
open Types
open MParser

let infix p o =
  Infix (p |>> (fun _ a b -> (Binop (o, a, b))), Assoc_left)

let operators =
  let cop c = Tokens.symbol s
  [ [ infix (Tokens.symbol "*") Times ];
    [ infix (Tokens.symbol "+") Plus;
      infix (Tokens.symbol "-") Minus ];
    [ infix (Tokens.symbol "<") Lt ] ]

let int = Tokens.integer |>> (fun i -> Int i) << spaces (* >> *)

let bool = (Tokens.symbol "true" >> return (Bool true)) <|>
           (Tokens.symbol "false" >> return (Bool false))

let rec term s = (Tokens.parens exp <|> int <|> bool <|> ifte) s
and exp s = expression operators term s
and ifte s =
  (
  (Tokens.symbol "if") >>
  exp >>= fun e1 ->
  (Tokens.symbol "then") >>
  exp >>= fun e2 ->
  (Tokens.symbol "else") >>
  exp >>= fun e3 ->
  return (If (e1, e2, e3))
  ) s                             

exception Syntax_error

let parser s =
  match parse_string exp s () with
    Success e -> e
  | Failed (msg, e) -> print_string msg; raise Syntax_error
