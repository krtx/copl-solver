
open Types
open MParser

let infix p o =
  Infix (p |>> (fun _ a b -> (Binop (o, a, b))), Assoc_left)

let operators =
  let cop c = char c << spaces in
  [ [ infix (cop '*') Times ];
    [ infix (cop '+') Plus;
      infix (cop '-') Minus ];
    [ infix (cop '<') Lt ] ]

let int = Tokens.integer |>> (fun i -> i) << spaces
let eint = int |>> fun i -> Int i
let vint = int |>> fun i -> VInt i

let bool = (Tokens.symbol "true" >> return true) <|>
           (Tokens.symbol "false" >> return false)
let ebool = bool |>> fun b -> Bool b
let vbool = bool |>> fun b -> VBool b

let id_string = regexp (make_regexp "[A-Za-z_][A-Za-z0-9_]*") << spaces
let var = id_string |>> fun id -> Var id

let rec term s =
  (Tokens.parens exp <|> lete <|> ifte <|> eint <|> ebool <|> var) s
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
and lete s =
  (
    (Tokens.symbol "let") >>
    id_string >>= fun id ->
    (Tokens.symbol "=") >>
    exp >>= fun e1 ->
    (Tokens.symbol "in") >>
    exp >>= fun e2 ->
    return (Let (id, e1, e2))
  ) s

let env s =
  let tuple =
    id_string >>= fun id ->
    Tokens.symbol "=" >>
    (vint <|> vbool) >>= fun v ->
    return (id, v)
  in
  sep_by tuple (Tokens.symbol ",") s

let ee s =
  (
    env >>= fun env ->
    Tokens.symbol "|-" >>
    exp >>= fun e ->
    return (List.rev env, e)
  ) s

exception Syntax_error

let parser s =
  match parse_string ee s () with
    Success e -> e
  | Failed (msg, e) -> print_string msg; raise Syntax_error
