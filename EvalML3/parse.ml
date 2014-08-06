
open Types
open MParser

let keywords = ["if"; "then"; "else"; "let"; "in"; "fun"; "rec"]

let infix p o = p |>> (fun _ a b -> (Binop (o, a, b)))

let operators =
  let cop c = char c << spaces in (* >> *)
  [ infix (cop '*') Times;
    infix (cop '+') Plus; infix (cop '-') Minus;
    infix (cop '<') Lt ]                 

let int = Tokens.integer |>> (fun i -> i) << spaces (* >> *)
let eint = int |>> fun i -> Int i
let vint = int |>> fun i -> VInt i

let bool = (Tokens.symbol "true" >> return true) <|> (Tokens.symbol "false" >> return false)
let ebool = bool |>> fun b -> Bool b
let vbool = bool |>> fun b -> VBool b

let id_string = regexp (make_regexp "[A-Za-z_][A-Za-z0-9_]*") << spaces (* >> *)
let var =
  id_string >>= fun id -> if List.mem id keywords
                          then fail "keywords"
                          else return (Var id)

let rec exp s =
  (choice [ifte; fune; attempt letrec; lete; attempt binop; attempt app; sexp]) s

and sexp s = (Tokens.parens exp <|> eint <|> ebool <|> var) s
and app s =
  (
    sexp >>= fun e1 ->
    spaces >>
    many1_fold_left (fun x y -> App (x, y)) e1 (attempt sexp)
  ) s

and binop s =
  ((attempt app <|> sexp) >>= fun e1 ->
   choice operators >>= fun f ->
   exp >>= fun e2 ->
   return (f e1 e2)) s

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
and fune s =
  (
    (Tokens.symbol "fun") >>
    id_string >>= fun id ->
    (Tokens.symbol "->") >>
    exp >>= fun e ->
    return (Fun (id, e))
  ) s

and letrec s =
  (
    (Tokens.symbol "let") >>
    (Tokens.symbol "rec") >>
    id_string >>= fun fn ->
    (Tokens.symbol "=") >>
    (Tokens.symbol "fun") >>
    id_string >>= fun id ->
    (Tokens.symbol "->") >>
    exp >>= fun e1 ->
    (Tokens.symbol "in") >>
    exp >>= fun e2 ->
    return (LetRec (fn, id, e1, e2))
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
    eof >>
    return (List.rev env, e)
  ) s

exception Syntax_error

let parser s =
  match parse_string ee s () with
    Success e -> e
  | Failed (msg, e) -> print_string msg; raise Syntax_error
