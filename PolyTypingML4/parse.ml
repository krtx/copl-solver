
open Types
open MParser

let keywords = ["if"; "then"; "else"; "let"; "in"; "fun"; "rec"; "match"; "with"]

let infix p o =
  Infix (p |>> (fun _ a b -> (Binop (o, a, b))), Assoc_left)

let cons_op =
  Infix (Tokens.symbol "::" |>> (fun _ a b -> (Cons (a, b))), Assoc_right)

let operators =
  [ [ infix (Tokens.symbol "*") Times ];
    [ infix (Tokens.symbol "+") Plus;
      infix (Tokens.symbol "-") Minus ];
    [ infix (Tokens.symbol "<") Lt ];
    [ cons_op ] ]

let int = Tokens.integer |>> (fun i -> i) << spaces (* >> *)
let eint = int |>> fun i -> Int i

let bool = (Tokens.symbol "true" >> return true) <|>
           (Tokens.symbol "false" >> return false)
let ebool = bool |>> fun b -> Bool b

let nil s = (Tokens.symbol "[]" >> return Nil << spaces) s (* >> *)

let id_string = regexp (make_regexp "[A-Za-z_][A-Za-z0-9_]*") << spaces (* >> *)
let var =
  id_string >>= fun id -> if List.mem id keywords
                          then fail "keywords"
                          else return (Var id)

let rec exp s =
  (choice [ifte;
           fune;
           attempt letrec;
           lete;
           matche;
           attempt binop;
           attempt app;
           sexp]) s

and sexp s = (Tokens.parens exp <|> eint <|> ebool <|> var <|> nil) s
and app s =
  (
    sexp >>= fun e1 ->
    spaces >>
    many1_fold_left (fun x y -> App (x, y)) e1 (attempt sexp)
  ) s

and binop s = expression operators (attempt app <|> attempt sexp <|> exp) s

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

and matche s =
  (
    (Tokens.symbol "match") >>
    exp >>= fun e1 ->
    (Tokens.symbol "with") >>
    nil >>
    (Tokens.symbol "->") >>
    exp >>= fun e2 ->
    (Tokens.symbol "|") >>
    id_string >>= fun x ->
    (Tokens.symbol "::") >>
    id_string >>= fun y ->
    (Tokens.symbol "->") >>
    exp >>= fun e3 ->
    return (Match (e1, e2, x, y, e3))
  ) s

let ty_bool = Tokens.symbol "bool" >> return PBool
let ty_int = Tokens.symbol "int" >> return PInt

let arrow = Infix (Tokens.symbol "->" |>> (fun _ a b -> PFun (a, b)), Assoc_right)
let list = Postfix (Tokens.symbol "list" |>> (fun _ a -> PList a))

let ty_var =
  char '\'' >> regexp (make_regexp "[a-z]")
  |>> (fun c -> (int_of_char c.[0]) - (int_of_char 'a')) << spaces (* >> *)

let rec ty s =
  expression [[list]; [arrow]]
             (Tokens.parens ty <|>
                ty_bool <|>
                ty_int <|>
                (ty_var |>> fun x -> PVar x)) s

let tysc s =
  (
    opt [] (attempt
              (many ty_var >>= fun tvs -> Tokens.symbol "." >> return tvs))
    >>= fun tvs ->
    ty >>= fun t ->
    return (tvs, t)
  ) s

let env s =
  let tuple =
    id_string >>= fun id ->
    Tokens.symbol ":" >>
    tysc >>= fun t ->
    return (id, t)
  in
  sep_by tuple (Tokens.symbol ",") s

let ee s =
  (
    env >>= fun env ->
    Tokens.symbol "|-" >>
    exp >>= fun e ->
    Tokens.symbol ":" >>
    ty >>= fun t ->
    eof >>
    return (List.rev env, e, t)
  ) s

exception Syntax_error

let parser s =
  match parse_string ee s () with
    Success e -> e
  | Failed (msg, e) -> print_string msg; raise Syntax_error
