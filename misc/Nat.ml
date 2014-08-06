open Format

type judgement = expr * value
 and value = nat
 and expr = Plus of nat * nat
          | Times of nat * nat
 and nat = Z | S of nat

type rule = PZero | PSucc | TZero | TSucc

type derivation = D of value * expr * rule * derivation list

let rec deriv e =
  match e with
  | Plus (Z, n) -> D (n, e, PZero, [])
  | Plus (S n1, n2) ->
     let D (n, _, _, _) as d = deriv (Plus (n1, n2))
     in D (S n, e, PSucc, [d])
  | Times (Z, n) -> D (Z, e, TZero, [])
  | Times (S n1, n2) ->
     let D (n3, _, _, _) as d1 = deriv (Times (n1, n2)) in
     let D (n4, _, _, _) as d2 = deriv (Plus (n2, n3)) in
     D (n4, e, TSucc, [d1; d2])

let rec pp_list pp ppf = function
  | [] -> ()
  | [x] -> fprintf ppf "%a" pp x
  | x :: xs -> fprintf ppf "%a;@\n%a" pp x (pp_list pp) xs

let rec pp_derivation ppf =
  function
    D (v, e, r, []) ->
    fprintf ppf
            "@[<2>%a is %a by %a {}@]"
            pp_expr e
            pp_value v
            pp_rule r
  | D (v, e, r, ds) ->
     fprintf ppf
             "@[<2>%a is %a by %a {@\n%a@]@\n}"
             pp_expr e
             pp_value v
             pp_rule r
             (pp_list pp_derivation) ds
and pp_expr ppf =
  function
  | Plus (x, y) -> fprintf ppf "%a plus %a" pp_value x pp_value y
  | Times (x, y) -> fprintf ppf "%a times %a" pp_value x pp_value y
and pp_value ppf =
  function
  | Z -> fprintf ppf "Z"
  | S x -> fprintf ppf "S (%a)" pp_value x
and pp_rule ppf =
  function
  | PZero -> fprintf ppf "PZero"
  | PSucc -> fprintf ppf "PSucc"
  | TZero -> fprintf ppf "TZero"
  | TSucc -> fprintf ppf "TSucc"
