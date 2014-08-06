open Types
open Format

let rec pp_list pp ppf = function
  | [] -> ()
  | [x] -> fprintf ppf "%a" pp x
  | x :: xs -> fprintf ppf "%a;@\n%a" pp x (pp_list pp) xs

let rec pp_exp ppf = function
  | Int i -> fprintf ppf "%d" i
  | Bool b -> fprintf ppf "%b" b
  | Binop (Plus, e1, e2) -> fprintf ppf "(%a) + (%a)" pp_exp e1 pp_exp e2
  | Binop (Minus, e1, e2) -> fprintf ppf "(%a) - (%a)" pp_exp e1 pp_exp e2
  | Binop (Times, e1, e2) -> fprintf ppf "(%a) * (%a)" pp_exp e1 pp_exp e2
  | Binop (Lt, e1, e2) -> fprintf ppf "(%a) < (%a)" pp_exp e1 pp_exp e2
  | If (e1, e2, e3) ->
     fprintf ppf "if %a then %a else %a" pp_exp e1 pp_exp e2 pp_exp e3

let pp_value ppf = function
  | VInt i -> fprintf ppf "%d" i
  | VBool b -> fprintf ppf "%b" b

let pp_rule ppf = function
  | EInt -> fprintf ppf "E-Int"
  | EBool -> fprintf ppf "E-Bool"
  | EIfT -> fprintf ppf "E-IfT"
  | EIfF -> fprintf ppf "E-IfF"
  | Arith Plus -> fprintf ppf "E-Plus"
  | Arith Minus -> fprintf ppf "E-Minus"
  | Arith Times -> fprintf ppf "E-Times"
  | Arith Lt -> fprintf ppf "E-Lt"
  | Base Plus -> fprintf ppf "B-Plus"
  | Base Minus -> fprintf ppf "B-Minus"
  | Base Times -> fprintf ppf "B-Times"
  | Base Lt -> fprintf ppf "B-Lt"

let pp_bexp ppf = function
  | Plus, i1, i2 -> fprintf ppf "%d plus %d" i1 i2
  | Minus, i1, i2 -> fprintf ppf "%d minus %d" i1 i2
  | Times, i1, i2 -> fprintf ppf "%d times %d" i1 i2
  | Lt, i1, i2 -> fprintf ppf "%d less than %d" i1 i2

let rec pp_derivation ppf = function
  | D (e, v, r, []) ->
     fprintf ppf
             "@[<2>%a evalto %a by %a {}@]"
             pp_exp e
             pp_value v
             pp_rule r
  | D (e, v, r, ds) ->
     fprintf ppf
             "@[<2>%a evalto %a by %a {@\n%a@]@\n}"
             pp_exp e
             pp_value v
             pp_rule r
             (pp_list pp_derivation) ds
  | B (e, v, r) ->
     fprintf ppf
             "@[<2>%a is %a by %a {}@]"
             pp_bexp e
             pp_value v
             pp_rule r
     
let print_derivation d =
  pp_derivation str_formatter d;
  printf "%s\n" (flush_str_formatter ())

