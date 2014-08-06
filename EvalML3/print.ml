open Types
open Format

let rec pp_list pp ppf = function
  | [] -> ()
  | [x] -> fprintf ppf "%a" pp x
  | x :: xs -> fprintf ppf ("%a;@\n%a") pp x (pp_list pp) xs

let rec pp_exp ppf = function
  | Int i -> fprintf ppf "%d" i
  | Bool b -> fprintf ppf "%b" b
  | Var x -> fprintf ppf "%s" x
  | Binop (Plus, e1, e2) -> fprintf ppf "((%a) + (%a))" pp_exp e1 pp_exp e2
  | Binop (Minus, e1, e2) -> fprintf ppf "((%a) - (%a))" pp_exp e1 pp_exp e2
  | Binop (Times, e1, e2) -> fprintf ppf "((%a) * (%a))" pp_exp e1 pp_exp e2
  | Binop (Lt, e1, e2) -> fprintf ppf "((%a) < (%a))" pp_exp e1 pp_exp e2
  | If (e1, e2, e3) ->
     fprintf ppf "(if (%a) then (%a) else (%a))" pp_exp e1 pp_exp e2 pp_exp e3
  | Let (x, e1, e2) ->
     fprintf ppf "(let %s = (%a) in (%a))" x pp_exp e1 pp_exp e2
  | Fun (id, e) ->
     fprintf ppf "(fun %s -> (%a))" id pp_exp e
  | App (e1, e2) ->
     fprintf ppf "((%a) (%a))" pp_exp e1 pp_exp e2
  | LetRec (fn, id, e1, e2) ->
     fprintf ppf "let rec %s = fun %s -> (%a) in (%a)" fn id pp_exp e1 pp_exp e2

and pp_value ppf = function
  | VInt i -> fprintf ppf "%d" i
  | VBool b -> fprintf ppf "%b" b
  | Closure (env, id, e) -> fprintf ppf "(%a)[fun %s -> %a]" pp_env env id pp_exp e
  | ReClosure (env, fn, id, e) ->
     fprintf ppf "(%a)[rec %s = fun %s -> %a]" pp_env env fn id pp_exp e

and pp_rule ppf = function
  | EInt -> fprintf ppf "E-Int"
  | EBool -> fprintf ppf "E-Bool"
  | EIfT -> fprintf ppf "E-IfT"
  | EIfF -> fprintf ppf "E-IfF"
  | EVar1 -> fprintf ppf "E-Var1"
  | EVar2 -> fprintf ppf "E-Var2"
  | ELet -> fprintf ppf "E-Let"
  | EFun -> fprintf ppf "E-Fun"
  | EApp -> fprintf ppf "E-App"
  | ELetRec -> fprintf ppf "E-LetRec"
  | EAppRec -> fprintf ppf "E-AppRec"
  | Arith Plus -> fprintf ppf "E-Plus"
  | Arith Minus -> fprintf ppf "E-Minus"
  | Arith Times -> fprintf ppf "E-Times"
  | Arith Lt -> fprintf ppf "E-Lt"
  | Base Plus -> fprintf ppf "B-Plus"
  | Base Minus -> fprintf ppf "B-Minus"
  | Base Times -> fprintf ppf "B-Times"
  | Base Lt -> fprintf ppf "B-Lt"

and pp_bexp ppf = function
  | Plus, i1, i2 -> fprintf ppf "%d plus %d" i1 i2
  | Minus, i1, i2 -> fprintf ppf "%d minus %d" i1 i2
  | Times, i1, i2 -> fprintf ppf "%d times %d" i1 i2
  | Lt, i1, i2 -> fprintf ppf "%d less than %d" i1 i2

and pp_env ppf =
  let pp_tuple ppf (id, v) = fprintf ppf "%s = %a" id pp_value v in
  function
  | [] -> ()
  | [x] -> fprintf ppf "%a" pp_tuple x
  | x :: xs -> fprintf ppf ("%a, %a") pp_env xs pp_tuple x

let rec pp_derivation ppf = function
  | Dd d -> pp_dderivation ppf d
  | Bd d -> pp_bderivation ppf d

and pp_dderivation ppf (env, e, v, r, ds) =
  match env, ds with
    [], [] -> fprintf ppf
                      "@[<2>|- %a evalto %a by %a {}@]"
                      pp_exp e pp_value v pp_rule r
  | [], _ -> fprintf ppf
                     "@[<2>|- %a evalto %a by %a {@\n%a@]@\n}"
                     pp_exp e pp_value v pp_rule r
                     (pp_list pp_derivation) ds
  | _, [] -> fprintf ppf
                  "@[<2>%a |- %a evalto %a by %a {}@]"
                  pp_env env pp_exp e pp_value v pp_rule r
  | _ -> fprintf ppf
                 "@[<2>%a |- %a evalto %a by %a {@\n%a@]@\n}"
                 pp_env env pp_exp e pp_value v pp_rule r
                 (pp_list pp_derivation) ds
                 
and pp_bderivation ppf = function
  | (e, v, r) ->
     fprintf ppf
             "@[<2>%a is %a by %a {}@]"
             pp_bexp e
             pp_value v
             pp_rule r
     
let print_derivation d =
  pp_derivation str_formatter d;
  printf "%s\n" (flush_str_formatter ())

