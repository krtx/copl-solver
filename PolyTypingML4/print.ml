
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
  | Binop (Plus, e1, e2) -> fprintf ppf "%a + (%a)" pp_exp e1 pp_exp e2
  | Binop (Minus, e1, e2) -> fprintf ppf "%a - (%a)" pp_exp e1 pp_exp e2
  | Binop (Times, e1, e2) -> fprintf ppf "%a * (%a)" pp_exp e1 pp_exp e2
  | Binop (Lt, e1, e2) -> fprintf ppf "%a < (%a)" pp_exp e1 pp_exp e2
  | If (e1, e2, e3) ->
     fprintf ppf "if %a then %a else %a" pp_exp e1 pp_exp e2 pp_exp e3
  | Let (x, e1, e2) ->
     fprintf ppf "let %s = (%a) in (%a)" x pp_exp e1 pp_exp e2
  | Fun (id, e) ->
     fprintf ppf "fun %s -> (%a)" id pp_exp e
  | App (e1, e2) -> fprintf ppf "%a (%a)" pp_exp e1 pp_exp e2
  | LetRec (fn, id, e1, e2) ->
     fprintf ppf "let rec %s = fun %s -> (%a) in (%a)" fn id pp_exp e1 pp_exp e2
  | Nil -> fprintf ppf "[]"
  | Cons (e1, e2) ->
     begin
       match e1 with
       | Cons _ -> fprintf ppf "(%a) :: %a" pp_exp e1 pp_exp e2
       | _ -> fprintf ppf "%a :: %a" pp_exp e1 pp_exp e2
     end
  | Match (e1, e2, x, y, e3) ->
     fprintf ppf "@[<2>match %a with [] -> %a | %s :: %s -> %a@]"
             pp_exp e1 pp_exp e2 x y pp_exp e3

and pp_tvar ppf tv = fprintf ppf "'%c" (char_of_int (tv + 97))

and pp_ty ppf = function
  | PBool -> fprintf ppf "bool"
  | PInt -> fprintf ppf "int"
  | PFun (t1, t2) ->
     begin
       match t1 with
         PFun _ -> fprintf ppf "(%a) -> %a" pp_ty t1 pp_ty t2
       | _ -> fprintf ppf "%a -> %a" pp_ty t1 pp_ty t2
     end
  | PList t ->
     begin
       match t with
         PFun _ -> fprintf ppf "(%a) list" pp_ty t
       | _ -> fprintf ppf "%a list" pp_ty t
     end
  | PVar a -> fprintf ppf "%a" pp_tvar a

and pp_tysc ppf (vs, ty) =
  let rec pp_tvars ppf = 
  function
  | [] -> ()
  | [x] -> fprintf ppf "%a" pp_tvar x
  | x :: xs -> fprintf ppf "%a %a" pp_tvar x pp_tvars xs
  in
  match vs with
    [] -> fprintf ppf "%a" pp_ty ty
  | _ -> fprintf ppf "%a.%a" pp_tvars vs pp_ty ty

and pp_rule ppf = function
  | TInt -> fprintf ppf "T-Int"
  | TBool -> fprintf ppf "T-Bool"
  | TIf -> fprintf ppf "T-If"
  | TVar -> fprintf ppf "T-Var"
  | TLet -> fprintf ppf "T-Let"
  | TAbs -> fprintf ppf "T-Abs"
  | TApp -> fprintf ppf "T-App"
  | TLetRec -> fprintf ppf "T-LetRec"
  | TNil -> fprintf ppf "T-Nil"
  | TCons -> fprintf ppf "T-Cons"
  | TMatch -> fprintf ppf "T-Match"
  | T Plus -> fprintf ppf "T-Plus"
  | T Minus -> fprintf ppf "T-Minus"
  | T Times -> fprintf ppf "T-Mult"
  | T Lt -> fprintf ppf "T-Lt"

and pp_env ppf =
  let pp_tuple ppf (id, tc) = fprintf ppf "%s : %a" id pp_tysc tc in
  function
  | [] -> ()
  | [x] -> fprintf ppf "%a" pp_tuple x
  | x :: xs -> fprintf ppf "%a, %a" pp_env xs pp_tuple x

let rec pp_derivation ppf (D (env, e, t, r, ds)) =
  match env, ds with
    [], [] -> fprintf ppf
                      "@[<2>|- %a : %a by %a {}@]"
                      pp_exp e pp_ty t pp_rule r
  | [], _ -> fprintf ppf
                     "@[<2>|- %a : %a by %a {@\n%a@]@\n}"
                     pp_exp e pp_ty t pp_rule r
                     (pp_list pp_derivation) ds
  | _, [] -> fprintf ppf
                  "@[<2>%a |- %a : %a by %a {}@]"
                  pp_env env pp_exp e pp_ty t pp_rule r
  | _ -> fprintf ppf
                 "@[<2>%a |- %a : %a by %a {@\n%a@]@\n}"
                 pp_env env pp_exp e pp_ty t pp_rule r
                 (pp_list pp_derivation) ds
     
let print_derivation d =
  pp_derivation str_formatter d;
  printf "%s\n" (flush_str_formatter ())

let print_exp e =
  pp_exp str_formatter e;
  printf "%s\n" (flush_str_formatter ())

let print_ty t =
  pp_ty str_formatter t;
  printf "%s\n" (flush_str_formatter ())

let string_of_ty t =
  pp_ty str_formatter t;
  flush_str_formatter ()

let rec pp_subst ppf =
  let pp_tuple ppf (t1, t2) = fprintf ppf "(%a, %a)" pp_ty t1 pp_ty t2 in
  function
  | [] -> ()
  | [x] -> fprintf ppf "%a" pp_tuple x
  | x :: xs -> fprintf ppf "%a; %a" pp_subst xs pp_tuple x

let print_subst s =
  pp_subst str_formatter s;
  printf "%s\n" (flush_str_formatter ())
