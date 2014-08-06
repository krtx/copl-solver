
open Types
open Unification

let rec subst_with_sc s tsc = function
  | PFun (t1, t2) -> PFun (subst_with_sc s tsc t1, subst_with_sc s tsc t2)
  | PList t -> PList (subst_with_sc s tsc t)
  | PVar x -> if List.mem x tsc then PVar x else subst (PVar x) s
  | _ as it -> it

let rec subst_env s = function
  | [] -> []
  | (x, (tc, t)) :: rest -> (x, (tc, subst_with_sc s tc t)) :: subst_env s rest

let rec subst_d s (D (env, e, t, r, ds)) =
  D (subst_env s env, e, subst t s, r, List.map (subst_d s) ds)

let rec infer_int env i  = ([], D (env, Int i, PInt, TInt, []))

and infer_bool env b = ([], D (env, Bool b, PBool, TBool, []))

and infer_var env x =

  let rec make_table = function
    | [] -> []
    | a :: rest -> (a, fresh None) :: make_table rest
  in

  let rec instanciate tbl = function
    | PVar a ->
       begin
         try PVar (List.assoc a tbl)
         with Not_found -> PVar a
       end
    | PList t -> PList (instanciate tbl t)
    | PFun (t1, t2) -> PFun (instanciate tbl t1, instanciate tbl t2)
    | _ as it -> it
  in
  
  let rec loc = function
    | (y, (tsc, t)) :: rest ->
       if x = y then
         ([], D (env, Var x, instanciate (make_table tsc) t, TVar, []))
       else loc rest
    | _ -> failwith ("variable not found: " ^ x)
  in

  loc env

and infer_if env (e1, e2, e3) =
  let s1, (D (_, _, t1, _, _) as d1) = deriv env e1
  and s2, (D (_, _, t2, _, _) as d2) = deriv env e2
  and s3, (D (_, _, t3, _, _) as d3) = deriv env e3 in
  let s = unify ((t1, PBool) :: (t2, t3) :: s1 @ s2 @ s3) in
  let ret = subst t2 s in
  (s, D (env, If (e1, e2, e3), ret, TIf, [d1; d2; d3]))

and infer_binop env (op, e1, e2) =
  let s1, (D (_, _, t1, _, _) as d1) = deriv env e1
  and s2, (D (_, _, t2, _, _) as d2) = deriv env e2 in
  let s = unify ((t1, PInt) :: (t2, PInt) :: s1 @ s2) in
  let ret = if op = Lt then PBool else PInt in
  (s, D (env, Binop (op, e1, e2), ret, T op, [d1; d2]))

and infer_let env (id, e1, e2) =
  let s1, (D (_, _, t1, _, _) as d1) = deriv env e1 in
  let tc =
    (sub (Unification.freevar t1)
         (Unification.freevar_env (subst_env s1 env)),
     t1) in
  let s2, (D (_, _, t2, _, _) as d2) = deriv ((id, tc) :: env) e2 in
  let s = unify (s1 @ s2) in
  let ret = subst t2 s in
  (s, D (env, Let (id, e1, e2), ret, TLet, [d1; d2]))

and infer_app env (e1, e2) =
  let s1, (D (_, _, tf, _, _) as d1) = deriv env e1 in
  let s2, (D (_, _, t, _, _) as d2) = deriv env e2 in
  let a = PVar (fresh None) in
  let s = unify ((tf, PFun (t, a)) :: s1 @ s2) in
  let ret = subst a s in
  (s, D (env, App (e1, e2), ret, TApp, [d1; d2]))

and infer_fun env (id, e) =
  let a = PVar (fresh None) in
  let s, (D (_, _, t, _, _) as d) = deriv ((id, ([], a)) :: env) e in
  let ret = PFun (subst a s, t) in
  (s, D (env, Fun (id, e), ret, TAbs, [d]))

and infer_letrec env (fn, id, e1, e2) =
  let a1 = PVar (fresh None) and a2 = PVar (fresh None) in
  let s1, (D (_, _, t1, _, _) as d1) =
    deriv ((id, ([], a2)) :: (fn, ([], a1)) :: env) e1 in
  let t = subst a1 s1 in
  let tc = (sub (Unification.freevar t)
                (Unification.freevar_env env), t) in
  let s2, (D (_, _, t2, _, _) as d2) = deriv ((fn, tc) :: env) e2 in
  let s = unify ((a1, PFun (a2, t1)) :: s1 @ s2) in
  let ret = subst t2 s in
  (s, D (env, LetRec (fn, id, e1, e2), ret, TLetRec, [d1; d2]))

and infer_nil env =
  let a = PVar (fresh None) in
  ([], D (env, Nil, PList a, TNil, []))

and infer_cons env (e1, e2) =
  let s1, (D (_, _, t1, _, _) as d1) = deriv env e1
  and s2, (D (_, _, t2, _, _) as d2) = deriv env e2 in
  let s = unify ((PList t1, t2) :: s1 @ s2) in
  (s, D (env, Cons (e1, e2), subst t2 s, TCons, [d1; d2]))

and infer_match env (e1, e2, x, y, e3) =
  let s1, (D (_, _, t1, _, _) as d1) = deriv env e1 in
  let s2, (D (_, _, t2, _, _) as d2) = deriv env e2 in
  let a = PVar (fresh None) in
  let s3, (D (_, _, t3, _, _) as d3) =
    deriv ((y, ([], PList a)) :: (x, ([], a)) :: env) e3 in
  let s = unify ((t2, t3) :: (PList a, t1) :: s1 @ s2 @ s3) in
  let ret = subst t2 s in
  (s, D (env, Match (e1, e2, x, y, e3), ret, TMatch, [d1; d2; d3]))

and deriv env e =
  match e with
  | Int i -> infer_int env i
  | Bool b -> infer_bool env b
  | Var x -> infer_var env x
  | If (e1, e2, e3) -> infer_if env (e1, e2, e3)
  | Binop (op, e1, e2) -> infer_binop env (op, e1, e2)
  | Let (id, e1, e2) -> infer_let env (id, e1, e2)
  | App (e1, e2) -> infer_app env (e1, e2)
  | Fun (id, e) -> infer_fun env (id, e)
  | LetRec (fn, id, e1, e2) -> infer_letrec env (fn, id, e1, e2)
  | Nil -> infer_nil env
  | Cons (e1, e2) -> infer_cons env (e1, e2)
  | Match (e1, e2, x, y, e3) -> infer_match env (e1, e2, x, y, e3)

