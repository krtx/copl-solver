
open Types
open Unification

let rec infer_int env i ty =
  let s = unify [(ty, PInt)] in
  (s, D (env, Int i, PInt, TInt, []))

and infer_bool env b ty =
  let s = unify [(ty, PBool)] in
  (s, D (env, Bool b, PBool, TBool, []))

and infer_var env x ty =
  let rec loc = function
    (y, t) :: rest -> if x = y then
                        let s = unify [(ty, t)] in
                        (s, D (env, Var x, t, TVar, []))
                      else loc rest
    | _ -> failwith ("variable not found: " ^ x)
  in loc env

and infer_if env (e1, e2, e3) ty =
  let s1, (D (_, _, t1, _, _) as d1) = deriv env e1 PBool
  and s2, (D (_, _, t2, _, _) as d2) = deriv env e2 ty
  and s3, (D (_, _, t3, _, _) as d3) = deriv env e3 ty in
  let s = unify ((t1, PBool) :: (t2, ty) :: (t3, ty) :: s1 @ s2 @ s3) in
  let ret = subst ty s in
  (s, D (env, If (e1, e2, e3), ret, TIf, [d1; d2; d3]))

and infer_binop env (op, e1, e2) ty =
  let s1, (D (_, _, t1, _, _) as d1) = deriv env e1 PInt
  and s2, (D (_, _, t2, _, _) as d2) = deriv env e2 PInt in
  if op = Lt then
    let s = unify ((ty, PBool) :: (t1, PInt) :: (t2, PInt) :: s1 @ s2) in
    (s, D (env, Binop (op, e1, e2), PBool, T op, [d1; d2]))
  else
    let s = unify ((ty, PInt) :: (t1, PInt) :: (t2, PInt) :: s1 @ s2) in
    (s, D (env, Binop (op, e1, e2), PInt, T op, [d1; d2]))

and infer_let env (id, e1, e2) ty =
  let t = PVar (fresh ()) in
  let s1, (D (_, _, t1, _, _) as d1) = deriv env e1 t in
  let s2, (D (_, _, t2, _, _) as d2) = deriv ((id, t1) :: env) e2 ty in
  let s = unify ((t, t1) :: s1 @ s2) in
  let ret = subst ty s in
  (s, D (env, Let (id, e1, e2), ret, TLet, [d1; d2]))

and infer_app env (e1, e2) ty =
  let t1 = PVar (fresh ()) in
  let s1, (D (_, _, tf, _, _) as d1) = deriv env e1 (PFun (t1, ty)) in
  let s2, (D (_, _, t, _, _) as d2) = deriv env e2 t1 in
  let s = unify ((tf, PFun (t1, ty)) :: (t, t1) :: s1 @ s2) in
  (s, D (env, App (e1, e2), subst ty s, TApp, [d1; d2]))

and infer_fun env (id, e) ty =
  let a = PVar (fresh ())
  and b = PVar (fresh ()) in
  let s, (D (_, _, t, _, _) as d) = deriv ((id, a) :: env) e b in
  let s = unify ((ty, PFun (a, b)) :: s) in
  (s, D (env, Fun (id, e), subst ty s, TFun, [d]))

and infer_letrec env (fn, id, e1, e2) ty =
  let a1 = PVar (fresh ())
  and a2 = PVar (fresh ()) in
  let s1, (D (_, _, t2, _, _) as d1) =
    deriv ((id, a1) :: (fn, PFun (a1, a2)) :: env) e1 a2 in
  let s2, (D (_, _, t, _, _) as d2) =
    deriv ((fn, PFun (a1, a2)) :: env) e2 ty in
  let s = unify ((a2, t2) :: s1 @ s2) in
  (s, D (env, LetRec (fn, id, e1, e2), subst ty s, TLetRec, [d1; d2]))

and infer_nil env ty =
  let a = PVar (fresh ()) in
  let s = unify [(PList a, ty)] in
  (s, D (env, Nil, subst (PList a) s, TNil, []))
                      
and infer_cons env (e1, e2) ty =
  let a = PVar (fresh ()) in
  let s1, (D (_, _, t1, _, _) as d1) = deriv env e1 a
  and s2, (D (_, _, t2, _, _) as d2) = deriv env e2 (PList a) in
  let s = unify ((PList a, ty) :: (a, t1) :: (PList a, t2) :: s1 @ s2) in
  (s, D (env, Cons (e1, e2), subst (PList a) s, TCons, [d1; d2]))

and infer_match env (e1, e2, x, y, e3) ty =
  let a = PVar (fresh ()) in
  let s1, (D (_, _, t1, _, _) as d1) = deriv env e1 (PList a) in
  let s2, (D (_, _, t2, _, _) as d2) = deriv env e2 ty in
  let s3, (D (_, _, t3, _, _) as d3) =
    deriv ((y, PList a) :: (x, a) :: env) e3 ty in
  let s = unify ((PList a, t1) :: (ty, t2) :: (ty, t3) :: s1 @ s2 @ s3) in
  (s, D (env, Match (e1, e2, x, y, e3), subst ty s, TMatch, [d1; d2; d3]))

and deriv env e ty =
  match e with
  | Int i -> infer_int env i ty
  | Bool b -> infer_bool env b ty
  | Var x -> infer_var env x ty
  | If (e1, e2, e3) -> infer_if env (e1, e2, e3) ty
  | Binop (op, e1, e2) -> infer_binop env (op, e1, e2) ty
  | Let (id, e1, e2) -> infer_let env (id, e1, e2) ty
  | App (e1, e2) -> infer_app env (e1, e2) ty
  | Fun (id, e) -> infer_fun env (id, e) ty
  | LetRec (fn, id, e1, e2) -> infer_letrec env (fn, id, e1, e2) ty
  | Nil -> infer_nil env ty
  | Cons (e1, e2) -> infer_cons env (e1, e2) ty
  | Match (e1, e2, x, y, e3) -> infer_match env (e1, e2, x, y, e3) ty

let rec subst_env s = function
  | [] -> []
  | (x, t) :: rest -> (x, subst t s) :: subst_env s rest

let rec subst_d s (D (env, e, t, r, ds)) =
  D (subst_env s env, e, subst t s, r, List.map (subst_d s) ds)

(* fill out type variables with fixed types *)
let rec fill_d (D (env, e, t, r, ds)) =
  let rec fill_ty = function
    | PVar a -> PInt
    | PList t -> PList (fill_ty t)
    | PFun (t1, t2) -> PFun (fill_ty t1, fill_ty t2)
    | _ as it -> it
  in
  let newenv = List.map (fun (x, ty) -> (x, fill_ty ty)) env in
  D (newenv, e, fill_ty t, r, List.map fill_d ds)
