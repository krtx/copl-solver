
open Types

exception Unify of string

let rec sub s1 s2 =
  match s1 with
  | [] -> []
  | a :: rest -> if List.mem a s2
                 then sub rest s2
                 else a :: sub rest s2

let rec remove_duplicate = function
  | [] -> []
  | a :: rest -> if List.mem a rest then remove_duplicate rest
                 else a :: remove_duplicate rest

let rec isfree a = function
  | PList t -> isfree a t
  | PVar b -> a != b
  | PFun (t1, t2) -> (isfree a t1) && (isfree a t2)
  | _ -> true

let rec freevar = function
  | PVar a -> [a]
  | PList t -> freevar t
  | PFun (t1, t2) -> remove_duplicate ((freevar t1) @ (freevar t2))
  | _ -> []

let rec freevar_env = function
  | [] -> []
  | (_, (tc, t)) :: rest ->
     remove_duplicate ((sub (freevar t) tc) @ freevar_env rest)

let rec subst_ty a ty = function
  | PList t -> PList (subst_ty a ty t)
  | PVar b -> if a = b then ty else PVar b
  | PFun (t1, t2) -> PFun (subst_ty a ty t1, subst_ty a ty t2)
  | _ as it -> it

let rec subst_eqs a ty = function
    [] -> []
  | (t1, t2) :: rest ->
     (subst_ty a ty t1, subst_ty a ty t2) ::
       (subst_eqs a ty rest)

let rec unify = function
    [] -> []
  | (t1, t2) :: rest when t1 = t2 -> unify rest
  | (PVar a, t) :: rest ->
     if not (isfree a t) then
       raise (Unify
                (Printf.sprintf "unification failed: bound1 %s, %s"
                                (Print.string_of_ty t)
                                (Print.string_of_ty (PVar a))))
     else (PVar a, t) :: unify (subst_eqs a t rest)
  | (t, PVar a) :: rest ->
     if not (isfree a t) then raise (Unify "unification failed: bound2")
     else (PVar a, t) :: unify (subst_eqs a t rest)
  | (PFun (t1, t2), PFun (t3, t4)) :: rest -> unify ((t2, t4) :: (t1, t3) :: rest)
  | (PList t1, PList t2) :: rest -> unify ((t1, t2) :: rest)
  | (t1, t2) :: _ ->
     raise
       (Unify
          ("unification failed: not compatible: " ^
             (Print.string_of_ty t1) ^ " and " ^ (Print.string_of_ty t2)))

let rec subst ty = function
    [] -> ty
  | (PVar a, t) :: rest -> subst (subst_ty a t ty) rest
  | _ -> failwith "unexpected"

let fresh =
  let counter = ref 3 in
  let body = function
    | None -> let v = !counter in
              counter := v + 1; v
    | Some x -> counter := x; x
  in body

