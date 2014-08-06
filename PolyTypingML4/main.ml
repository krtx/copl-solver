
open Types

let rec main () =
  try
    let program = read_line () in
    let (env, ast, ty) = Parse.parser program in
    let (s, (D (_, _, t, _, _) as d)) = Derivate.deriv env ast in
    let s = Unification.unify ((ty, t) :: s) in
    let d = Derivate.subst_d s d in
    Print.print_derivation d;
    ignore (Unification.fresh (Some 3));
    main ()
  with End_of_file -> ()

let () = main ()
