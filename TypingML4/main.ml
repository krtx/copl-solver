
open Types

let rec main () =
  try
    let program = read_line () in
    let (env, ast, ty) = Parse.parser program in
    let (s, d) = Derivate.deriv env ast ty in
    let d = Derivate.subst_d s d in
    let d = Derivate.fill_d d in
    Print.print_derivation d;
    main ()
  with End_of_file -> ()

let () = main ()
