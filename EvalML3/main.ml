
open Types

let rec main () =
  try
    let program = read_line () in
    let (env, ast) = Parse.parser program in
    Print.print_derivation (Dd (Derivate.deriv env ast));
    main ()
  with End_of_file -> ()

let () = main ()
