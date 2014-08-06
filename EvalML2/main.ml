
let rec main () =
  let (env, ast) = Parse.parser (read_line ()) in
  Print.print_derivation (Derivate.deriv env ast);
  main ()

let () = main ()
