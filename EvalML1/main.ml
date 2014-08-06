
let rec main () =
  let ast = Parse.parser (read_line ()) in
  Print.print_derivation (Derivate.deriv ast);
  main ()

let () = main ()
