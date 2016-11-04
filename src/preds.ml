open Abduction

let _ = register_predicate "eq0" ["fp17"] (fun xs -> (List.hd xs) = "0");

print_endline "Predicates registered.";
