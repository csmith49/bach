open Abduction

let _ = register_predicate "eq0" ["fp17"] (fun xs -> (List.hd xs) = "0")
let _ = register_predicate "true" ["bool"] (fun xs -> (List.hd xs) = "True")
let _ = register_predicate "false" ["bool"] (fun xs -> (List.hd xs) = "False")

let _ = print_endline "Predicates registered."
