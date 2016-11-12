open Abduction

let _ = register_predicate "eq0" ["fp17"] (fun xs -> (List.hd xs) = "0")
let _ = register_predicate "true" ["bool"] (fun xs -> (List.hd xs) = "True")
let _ = register_predicate "false" ["bool"] (fun xs -> (List.hd xs) = "False")
let _ = register_predicate "err-tensor" ["tensor"] (fun xs -> (List.hd xs) = "Error")
let _ = register_predicate "identity" ["tensor"] (fun xs -> (List.hd xs) = "(1, 0, 0, 1)")
let _ = register_predicate "gte-1" ["int"] (fun xs -> (int_of_string (List.hd xs)) >= -1)
let _ = register_predicate "gt" ["int";"int"] (fun xs -> (int_of_string (List.nth xs 0)) > (int_of_string (List.nth xs 1)) )
let _ = register_predicate "lte1" ["int"] (fun xs -> (int_of_string (List.hd xs)) <= 1)
let _ = register_predicate "invr" ["rad";"rad"] (fun xs -> (int_of_string (List.nth xs 0)) = -1*(int_of_string (List.nth xs 1)))
let _ = register_predicate "invi" ["int";"int"] (fun xs -> (int_of_string (List.nth xs 0)) = -1*(int_of_string (List.nth xs 1)))
let _ = register_predicate "m1" ["rad"; "rad"] (

    fun xs ->
    (*fun xs -> if (((List.nth xs 0) = "Error") ||  ((List.nth xs 1) = "Error")) then
        true
    else*)
        let x = (int_of_string (List.nth xs 0)) in
        let y = (int_of_string (List.nth xs 1)) in
        (x - y) mod 2 = 0
)

let _ = print_endline "Predicates registered."
