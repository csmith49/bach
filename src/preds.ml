open Abduction

let _ = register_predicate "eq0" ["fp17"] (fun xs -> (List.hd xs) = "0")
let _ = register_predicate "true" ["bool"] (fun xs -> (List.hd xs) = "True")
let _ = register_predicate "false" ["bool"] (fun xs -> (List.hd xs) = "False")
let _ = register_predicate "error" ["tensor"] (fun xs -> (List.hd xs) = "Error")
let _ = register_predicate "m1" ["rad"; "rad"] (

    fun xs ->
    (*fun xs -> if (((List.nth xs 0) = "Error") ||  ((List.nth xs 1) = "Error")) then
        true
    else*)
        let x = (int_of_string (List.nth xs 0)) in
        let y = (int_of_string (List.nth xs 1)) in
        (x - y) mod 2 = 0
)
