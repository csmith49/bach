open Abduction

let _ = register_predicate "eq0" ["fp17"] (fun xs -> (List.hd xs) = "0")
let _ = register_predicate "true" ["bool"] (fun xs -> (List.hd xs) = "True")
let _ = register_predicate "identity" ["tensor"] (fun xs -> (List.hd xs) = "(1, 0, 0, 1)")
let _ = register_predicate "gt" ["int";"int"] (fun xs -> (int_of_string (List.nth xs 0)) > (int_of_string (List.nth xs 1)) )
let _ = register_predicate "gt0" ["int";"int"] (fun xs -> (int_of_string (List.nth xs 0)) > 0 )
let _ = register_predicate "gt1" ["int";"int"] (fun xs -> (int_of_string (List.nth xs 0)) > 1 )

(*let _ = register_predicate "gte" ["int";"int"] (fun xs -> (int_of_string (List.nth xs 0)) >= (int_of_string (List.nth xs 1)) )
*)
(*let _ = register_predicate "gte-1" ["int"] (fun xs -> (int_of_string (List.hd xs)) >= -1)
let _ = register_predicate "gt" ["int";"int"] (fun xs -> (int_of_string (List.nth xs 0)) > (int_of_string (List.nth xs 1)) )

let _ = register_predicate "lte1" ["int"] (fun xs -> (int_of_string (List.hd xs)) <= 1)
*)
let _ = register_predicate "invr" ["rad";"rad"] (fun xs -> (int_of_string (List.nth xs 0)) = -1*(int_of_string (List.nth xs 1)))
let _ = register_predicate "invi" ["arc";"arc"] (fun xs -> (int_of_string (List.nth xs 0)) = -1*(int_of_string (List.nth xs 1)))

let _ = register_predicate "empty_dict" ["dict"] (fun xs -> (List.nth xs 0) = "{}")
(*let _ = register_predicate "empty_list" ["list"] (fun xs -> (List.nth xs 0) = "[]")*)
let _ = register_predicate "twoPiApart" ["rad"; "rad"] (
    fun xs ->
    (*fun xs -> if (((List.nth xs 0) = "Error") ||  ((List.nth xs 1) = "Error")) then
        true
    else*)
        let x = (int_of_string (List.nth xs 0)) in
        let y = (int_of_string (List.nth xs 1)) in
        (x - y) mod 4 = 0
)

let _ = register_predicate "halfPiApart" ["rad"; "rad"] (
    fun xs ->
    (*fun xs -> if (((List.nth xs 0) = "Error") ||  ((List.nth xs 1) = "Error")) then
        true
    else*)
        let x = (int_of_string (List.nth xs 0)) in
        let y = (int_of_string (List.nth xs 1)) in
        (x + y) = 1
)
