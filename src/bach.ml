open Core
open Search
open Problem
open Checker


let noisy = ref false

let spec_list = [
    ("-noisy", Arg.Set noisy, " Print additional information.");
    ("-induct", Arg.String parse_problem_file, " Parses problem statement file.")
]

let usage_msg = "todo"
let anon_fun s = raise (Arg.Bad (s ^ " is not recognized."))

let _ =
    let f = ref (Search.start Partition.empty) in

    Arg.parse (Arg.align spec_list) anon_fun usage_msg;

    if !noisy then print_endline "Noisy is set.";

    for i = 0 to 100 do
        let e, fp = Search.good_next !f in
        f := fp;
        print_endline (Partition.to_string e);
    done;

    if !noisy then begin
        print_endline "STATS";
        print_endline "not yet...";
    end
