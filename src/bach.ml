open Core
open Search
open Problem
open Checker
open Decision
open Abduction

let work_dir = "/tmp/"
let work_file = "/tmp/tmp.dl"
let souffle = "~/Documents/code/souffle/src/souffle"

let check l r impl = begin
    to_souffle !Problem.globals.signature l r impl work_file;
    run_souffle souffle work_dir work_file work_dir
end

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

    let e, fp = Search.good_next !f in
        let e, fp = Search.good_next fp in begin
        f := fp;
        print_endline (Partition.to_string e);
        let c = Partition.to_cube_list e in
        check c c true
    end;

    if !noisy then begin
        print_endline "STATS";
        print_endline "not yet...";
    end
