open Core
open Problem
open Checker
open Decision
open Abduction
open Terms
open Souffle

let work_dir = "/tmp/"
let work_file = "/tmp/tmp.dl"
let souffle = "~/Documents/code/souffle/src/souffle"

let noisy = ref false

let spec_list = [
    ("-noisy", Arg.Set noisy, " Print additional information.");
    ("-induct", Arg.String parse_problem_file, " Parses problem statement file.")
]

let usage_msg = "todo"
let anon_fun s = raise (Arg.Bad (s ^ " is not recognized."))

let _ =
    let f = ref (TermSearch.start []) in

    Arg.parse (Arg.align spec_list) anon_fun usage_msg;

    if !noisy then print_endline "Noisy is set.";

    let e, fp = TermSearch.next !f in
        let e, fp = TermSearch.next fp in
            let e, fp = TermSearch.next fp in
                to_souffle e e "tmp.dl";

    if !noisy then begin
        print_endline "STATS";
        print_endline "not yet...";
    end
