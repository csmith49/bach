open Core
open Problem
open Abduction
open Terms
open Souffle

let noisy = ref false

let spec_list = [
    ("-noisy", Arg.Set noisy, " Print additional information.");
    ("-induct", Arg.String parse_problem_file, " Parses problem statement file.");
    ("-fact", Arg.String (fun s -> Problem.fact_dir := s), " Sets the fact directory.")
]

let usage_msg = "todo"
let anon_fun s = raise (Arg.Bad (s ^ " is not recognized."))

let _ =
    (* parse command line options *)
    Arg.parse (Arg.align spec_list) anon_fun usage_msg;
    (* load config options *)
    parse_work_file "config.sexp";
    (* now we search *)

    (* and finally, print out any relevant stats *)
    if !noisy then begin
        print_endline "STATS";
        print_endline "not yet...";
    end
