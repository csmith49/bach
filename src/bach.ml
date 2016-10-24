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
    let seen = ref [] in
    let frontier = ref (TermSearch.start []) in
    (* we need to iterate the frontier to get rid of blank elements *)
    let _, f = TermSearch.next !frontier in
    frontier := f;
    (* then we can start looping *)
    while true do
        (* let's increment the search *)
        let e, new_frontier = TermSearch.next !frontier in
        frontier := new_frontier;
        (* we'll check each pair in seen x e *)
        List.iter (fun s ->
                if ((Multiterm.out_constrained_by s e) && (Multiterm.out_constrained_by e s)) then
                    let results = check s e in
                    if !noisy then begin
                        print_endline "Checking...";
                        print_endline ((Multiterm.to_string s) ^ " = " ^ (Multiterm.to_string e));
                    end else ();
                else ();
            ) !seen;
        (* and finally add our new multiterm to the seen list *)
        seen := e :: !seen;
    done;
    (* and finally, print out any relevant stats *)
    if !noisy then begin
        print_endline "STATS";
        print_endline "not yet...";
    end
