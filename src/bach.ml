open Core
open Problem
open Abduction
open Terms
open Souffle
open Clean

let noisy = ref false

let spec_list = [
    ("-noisy", Arg.Set noisy, " Print additional information.");
    ("-induct", Arg.String parse_problem_file, " Parses problem statement file.");
    ("-fact", Arg.String (fun s -> Problem.fact_dir := s), " Sets the fact directory.")
]

let usage_msg = "todo"
let anon_fun s = raise (Arg.Bad (s ^ " is not recognized."))

let noisy_print s = if !noisy then print_endline s else ()

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
                (* check and get results *)
                let results = check s e in
                let pos_vals = StrMap.find "pos" results in
                let lneg_vals = StrMap.find "lneg" results in
                let rneg_vals = StrMap.find "rneg" results in
                (* now we have a few immediate cases we care about *)
                (* there is positive evidence *)
                if ((List.length pos_vals) > 0) then
                    (* and no negative evidence *)
                    if ((List.length (lneg_vals @ rneg_vals)) == 0) then begin
                        noisy_print "POSITIVE FORMULA";
                        print_endline ((Multiterm.to_string s) ^ " <=> " ^ (Multiterm.to_string e));
                    end
                    else if ((List.length lneg_vals) == 0) then begin
                        noisy_print "IMPLICATION FORMULA";
                        print_endline ((Multiterm.to_string s) ^ " ==> " ^ (Multiterm.to_string e));
                    end
                    else if ((List.length rneg_vals) == 0) then begin
                        noisy_print "IMPLICATION FORMULA";
                        print_endline((Multiterm.to_string e) ^ " ==> " ^ (Multiterm.to_string s));
                    end
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
