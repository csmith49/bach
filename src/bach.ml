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

let noisy_print s = if !noisy then print_endline s else ()

let _ =
    (* parse command line options *)
    Arg.parse (Arg.align spec_list) anon_fun usage_msg;
    (* load config options *)
    parse_work_file "config.sexp";
    (* now we search *)
    print_endline (string_of_int (List.length (Multiterm.children [])));

    print_endline "TESTING";

    let vars = Variables.next_vars ["x";"y";"z"] "fp17" in
    print_endline ("VARS: " ^ (String.concat ", " vars));
    print_endline (Variables.get_sort "x");

    let frontier = ref (AbstractSearch.start []) in
    let _, f' = AbstractSearch.next !frontier in
    let seen = ref [] in
    frontier := f';

    let interpret_check smp l r =
        let pos_ev = StrMap.find "pos" smp in
        let lneg_ev = StrMap.find "lneg" smp in
        let rneg_ev = StrMap.find "rneg" smp in
        if (List.length pos_ev) > 0 then
            if (List.length (lneg_ev @ rneg_ev)) = 0 then
                let fancy_l = Form.to_string l in
                let fancy_r = Form.to_string r in
                    print_endline (fancy_l ^ " == " ^ fancy_r)
    in
    while true do
        let e, f' = AbstractSearch.next !frontier in
        frontier := f';
        seen := !seen @ [e];
        List.iter (fun e' -> begin
            let forms = Form.convert e e' in
            List.iter (fun (l, r) ->
                let lhs, rhs = Form.name_forms l r in
                let vars, smp = check lhs rhs in
                (* print_endline ("Checking: " ^ (Form.to_string lhs) ^ ", " ^ (Form.to_string rhs)); *)
                interpret_check smp lhs rhs;
                ()
                ) forms;
        end) !seen;
        print_endline (Multiterm.to_string e);
    done;
    (* and finally, print out any relevant stats *)
    if !noisy then begin
        print_endline "STATS";
        print_endline "not yet...";
    end
