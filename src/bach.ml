open Core
open Problem
open Abduction
open Terms
open Souffle
open Preds

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
    (* load fact files *)
    add_fact_data !Problem.fact_dir;
    (* now we search *)
    print_endline "TESTING";

    let frontier = ref (AbstractSearch.start LiftedMT.Truth) in
    let seen = ref [] in
    while true do
        let e, frontier' = AbstractSearch.next !frontier in
        frontier := frontier';
        seen := !seen @ [e];
        print_endline ("GENERATED: " ^ (LiftedMT.to_string e));
        (* this is what we're iterating over pairs with *)
        let process_results res =
            let g s =
                let ev = StrMap.find s res in
                let count = List.length ev in
                s ^ ": " ^ (string_of_int count)
            in
            let summary = List.map g ["pos"; "lneg"; "rneg"] in
            Aux.concat summary
        in
        let learn vs res =
            let pos_ev = StrMap.find "pos" res in
            let lneg_ev = StrMap.find "lneg" res in
            let rneg_ev = StrMap.find "rneg" res in
            let pos = List.map (fun v -> (v, true)) pos_ev in
            let neg = List.map (fun v -> (v, false)) (lneg_ev @ rneg_ev) in
            let var_order = List.fold_left (fun m p ->
                    let (i, v) = p in
                    VarMap.add v i m)
                VarMap.empty (List.mapi (fun i v ->
                        (i, v))
                    vs) in
            abduce var_order !Problem.globals.variables (pos @ neg)
        in
        let f e' =
            print_endline "=========================>\nCOMPARING: ";
            print_endline ("\t" ^ (LiftedMT.to_string e));
            print_endline ("\t" ^ (LiftedMT.to_string e'));
            let sorts = (LiftedMT.sort_list e) @ (LiftedMT.sort_list e') in
            print_endline ("FOUND SORTS: " ^ (Aux.concat sorts));
            let vars = Variables.valid_assignments sorts in
            print_endline "GIVING CONCRETIZATIONS:";
            let concs = List.map (fun vs ->
                    let lhs, vs' = LiftedMT.concretize e vs in
                    let rhs, _ = LiftedMT.concretize e' vs' in
                    (lhs, rhs))
                vars in
            List.iter (fun (vs, (lhs, rhs)) ->
                    let rel_vars, results = check lhs rhs in
                    print_endline ("\tvars: " ^ (Aux.concat vs));
                    let l = ConcretizedMT.to_string lhs in
                    let r = ConcretizedMT.to_string rhs in
                    print_endline ("\tformula: " ^ l ^ " == " ^ r);
                    print_endline ("\t" ^ (process_results results));
                    let guard = if (rel_vars = [])
                        then []
                        else learn rel_vars results
                    in
                    print_endline ("\t" ^ (Guard.to_string guard));
                    Aux.wait ();)
                (List.combine vars concs);

        in List.iter f !seen;
    done;

    (* and finally, print out any relevant stats *)
    if !noisy then begin
        print_endline "STATS";
        print_endline "not yet...";
    end
