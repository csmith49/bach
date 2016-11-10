open Core
open Problem
open Abduction
open Terms
open Souffle
open Preds

let noisy = ref false
let scalar = ref 1.0
let abduce_flag = ref false
let prune_flag = ref true

let spec_list = [
    ("-noisy", Arg.Set noisy, " Print additional information.");
    ("-induct", Arg.String parse_problem_file, " Parses problem statement file.");
    ("-fact", Arg.String (fun s -> Problem.fact_dir := s), " Sets the fact directory.");
    ("-b", Arg.String (fun s -> begin
            parse_problem_file ("./benchmarks/" ^ s ^ "/" ^ s ^ ".sexp");
            Problem.fact_dir := ("./benchmarks/" ^ s ^ "/facts/");
        end),
    " Runs the named benchmark.");
    ("-abduce", Arg.Set abduce_flag, " Turns on abduction.")
]

let usage_msg = "todo"
let anon_fun s = raise (Arg.Bad (s ^ " is not recognized."))
let noisy_print s = if !noisy then print_endline s else ()

(* utility functions for connecting all the pieces together *)
(* specifially here for modifying results *)
let results_to_string res =
    let g s =
        let ev = StrMap.find s res in
        let count = List.length ev in
        s ^ ": " ^ (string_of_int count)
    in
    let summary = List.map g ["pos"; "lneg"; "rneg"] in
    Aux.concat summary
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

(* and here for checking how good the results are *)
let equivalent res =
    let pos_ev = StrMap.find "pos" res in
    let neg_ev = (StrMap.find "lneg" res) @ (StrMap.find "rneg" res) in
    (List.length pos_ev) > 0 && (List.length neg_ev) = 0
let left_impl res =
    let pos_ev = StrMap.find "pos" res in
    let lneg_ev = (StrMap.find "lneg" res) in
    (List.length pos_ev) > 0 && (List.length lneg_ev) = 0
let right_impl res =
    let pos_ev = StrMap.find "pos" res in
    let rneg_ev = (StrMap.find "rneg" res) in
    (List.length pos_ev) > 0 && (List.length rneg_ev) = 0

(* and finally for scoring the function as best we can *)
let score res g lhs rhs =
    let pos_ev = float (List.length (StrMap.find "pos" res)) in
    let g_score = float (Guard.metric g) in
    let t_score = float ((ConcretizedMT.metric lhs) + (ConcretizedMT.metric rhs)) in
    (pos_ev /. (!scalar *. (g_score +. t_score)))

(* MAIN LOOP *)
let _ =
    (* parse command line options *)
    Arg.parse (Arg.align spec_list) anon_fun usage_msg;
    (* load config options *)
    parse_work_file "config.sexp";
    (* load fact files *)
    load_fact_data !Problem.fact_dir;
    (* now we search *)
    noisy_print "Starting iteration...";
    (* construct the frontier and history *)
    let frontier = ref (AbstractSearch.start LiftedMT.Truth) in
    let seen = ref ([ConcretizedMT.Truth] : ConcretizedMT.t list) in
    let implied = ref ([] : ConcretizedMT.t list) in
    (* and now we loop *)
    while true do
        (* push an abstract mt off the frontier *)
        let e, frontier' = AbstractSearch.next !frontier in
        frontier := frontier';
        noisy_print ("GENERATED: " ^ (LiftedMT.to_string e));
        noisy_print ("CONCRETIZING...");
        let compare_with_concrete c =
            (* pick out the sorts we need to fill, and the variables we need to fill against *)
            let concrete_vars = ConcretizedMT.variables c in
            let symbolic_sorts = LiftedMT.sort_list e in
            let vars = Variables.valid_assignments_inner symbolic_sorts concrete_vars in
            (* now generate all concretizations *)
            let relative_concretizations = List.map (fun vs ->
                    let rel, vs' = LiftedMT.concretize e vs in
                    rel)
                vars in
            (* and now we process the concretizations *)
            let handle_concretized c' =
                let okay_to_report = ref false in
                let guard = ref ([] : Guard.t) in
                let direction = ref " ? " in
                (* also help for printing *)
                let pair_string d =
                    let l = ConcretizedMT.to_string c in
                    let r = ConcretizedMT.to_string c' in
                    let g =
                        if Guard.decides !guard
                            then
                                " | " ^ (Guard.to_string !guard)
                            else ""
                    in
                    l ^ d ^ r ^ g in
                let _ = noisy_print ("CHECKING: " ^ (pair_string " ? ")) in
                (* checks to make sure everything is ship-shape *)
                let non_trivial = ConcretizedMT.non_trivial c c' in
                let well_constrained = ConcretizedMT.well_constrained c c' in
                let not_seen = not (List.mem (ConcretizedMT.rebase_variables c') !implied) in
                if not_seen && (!abduce_flag ||
                                well_constrained ||
                                not non_trivial) then begin
                (* =================================== *)
                (* check the pair *)
                let var_order, results = check c c' in
                let _ = noisy_print ("CHECKING: " ^ (pair_string " ? ")) in
                (* first case, best case --- all positive evidence *)
                if equivalent results then begin
                        if !prune_flag then
                            let clean = ConcretizedMT.rebase_variables c' in
                            implied := !implied @ [clean];
                        okay_to_report := true;
                        direction := " === ";
                    end
                (* next case, left-implication *)
                else if left_impl results && non_trivial then begin
                        okay_to_report := true;
                        direction := " ==> ";
                    end
                (* symmetrically, right-implication *)
                else if right_impl results && non_trivial then begin
                        okay_to_report := true;
                        direction := " <== ";
                    end
                (* final case, maybe we abduce *)
                else if !abduce_flag then begin
                        guard := learn var_order results;
                        if Guard.decides !guard then begin
                                okay_to_report := true;
                                direction := " === ";
                            end
                    end;
                let s = score results !guard c c' in
                if !okay_to_report && (s > 1.0) then begin
                        print_endline (pair_string !direction);
                        noisy_print ("\t" ^ (results_to_string results));
                        noisy_print ("\t" ^ (string_of_float s));
                        noisy_print ("\t" ^ (Guard.to_string !guard));
                    end;
                let clean = ConcretizedMT.rebase_variables c' in
                if not (List.mem clean !implied) then
                    if not (List.mem clean !seen) then
                        seen := !seen @ [clean];
                        noisy_print ("ADDED: "  ^ (ConcretizedMT.to_string clean));
                (* =================================== *)
                end
            in
            List.iter handle_concretized relative_concretizations;
        in
        List.iter compare_with_concrete !seen;
    done;
