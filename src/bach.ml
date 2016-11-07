open Core
open Problem
open Abduction
open Terms
open Souffle
open Preds

let noisy = ref false
let scalar = ref 1.0

let spec_list = [
    ("-noisy", Arg.Set noisy, " Print additional information.");
    ("-induct", Arg.String parse_problem_file, " Parses problem statement file.");
    ("-fact", Arg.String (fun s -> Problem.fact_dir := s), " Sets the fact directory.")
]

let usage_msg = "todo"
let anon_fun s = raise (Arg.Bad (s ^ " is not recognized."))
let noisy_print s = if !noisy then print_endline s else ()

(* utility functions for connecting all the pieces together *)
let results_to_string res =
    let g s =
        let ev = StrMap.find s res in
        let count = List.length ev in
        s ^ ": " ^ (string_of_int count)
    in
    let summary = List.map g ["pos"; "lneg"; "rneg"] in
    Aux.concat summary
let already_implied res =
    let pos_ev = StrMap.find "pos" res in
    let neg_ev = (StrMap.find "lneg" res) @ (StrMap.find "rneg" res) in
    (List.length pos_ev) > 0 && (List.length neg_ev) = 0
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
let left_implied res =
    let pos_ev = StrMap.find "pos" res in
    let lneg_ev = (StrMap.find "lneg" res) in
    (List.length pos_ev) > 0 && (List.length lneg_ev) = 0
let right_implied res =
    let pos_ev = StrMap.find "pos" res in
    let rneg_ev = (StrMap.find "rneg" res) in
    (List.length pos_ev) > 0 && (List.length rneg_ev) = 0
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
    add_fact_data !Problem.fact_dir;
    (* now we search *)
    noisy_print "Starting iteration...";
    (* construct the frontier and history *)
    let frontier = ref (AbstractSearch.start LiftedMT.Truth) in
    let seen = ref ([ConcretizedMT.Truth] : ConcretizedMT.t list) in
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
                (* check the pair *)
                let var_order, results = check c c' in
                (* also help for printing *)
                let pair_string d = (ConcretizedMT.to_string c) ^ d ^ (ConcretizedMT.to_string c') in
                (* we might end up not adding anything` *)
                let okay_to_store = ref true in
                let guard = ref ([] : Guard.t) in
                let direction = ref " ? " in
                (* heuristically, prune if something is reducible to what we've seen *)

                    if already_implied results then begin
                        noisy_print ("" ^ (pair_string " == "));
                        noisy_print ("\talready implied");
                        (* we don't need to score, and guard stays empty *)
                        okay_to_store := false;
                        direction := " == ";
                    end
                    else if left_implied results then begin
                        noisy_print ("" ^ (pair_string " => "));
                        direction := " => ";
                    end
                    else if right_implied results then begin
                        noisy_print ("" ^ (pair_string " <= "));
                        direction := " <= ";
                    end
                    else begin
                        noisy_print ("" ^ (pair_string " == "));
                        noisy_print ("\t" ^ (Guard.to_string !guard));
                        direction := " == ";
                        guard := learn var_order results;
                    end;
                    let s = score results !guard c c' in
                    if 1.0 <= s then begin
                        print_endline "FOUND: ";
                        print_endline ("\t" ^ (pair_string !direction));
                        print_endline ("\t" ^ (results_to_string results));
                        print_endline ("\t" ^ (string_of_float s));
                        print_endline ("\t" ^ (Guard.to_string !guard));
                    end;
                    if !okay_to_store then
                        seen := !seen @ [c'];
            in
            List.iter handle_concretized relative_concretizations;
        in
        List.iter compare_with_concrete !seen;
    done;
