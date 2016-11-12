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

let mindepth = ref 0
let set_mindepth n = mindepth := n
let scope = ref 1000
let set_scope n = scope := n
let pruned = ref []

let spec_list = [
    ("-noisy", Arg.Set noisy, " Print additional information.");
    ("-induct", Arg.String parse_problem_file, " Parses problem statement file.");
    ("-fact", Arg.String (fun s -> Problem.fact_dir := s), " Sets the fact directory.");
    ("-b", Arg.String (fun s -> begin
            parse_problem_file ("./benchmarks/" ^ s ^ "/" ^ s ^ ".sexp");
            Problem.fact_dir := ("./benchmarks/" ^ s ^ "/facts/");
        end),
    " Runs the named benchmark.");
    ("-abduce", Arg.Set abduce_flag, " Turns on abduction.");
    ("-mindepth", Arg.Int (set_mindepth), " Minimum size of specs.");
    ("-scope", Arg.Int (set_scope), " Scope size.")
]

let usage_msg = "todo"
let anon_fun s = raise (Arg.Bad (s ^ " is not recognized."))
let noisy_print s = if !noisy then print_endline s else ()

(* utility functions for connecting all the pieces together *)
(* specifially here for modifying results *)
let counts_to_string res = match res with
    Counts cts -> let g s =
                    let count = StrMap.find s cts in
                    s ^ ": " ^ (string_of_int count)
        in let summary = List.map g ["pos"; "lneg"; "rneg"] in
        Aux.concat summary
let list_to_varmap ls =
    List.fold_left (fun m (k, v) -> VarMap.add k v m)
        VarMap.empty
        (List.mapi (fun i v -> (v, i)) ls)
let learn var_order results = match results with
    | Values vals ->
        let evidence = create_evidence ["pos"] ["lneg";"rneg"] vals in
        simple_abduce (list_to_varmap var_order) evidence
    | Nothing -> None

(* and here for checking how good the results are *)
let equivalent res = match res with
    Counts cts ->
        let pos_ev = StrMap.find "pos" cts in
        let neg_ev = (StrMap.find "lneg" cts) + (StrMap.find "rneg" cts) in
        (pos_ev > 0) && (neg_ev = 0)
let left_impl res = match res with
    Counts cts ->
        let pos_ev = StrMap.find "pos" cts in
        let neg_ev = StrMap.find "lneg" cts in
        (pos_ev > 0) && (neg_ev = 0)
let right_impl res = match res with
    Counts cts ->
        let pos_ev = StrMap.find "pos" cts in
        let neg_ev = StrMap.find "rneg" cts in
        (pos_ev > 0) && (neg_ev = 0)

(* and finally for scoring the function as best we can *)
let score res lhs rhs = match res with
    Counts cts ->
        let pos_ev = float (StrMap.find "pos" cts) in
        let t_score = float ((ConcretizedMT.metric lhs) + (ConcretizedMT.metric rhs)) in
        (pos_ev /. (!scalar *. t_score))

(* now we need to actually process pairs of mts and stuff *)
let process_pair (lhs : ConcretizedMT.t)
                 (rhs : ConcretizedMT.t) : ConcretizedMT.t option =
    (* fill this out as we go *)
    let okay_to_record = ref true in
    let okay_to_report = ref false in
    let direction = ref " ? " in
    let guard_string = ref "" in
    (* eases printing throughout *)
    let pair_string d =
        let l = ConcretizedMT.to_string lhs in
        let r = ConcretizedMT.to_string rhs in
        l ^ d ^ r ^ " | " ^ !guard_string in
    let _ = noisy_print ("\tChecking: " ^ (pair_string !direction)) in
    (* now we need to check that everything is in tip top shape *)
    (* lhs not in pruned *)
    let lhs_np = not (List.mem (ConcretizedMT.rebase_variables lhs) !pruned) in
    let rhs_np = not (List.mem (ConcretizedMT.rebase_variables rhs) !pruned) in
    let np = lhs_np && rhs_np in
    (* no truth sides *)
    let nt = ConcretizedMT.non_trivial lhs rhs in
    (* non-empty variable intersection *)
    let wc = ConcretizedMT.well_constrained lhs rhs in
    (* sides aren't contained in the other *)
    let nc = not (ConcretizedMT.containment_check lhs rhs) in
    (* now see if we should proceed *)
    if np && nc && (!abduce_flag || wc) then begin
        (* get results! finally! *)
        let var_order, counts, values = check lhs rhs !abduce_flag in
        let _ = noisy_print ("\t" ^ (counts_to_string counts)) in
        if equivalent counts then begin
            if !prune_flag then begin
                let crhs = ConcretizedMT.rebase_variables rhs in
                let clhs = ConcretizedMT.rebase_variables lhs in
                if (Pervasives.compare crhs clhs) != 0 then begin
                    pruned := Aux.append !pruned crhs;
                    noisy_print ("PRUNED: " ^ (ConcretizedMT.to_string clhs) ^
                                " < " ^ (ConcretizedMT.to_string crhs));
                end;
            end;
            okay_to_report := true;
            direction := " === ";
        end else if left_impl counts && nt then begin
            okay_to_report := true;
            direction := " ==> ";
        end else if right_impl counts && nt then begin
            okay_to_report := true;
            direction := " <== ";
        end else if !abduce_flag then begin
            let guard = learn var_order values in match guard with
                | Some c -> begin
                    okay_to_report := true;
                    direction := " === ";
                    let l, r = ConjunctLearner.conjunct_description c in
                    let ls = Aux.concat (List.map Relation.to_string l) in
                    let rs = Aux.concat (List.map Relation.to_string r) in
                    guard_string := "(" ^ ls ^ ") & !(" ^ rs ^ ")"
                end
                | _ -> ()
        end;
        (* now we can see how well we did *)
        let s = score counts lhs rhs in
        (* if the results are worth reporting, print 'em *)
        if !okay_to_report && (s >= 0.0) then begin
            print_endline (pair_string !direction);
            noisy_print ("\t" ^ (counts_to_string counts));
            noisy_print ("\t" ^ (string_of_float s));
        end;
        (* now clean up the rhs *)
        let clean = ConcretizedMT.rebase_variables rhs in
        Some (clean)
    end else None

(* MAIN LOOP *)
let _ =
    (* parse command line options *)
    Arg.parse (Arg.align spec_list) anon_fun usage_msg;
    (* load config options *)
    parse_work_file "config.sexp";
    (* load fact files *)
    load_fact_data !Problem.fact_dir !scope;
    (* now we search *)
    noisy_print "Starting iteration...";
    (* construct the frontier and history *)
    let frontier = ref (AbstractSearch.start LiftedMT.Truth) in
    let seen = ref ([] : LiftedMT.t list) in
    (* and now we loop *)
    while true do
        (* push an abstract mt off the frontier *)
        let e, frontier' = AbstractSearch.next !frontier in
        frontier := frontier';
        noisy_print ("GENERATED: " ^ (LiftedMT.to_string e));
        noisy_print ("CONCRETIZING...");
        (* how do we actually compare? *)
        let compare_with_symbolic c =
            print_endline ("length " ^ (string_of_int ((LiftedMT.length c) + (LiftedMT.length e))));
            (* minimum depth check *)
            if ((LiftedMT.length c) + (LiftedMT.length e)) < !mindepth then
                []
            else begin
                let symbolic_sorts = (LiftedMT.sort_list c) @ (LiftedMT.sort_list e) in
                let _ = print_endline  "start vars" in
                let vars = Variables.valid_assignments symbolic_sorts in
                let _ = print_endline  "end vars" in
                (* now we concretize *)
                let concs = List.rev (List.rev_map (fun vs ->
                        let lhs, vs' = LiftedMT.concretize c vs in
                        let rhs, _ = LiftedMT.concretize e vs' in
                        (lhs, rhs))
                    vars) in
                List.fold_left (fun l (lhs, rhs) ->
                        match (process_pair lhs rhs) with
                            | None -> l
                            | Some conc -> Aux.append l conc)
                    []
                    concs
            end
        in begin
            seen := Aux.append !seen e;
            List.iter (fun c -> let _ = compare_with_symbolic c in ()) !seen;
        end
    done;
