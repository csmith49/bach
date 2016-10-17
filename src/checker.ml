open Core
open Printf

(*
  NOTES:
  assumes that souffle is in ~/git/souffle-lang/src/souffle
  it's hardcoded for now.

  input facts should be in RELNAME.facts file - tab-separated
  (make sure it's actually tabs when testing, in case you're a spaceman)
*)

let rec make_type n =
  if n == 0 then [] else "T" :: (make_type (n-1))

let make_var n =
  let rec make_var' n m =
    if m >= n+1 then [] else ("v"^(string_of_int m)) :: (make_var' n (m+1))
  in
  make_var' n 1

(* checker receives a list of symbols and lists of subcubes *)

let to_souffle rs lhs rhs impl output_file = begin
    let count = ref 0 in

    let cube_representation c name =
        incr count;
        let in_vars = Cube.inputs c in
        let out_vars = Cube.outputs c in
        let arr = (List.length in_vars) + (List.length out_vars) in
        let s_name = name ^ "_" ^ (string_of_int !count) in
        let s = Symbol (s_name, make_type arr) in
        let r = Relation (s_name, in_vars @ out_vars) in
        (c, Cube.to_string c, s, r)
    in
    let cube_representation_string cr =
        let (c, body, s, r) = cr in
        (Relation.to_string r) ^ " :- " ^ body ^ "."
    in
    let relation_string = function
        | Symbol (s_name, types) ->
            let vars = make_var (List.length types) in
            let args = List.map (fun v -> v ^ " : " ^ "T") vars in
                ".decl " ^ s_name ^ "(" ^ (String.concat ", " args) ^ ")"
    in
    let input_rel_string s = (relation_string s) ^ " input"
    in
    (* we use the following line when we only care about numbers, and not
    actual evidence
    let output_rel_string s = (relation_string s) ^ " output printsize" *)
    let output_rel_string s = (relation_string s) ^ " output"
    in

    let makePositiveCl lhsReps rhsReps =
        let body =
            Cube (List.map
                (fun (_, _, _, rel) -> rel)
                (lhsReps @ rhsReps)) in
        let body_string = Cube.to_string body in

        let in_vars = Cube.inputs body in
        let out_vars = Cube.outputs body in
        let arr = (List.length in_vars) + (List.length out_vars) in

        let s_name = "pos" in
        let s = Symbol (s_name, make_type arr) in
        let r = Relation (s_name, in_vars @ out_vars) in
        (s, (Relation.to_string r) ^ " :- " ^ body_string ^ ".")
    in

    let makeNegativeCls lhsReps rhsReps left =
        let negate = fun (c, body, s, r) ->
            let out_var = Relation.output r in
            let in_vars = Relation.inputs r in
            let fresh_out = "fr_" ^ out_var in
            let neg_r = match r with
                Relation (n, vs) -> Relation (n, in_vars @ [fresh_out]) in
            (Relation.to_string neg_r) ^ (", " ^ out_var ^ " != " ^ fresh_out) in
        let pos_body =
            Cube (List.map (fun (_, _, _, r) -> r) lhsReps) in
        let body_string = Cube.to_string pos_body in
        let in_vars = Cube.inputs pos_body in
        let out_vars = Cube.outputs pos_body in
        let arr = (List.length in_vars) + (List.length out_vars) in

        let rhs_neg = List.map (fun x -> negate x) rhsReps in

        let s_name = if left then "lneg" else "rneg" in
        let s = Symbol (s_name, make_type arr) in
        let r = Relation (s_name, in_vars @ out_vars) in

        (s, List.map (fun x ->
                (Relation.to_string r) ^ " :- " ^ body_string ^ ", " ^ x ^ "."
            ) rhs_neg)
    in

    let lhsReps = List.map (fun c -> cube_representation c "lhs") lhs in
    let rhsReps = List.map (fun c -> cube_representation c "rhs") rhs in

    let (psym, pcl) = makePositiveCl lhsReps rhsReps in

    (* dump to file *)
    let oc = open_out output_file in

    (* TODO: for now, im assuming everything is type T *)
    fprintf oc ".type T\n\n";

    (* declare input relations *)
    List.iter (fun x -> fprintf oc "%s\n" (input_rel_string x)) rs;

    (* declare output relations LHS *)
    fprintf oc "\n\n//LHS programs:\n";

    let orelsLhs = List.map (fun (_,_,s,_) -> s) lhsReps in
    List.iter (fun x -> fprintf oc "%s\n" (relation_string x)) orelsLhs;

    List.iter (fun x -> fprintf oc "%s\n" (cube_representation_string x)) lhsReps;

    (* declare output relations RHS *)
    fprintf oc "\n\n//RHS programs:\n";

    let orelsRhs = List.map (fun (_,_,s,_) -> s) rhsReps in
    List.iter (fun x -> fprintf oc "%s\n" (output_rel_string x)) orelsRhs;

    List.iter (fun x -> fprintf oc "%s\n" (cube_representation_string x)) rhsReps;

    (* declare output relation Positive Evidence *)
    fprintf oc "\n\n//Positive evidence:\n";
    fprintf oc "%s\n" (output_rel_string psym);
    fprintf oc "%s\n" pcl;

    let (lnsym, lnegClsList) = makeNegativeCls lhsReps rhsReps true in

    fprintf oc "\n\n//Left Negative evidence:\n";
    fprintf oc "%s\n" (output_rel_string lnsym);
    List.iter (fun clsStr -> fprintf oc "%s\n" clsStr) lnegClsList;

    if not impl then begin
        let (rnsym, rnegClsList) = makeNegativeCls rhsReps lhsReps false in

        fprintf oc "\n\n//Right Negative evidence:\n";
        fprintf oc "%s\n" (output_rel_string rnsym);
        List.iter (fun clsStr -> fprintf oc "%s\n" clsStr) rnegClsList;
    end;

    close_out oc;
    end

let run_souffle souffle work_dir in_file fact_dir =
    (* we build up a souffle command *)
    let cmd = souffle ^ " -D " ^ work_dir ^ " -I " ^ fact_dir ^ " " in
    let std_out = Aux.syscall (cmd ^ in_file) in
    (* then extract the results from the fact files in the work_dir *)

    (* and return them wrapped up in a StrMap *)
    ();
