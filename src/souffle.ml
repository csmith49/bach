open Core
open Terms
open Problem
open Printf

(* helper functions for making some boilerplate code *)
let rec make_type n =
    if n == 0 then [] else "T" :: (make_type (n-1))

let make_var n =
    let rec make_var' n m =
        if m >= n+1 then
            []
        else
            ("v" ^ (string_of_int m)) :: (make_var' n (m+1))
    in make_var' n 1

(* minor string and type conversion functions *)
let symbol_to_rel = function
    | Symbol (s, ss) -> Relation (s, ss)

let relation_string = function
    | Relation (f, vs) ->
        let vars = make_var (List.length vs) in
        let args = List.map (fun v -> v ^ " : T") vars in
        ".decl " ^ f ^ "(" ^ (String.concat ", " args) ^ ")"

let root_to_rel r n =
    let (c, iv, ov) = (Root.to_cube r) in
        c, Relation (n, iv @ [ov])

let root_to_rule_string r n =
    let c, rel = root_to_rel r n in
    let rs = relation_string rel in
    let hd = Relation.to_string rel in
    let bdy = match c with
        Cube xs ->
            String.concat ", " (List.map Relation.to_string xs)
    in
        rs ^ "\n" ^ hd ^ " :- " ^ bdy ^ "."

let rel_to_neg_string r =
    let x = Relation.output r in
    let fresh_x = "fr_" ^ x in
    let inputs = Relation.inputs r in
    let fresh_inputs = List.map (fun s -> "_") inputs in
    match r with Relation (f, _) ->
        let new_rel = Relation (f, inputs @ [fresh_x]) in
        let binder_rel = Relation (f, fresh_inputs @ [x]) in
        let rs = Relation.to_string new_rel in
        let brs = Relation.to_string binder_rel in
            rs ^ ", " ^ brs ^ ", " ^ x ^ " != " ^ fresh_x

(* now lets write stuff out to a file *)
let to_souffle (lhs : multiterm)
               (rhs : multiterm)
               (filename : string): var list = begin
    (* for making variables *)
    let count = ref 0 in

    (* we need these *)
    let _, li, lo = Multiterm.to_cube lhs in
    let _, ri, ro = Multiterm.to_cube rhs in
    let total_vars = List.sort_uniq Pervasives.compare (li @ ri @ lo @ ro) in
    (* pos and neg relations *)
    let pos = Relation ("pos", total_vars) in
    let lneg = Relation ("lneg", total_vars) in
    let rneg = Relation ("rneg", total_vars) in
    (* so we can find the relations later *)
    let lhs_rels = ref [] in
    let rhs_rels = ref [] in

    (* lets make an output channel *)
    let oc = open_out filename in
    let write s = fprintf oc "%s\n" s in
    write "// BASIC DECLS";
    (* we assume souffle just has to deal with a single type *)
    write ".type T";
    (* we'll print all possible input relations *)
    List.iter (fun s -> write
        ((relation_string (symbol_to_rel s)) ^ " input")
    ) !Problem.globals.signature;
    (* now the lhs roots *)
    write "\n// LHS ROOTS";
    List.iteri (fun i r -> begin
            let name = "lhs_" ^ (string_of_int i) in
            lhs_rels := !lhs_rels @ [snd (root_to_rel r name)];
            write (root_to_rule_string r name);
        end) lhs;
    (* and the rhs roots *)
    write "\n// RHS ROOTS";
    List.iteri (fun i r -> begin
            let name = "rhs_" ^ (string_of_int i) in
            rhs_rels := !rhs_rels @ [snd (root_to_rel r name)];
            write (root_to_rule_string r name);
        end) rhs;
    (* now we can say what we mean by positive evidence *)
    write "\n// POSITIVE EVIDENCE";
    let rs = (relation_string pos) ^ " output" in
    let hd = Relation.to_string pos in
    let bdy = String.concat ", " (List.map
            Relation.to_string
        (!lhs_rels @ !rhs_rels)) in
    write (rs ^ "\n" ^ hd ^ " :- " ^ bdy ^ ".");
    (* we'll start with left negative evidence *)
    write "\n// LEFT NEGATIVE EVIDENCE";
    let hd = Relation.to_string lneg in
    let pos_body = (String.concat ", " (List.map
            Relation.to_string
        !lhs_rels)) in
    write ((relation_string lneg) ^ " output");
    List.iter (fun r ->
            let neg = rel_to_neg_string r in
            write (hd ^ " :- " ^ pos_body ^ ", " ^ neg ^ ".")
        ) !rhs_rels;
    (* and copy for right neg evidence *)
    write "\n// RIGHT NEGATIVE EVIDENCE";
    let hd = Relation.to_string rneg in
    let pos_body = (String.concat ", " (List.map
            Relation.to_string
        !rhs_rels)) in
    write ((relation_string rneg) ^ " output");
    List.iter (fun r ->
            let neg = rel_to_neg_string r in
            write (hd ^ " :- " ^ pos_body ^ ", " ^ neg ^ ".")
        ) !lhs_rels;
    (* finally, we need to close the file *)
    close_out oc;
    total_vars
end

(* actually executes the command *)
let run_souffle souffle work_dir in_file fact_dir =
    (* we build up a souffle command *)
    let cmd = souffle ^ " -D " ^ work_dir ^ " -F " ^ fact_dir ^ " " in
    (* and then we shia just do it *)
    Aux.syscall (cmd ^ in_file)

(* splits a line on tabs and turns everything to integers *)
let parse_line (line : string): int list =
    List.map (fun s -> int_of_string (String.trim s)) (Str.split (Str.regexp "\t") line)

(* finally, we bundle everything together as a checker *)
let check (lhs : multiterm)
          (rhs : multiterm) : (int list) list StrMap.t =
    let work_dir = !Problem.work_globals.work_dir in
    let filename = !Problem.work_globals.work_file in
    let fact_dir = !Problem.fact_dir in
    let souffle = !Problem.work_globals.souffle in
    let process_output_file n =
        let lines = Aux.load_lines (work_dir ^ n) in
        List.map parse_line lines
    in
    begin
        to_souffle lhs rhs (work_dir ^ filename);
        run_souffle souffle work_dir (work_dir ^ filename) fact_dir;
        List.fold_left (fun m n ->
                StrMap.add n (process_output_file (n ^ ".csv")) m
            ) StrMap.empty ["pos"; "lneg"; "rneg"]
    end
