open Test
open Core
open Printf
open Problem

type printable = string * Form.form

let decl_rel r = match r with
    Relation (n, ts) ->
        let vars = List.mapi (fun i _ ->
                "v_" ^ (string_of_int i) ^ " : T")
            ts in
        ".decl " ^ n ^ "(" ^ (String.concat ", " vars) ^ ")"

let to_souffle (lhs : printable list)
               (rhs : printable list)
               (filename : string): var list = begin
    (* first, might as well grab the variables *)
    let extract_vars (n, (c, iv, ov)) = Aux.append iv ov in
    let total_vars = List.sort_uniq
            Pervasives.compare
        (Aux.flat_map
                extract_vars
            (lhs @ rhs)) in
    (* and now we'll define pos, lneg, rneg *)
    let pos = Relation ("pos", total_vars) in
    let lneg = Relation ("lneg", total_vars) in
    let rneg = Relation ("rneg", total_vars) in
    (* then open an output channel *)
    let oc = open_out filename in
    let write s = fprintf oc "%s\n" s in
    (* and do some printing *)
    write "// BASIC DECLS";
    (* we assume souffle has to deal with just one type *)
    write ".type T";
    (* now we need to print all input relations *)
    List.iter (fun s -> match s with Symbol (n, ts) ->
                write ((decl_rel (Relation (n, ts))) ^ " input"))
        !Problem.globals.signature;
    (* now the lhs decls and bodies *)
    write "\n// LHS ROOTS";
    List.iter (fun (n, f) ->
            begin
                write (Form.decl_string n f);
                write (Form.definition_string n f);
            end)
        lhs;
    (* how about the rhs now *)
    write "\n// RHS ROOTS";
    List.iter (fun (n, f) ->
            begin
                write (Form.decl_string n f);
                write (Form.definition_string n f);
            end)
        rhs;
    (* now let's define positive evidence *)
    write "\n// POS EVIDENCE";
    write ((decl_rel pos) ^ " output");
    let hd = Relation.to_string pos in
    let bdy = String.concat ", " (List.map (fun (n, f) ->
            Form.pos_string n f)
        (lhs @ rhs)) in
    write (hd ^ " :- " ^ bdy ^ ".");
    (* and left neg next *)
    write "\n// LNEG EVIDENCE";
    write ((decl_rel lneg) ^ " output");
    let hd = Relation.to_string lneg in
    let lbdy = String.concat ", " (List.map (fun (n, f) ->
            Form.pos_string n f)
        lhs) in
    let rbdys = List.map (fun (n, f) -> Form.neg_string n f) rhs in
    List.iter (fun n ->
            write (hd ^ " :- " ^ lbdy ^ ", " ^ n ^ ".");)
        rbdys;
    (* symmetrically for right *)
    write "\n// RNEG EVIDENCE";
    write ((decl_rel rneg) ^ " output");
    let hd = Relation.to_string rneg in
    let lbdy = String.concat ", " (List.map (fun (n, f) ->
            Form.pos_string n f)
        rhs) in
    let rbdys = List.map (fun (n, f) -> Form.neg_string n f) lhs in
    List.iter (fun n ->
            write (hd ^ " :- " ^ lbdy ^ ", " ^ n ^ ".");)
        rbdys;
    (* and finally, we can print things *)
    close_out oc;
    total_vars
end

(* actually executes the command *)
let run_souffle souffle work_dir in_file fact_dir =
    (* we build up a souffle command *)
    let cmd = souffle ^ " -D " ^ work_dir ^ " -F " ^ fact_dir ^ " " in
    (* and then we shia just do it *)
    let _ = Aux.syscall (cmd ^ in_file) in
    ()

(* splits a line on tabs and turns everything to strings *)
let parse_line (line : string): string list =
    Str.split (Str.regexp "\t") line

(* finally, bundle it all up in a checker *)
let check (lhs : printable list)
          (rhs : printable list): (var list) * ((string list) list StrMap.t) =
    let work_dir = !Problem.work_globals.work_dir in
    let filename = !Problem.work_globals.work_file in
    let fact_dir = !Problem.fact_dir in
    let souffle = !Problem.work_globals.souffle in
    let process_output_file n =
        let lines = Aux.load_lines (work_dir ^ n) in
        List.map parse_line lines
    in
    begin
        let vars = to_souffle lhs rhs (work_dir ^ filename) in
        run_souffle souffle work_dir (work_dir ^ filename) fact_dir;
        let results = List.fold_left (fun m n ->
                StrMap.add n (process_output_file (n ^ ".csv")) m)
            StrMap.empty ["pos"; "lneg"; "rneg"] in
        vars, results
    end
