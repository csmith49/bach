open Terms
open Core
open Printf
open Problem

let decl_rel r = match r with
    Relation (n, ts) ->
        let vars = List.mapi (fun i _ ->
                "v_" ^ (string_of_int i) ^ " : T")
            ts in
        ".decl " ^ n ^ "(" ^ (String.concat ", " vars) ^ ")"

let to_souffle (lhs : ConcretizedMT.t)
               (rhs : ConcretizedMT.t)
               (filename : string): var list = begin
    (* first, might as well grab the variables *)
    let total_vars = List.sort_uniq
            Pervasives.compare
        ((ConcretizedMT.variables lhs) @ (ConcretizedMT.variables rhs)) in
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
    List.iter write (ConcretizedMT.decl_strings "lhs" lhs);
    List.iter write (ConcretizedMT.defn_strings "lhs" lhs);

    (* how about the rhs now *)
    write "\n// RHS ROOTS";
    List.iter write (ConcretizedMT.decl_strings "rhs" rhs);
    List.iter write (ConcretizedMT.defn_strings "rhs" rhs);

    (* now let's define positive evidence *)
    write "\n// POS EVIDENCE";
    (* which we declare *)
    write ((decl_rel pos) ^ " output");
    (* and then construct *)
    let hd = Relation.to_string pos in
    let bdy = Aux.concat
        ((ConcretizedMT.pos_strings "lhs" lhs) @ (ConcretizedMT.pos_strings "rhs" rhs)) in
    write (hd ^ " :- " ^ bdy ^ ".");

    (* and left neg next *)
    write "\n// LNEG EVIDENCE";
    (* declare again *)
    write ((decl_rel lneg) ^ " output");
    (* save the hd string, we'll need it a bit *)
    let hd = Relation.to_string lneg in
    (* neg parts a list of lists *)
    let pos_part = ConcretizedMT.pos_strings "lhs" lhs in
    let neg_parts = ConcretizedMT.neg_strings "rhs" rhs in
    List.iter (fun ns ->
            let bdy = Aux.concat (pos_part @ ns) in
            write (hd ^ " :- " ^ bdy ^ "."))
        neg_parts;

    (* symmetrically for right *)
    write "\n// RNEG EVIDENCE";
    (* declare again *)
    write ((decl_rel rneg) ^ " output");
    (* save the hd string, we'll need it a bit *)
    let hd = Relation.to_string rneg in
    (* neg parts a list of lists *)
    let pos_part = ConcretizedMT.pos_strings "rhs" rhs in
    let neg_parts = ConcretizedMT.neg_strings "lhs" lhs in
    List.iter (fun ns ->
            let bdy = Aux.concat (pos_part @ ns) in
            write (hd ^ " :- " ^ bdy ^ "."))
        neg_parts;

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
let check (lhs : ConcretizedMT.t)
          (rhs : ConcretizedMT.t): (var list) * ((string list) list StrMap.t) =
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
