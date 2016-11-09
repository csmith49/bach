open Terms
open Core
open Printf
open Problem

let fact_data = ref (StrMap.add "bool" "bool(\"true\")." StrMap.empty)

(* splits a line on tabs and turns everything to strings *)
let parse_line (line : string): string list =
    List.map String.trim (Str.split (Str.regexp "\t") line)

(* helpful, as we want to declare several relations as we build *)
let decl_rel r = match r with
    Relation (n, ts) ->
        let vars = List.mapi (fun i _ ->
                "v_" ^ (string_of_int i) ^ " : T")
            ts in
        ".decl " ^ n ^ "(" ^ (String.concat ", " vars) ^ ")"

(* we're gonna make a string map containing all the data, and we're gonna make souffle pay for it *)
let load_facts (fact_dir : string) =
    (* line by line, turn a b c into rel("a", "b", "c"). *)
    let handle_fact_file rel_name =
        let data = Aux.load_lines (fact_dir ^ rel_name ^ ".facts") in
        let quote s = "\"" ^ s ^ "\"" in
        let process_line l =
            let line_vals = parse_line l in
            rel_name ^ "(" ^ (Aux.concat (List.map quote line_vals)) ^ ")." in
        String.concat "\n" (List.map process_line data)
    in List.fold_left (fun m r ->
            StrMap.add r (handle_fact_file r) m)
        StrMap.empty (List.map Symbol.name !Problem.globals.signature)

(* and now we update our global variable appropriately *)
let add_fact_data (fact_dir : string) =
    let new_facts = load_facts fact_dir in
    fact_data := StrMap.union (fun k v v' ->
            Some (v ^ "\n" ^ v'))
        new_facts !fact_data

(* once we have our concretized stuff, we want to print it out *)
let to_souffle (lhs : ConcretizedMT.t)
               (rhs : ConcretizedMT.t)
               (filename : string): var list = begin

    (* first, might as well grab the variables *)
    let total_vars = List.sort_uniq
            Pervasives.compare
        ((ConcretizedMT.variables lhs) @ (ConcretizedMT.variables rhs)) in
    (* and the symbols to be used *)
    let total_symbols = List.sort_uniq
        Pervasives.compare
        ((ConcretizedMT.symbols_used lhs) @ (ConcretizedMT.symbols_used rhs)) in

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
    (* TODO: extend this with types from the symbols used *)
    write ".type T";

    (* now we declare all relations we use in this file *)
    List.iter (fun s -> match s with Symbol (n, ts) ->
            write (decl_rel (Relation (n, ts))))
        total_symbols;

    (* and booleans *)
    write (decl_rel (Relation ("bool", ["bool_sort"])));

    (* now we can write down all the data we need for the symbols used *)
    write "\n// DATA";
    List.iter (fun s ->
            write (StrMap.find (Symbol.name s) !fact_data))
        total_symbols;

    (* and for the booleans *)
    write (StrMap.find "bool" !fact_data);

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
    (* TODO : this is only n, needs to be 2^n *)
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
let run_souffle souffle work_dir in_file =
    (* we build up a souffle command *)
    let cmd = souffle ^ " -D " ^ work_dir ^ " " in
    (* and then we shia just do it *)
    let _ = Aux.syscall (cmd ^ in_file) in
    ()

(* finally, bundle it all up in a checker *)
let check (lhs : ConcretizedMT.t)
          (rhs : ConcretizedMT.t): (var list) * ((string list) list StrMap.t) =
    let work_dir = !Problem.work_globals.work_dir in
    let filename = !Problem.work_globals.work_file in
    let souffle = !Problem.work_globals.souffle in
    let process_output_file n =
        let lines = Aux.load_lines (work_dir ^ n) in
        List.map parse_line lines
    in
    begin
        let vars = to_souffle lhs rhs (work_dir ^ filename) in
        run_souffle souffle work_dir (work_dir ^ filename);
        let results = List.fold_left (fun m n ->
                StrMap.add n (process_output_file (n ^ ".csv")) m)
            StrMap.empty ["pos"; "lneg"; "rneg"] in
        vars, results
    end
