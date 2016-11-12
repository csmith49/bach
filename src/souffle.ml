open Terms
open Core
open Printf
open Problem

let fact_data = ref (StrMap.add "bool" "bool(\"true\")." StrMap.empty)

let sort_data = ref SortMap.empty

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

(* we're gonna parse all the fact files, and we're gonna make souffle pay for it *)
let load_fact_data (fact_dir : string) =
    let quote s = "\"" ^ s ^ "\"" in
    let sort_values = ref SortMap.empty in
    let handle_fact_file (s : symbol) = match s with
        Symbol (name, sorts) -> begin
                (* load the data as rows *)
                let raw_lines = Aux.load_lines (fact_dir ^ name ^ ".facts") in
                let data = List.map (fun l ->
                        List.map quote (parse_line l))
                    raw_lines in
                (* turn each line into the appropriate string *)
                let convert_line vals = name ^ "(" ^ (Aux.concat vals) ^ ")." in
                let line_blob = String.concat "\n" (List.map convert_line data) in
                (* and save the blob for later *)
                fact_data := StrMap.add name line_blob !fact_data;
                (* now we need to extract all the columns *)
                let columns = List.combine sorts (Aux.transpose data) in
                let sort_union k l r = Some (l @ r) in
                (* and stick all the sorts together *)
                sort_values := List.fold_left (fun m (k, v) ->
                        let col = SortMap.singleton k v in
                        SortMap.union sort_union m col)
                    !sort_values
                    columns;
            end
        in
    begin
        List.iter handle_fact_file !Problem.globals.signature;
        sort_data := SortMap.mapi (fun k vs ->
                let rel_name = "scope_" ^ k in
                let proc_val v = rel_name ^ "(" ^ v ^ ")." in
                let vs' = List.sort_uniq Pervasives.compare vs in
                let rand_vs = Aux.take_upto
                    (Aux.shuffle vs')
                    !Problem.work_globals.scope_size in
                String.concat "\n" (List.map proc_val rand_vs))
            !sort_values;
    end

(* once we have our concretized stuff, we want to print it out *)
let to_souffle (lhs : ConcretizedMT.t)
               (rhs : ConcretizedMT.t)
               (filename : string)
               (abduction : bool): var list = begin

    (* first, might as well grab the variables *)
    let total_vars = List.sort_uniq
            Pervasives.compare
        ((ConcretizedMT.variables lhs) @ (ConcretizedMT.variables rhs)) in
    (* and the symbols to be used *)
    let total_symbols = List.sort_uniq
        Pervasives.compare
        ((ConcretizedMT.symbols_used lhs) @ (ConcretizedMT.symbols_used rhs)) in
    (* and the sorts, which we need fors coping *)
    let total_sorts = List.sort_uniq
            Pervasives.compare
        (List.map Variables.get_sort total_vars) in

    let out_type = if abduction then " output" else " printsize" in

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

    (* and scope relations *)
    List.iter (fun s ->
            let n = "scope_" ^ s in
            write (".decl " ^ n ^ "(v : T)"))
        total_sorts;

    (* now we can write down all the data we need for the symbols used *)
    write "\n// DATA";
    List.iter (fun s ->
            write (StrMap.find (Symbol.name s) !fact_data))
        total_symbols;

    (* and for the booleans *)
    write (StrMap.find "bool" !fact_data);

    (* now we can write down the actual scoping values *)
    write "\n// SCOPING";
    List.iter (fun s ->
            write (SortMap.find s !sort_data))
        total_sorts;

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
    write ((decl_rel pos) ^ out_type);
    (* and then construct *)
    let hd = Relation.to_string pos in
    let bdy = Aux.concat
        ((ConcretizedMT.pos_strings "lhs" lhs) @ (ConcretizedMT.pos_strings "rhs" rhs)) in
    write (hd ^ " :- " ^ bdy ^ ".");

    (* and left neg next *)
    write "\n// LNEG EVIDENCE";
    (* declare again *)
    write ((decl_rel lneg) ^ out_type);
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
    write ((decl_rel rneg) ^ out_type);
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
    Aux.syscall (cmd ^ in_file)

type counts = Counts of int StrMap.t
type values = Nothing | Values of (string list) list StrMap.t

(* finally, bundle it all up in a checker *)
let check (lhs : ConcretizedMT.t)
          (rhs : ConcretizedMT.t)
          (abduction : bool): (var list) * counts * values =
    let work_dir = !Problem.work_globals.work_dir in
    let filename = !Problem.work_globals.work_file in
    let souffle = !Problem.work_globals.souffle in
    let process_output_file n =
        let lines = Aux.load_upto
            (work_dir ^ n)
            !Problem.work_globals.max_file_size in
        List.map parse_line lines
    in
        let vars = to_souffle lhs rhs (work_dir ^ filename) abduction in
        let output = run_souffle souffle work_dir (work_dir ^ filename) in
        if abduction then begin
            let vals = List.fold_left (fun m n ->
                        StrMap.add n (process_output_file (n ^ ".csv")) m)
                    StrMap.empty ["pos";"lneg";"rneg"] in
            let cnts = StrMap.map List.length vals in
            vars, (Counts cnts), (Values vals)
        end else begin
            let lines = Str.split (Str.regexp "\n") output in
            let pairs' = List.map (fun l -> Str.split (Str.regexp "\t") l) lines in
            let pairs = List.filter (fun l -> (List.length l) = 2) pairs' in
            let cnts = List.fold_left (fun m l ->
                    let k = List.nth l 0 in
                    let v = int_of_string (List.nth l 1) in
                    StrMap.add k v m)
                StrMap.empty pairs in
            vars, (Counts cnts), Nothing
        end
