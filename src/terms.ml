open Core
open Frontier
open Problem

(* a very helpful module *)
module Variables = struct
    type t = (sort * (var list)) list
    (* a copy from above *)
    let global_sorts _ = fst (List.split !Problem.globals.variables)
    (* find the sort of a variable *)
    let get_sort (v : var): sort =
        let f (_, vs) = List.mem v vs in
        let ss =
            fst (List.split (List.filter
                    f
                !Problem.globals.variables)) in
        match ss with
            | [] -> invalid_arg "get_sort"
            | s :: xs -> s
    (* get all variables of a particular sort form a list *)
    let vars_with_sort (vs : var list) (s : sort): var list =
        List.filter (fun v -> (get_sort v) = s) vs
    (* and now the next variables that can be used of a sort *)
    let next_vars_inner (vs : var list) (seen : var list) (s : sort): var list =
        let vars = List.sort_uniq Pervasives.compare (vs @ seen) in
        let vs' = vars_with_sort vars s in
        let leftover = Aux.subtract
            (List.assoc s !Problem.globals.variables)
            vs' in
        match leftover with
            | [] -> vars
            | x :: xs -> Aux.append vars x
    let next_vars (vs : var list) (s : sort): var list =
        next_vars_inner vs [] s
    (* we can build up from the sorts and variables that we've seen *)
    let valid_assignments_inner (ss : sort list) (vs : var list) : (var list) list =
        let extend path s =
            List.map (fun v -> path @ [v]) (next_vars_inner path vs s) in
        let extend_all paths s =
            Aux.flat_map (fun p -> extend p s) paths in
        let paths = List.fold_left extend_all [[]] ss in
        List.sort_uniq Pervasives.compare paths
    let valid_assignments (ss : sort list): (var list) list =
        valid_assignments_inner ss []
    (* but the names aren't always right *)
    let get_fresh (s : sort) (used : var list) : var =
        let vars = List.assoc s !Problem.globals.variables in
        try List.hd (Aux.subtract vars used)
        with _ -> invalid_arg "get_fresh"
    let rebase_variables (vs : var list) : var list =
        let subs = List.fold_left (fun m v ->
                if not (VarMap.mem v m) then
                    let used = snd (List.split (VarMap.bindings m)) in
                    let fresh = get_fresh (get_sort v) used in
                    VarMap.add v fresh m
                else m)
            VarMap.empty vs in
        List.map (fun v -> VarMap.find v subs) vs
end

(* how we search *)
module SortTerm = struct
    (* leaves are sorts, nodes are symbols *)
    type t = (sort, symbol) term
    (* we can make these in several ways *)
    let from_symbol (s: symbol): t =
        N (s, List.map( fun s' -> L s') (Symbol.inputs s))
    let from_sort (s: sort): t list = List.map
            from_symbol
        (List.filter (fun s' ->
                (Symbol.output s') = s)
            !Problem.globals.signature)
    let get_sort t = match t with
        | L s -> s
        | N (s, _) -> Symbol.output s
    (* we'll want to know which sorts we care about *)
    let input_sorts (t: t): (sort * Term.position) list =
        let sort_positions = Term.filter Term.is_leaf t in
        let sorts = Term.pos_map get_sort t sort_positions in
        List.combine sorts sort_positions
    let get_sorts (t: t): (sort * Term.position) list * sort =
        (input_sorts t, get_sort t)
    let sort_list t = fst (List.split (input_sorts t)) @ [get_sort t]
    (* now we can define the searching functions *)
    let children t =
        Aux.flat_map (fun (s, p) ->
                List.map (fun t' ->
                        Term.set_at t p t')
                    (from_sort s))
            (input_sorts t)
    let parents t =
        let terminal_positions = Term.filter Term.is_terminal t in
        let terminal_sorts = Term.pos_map get_sort t terminal_positions in
        List.map (fun (s, p) ->
                Term.set_at t p (L s))
            (List.combine terminal_sorts terminal_positions)
    (* and printing stuff *)
    let rec to_string t = match t with
        | L s -> s
        | N (s, ts) ->
            let args = String.concat ", " (List.map to_string ts) in
            (Symbol.name s) ^ "(" ^ args ^ ")"
    (* comparison stuff *)
    let metric s = (Term.size s, s)
    let compare l r = Pervasives.compare (metric l) (metric r)
    let num_variables s = List.length (sort_list s)
end

module Multiterm = struct
    type t = SortTerm.t list
    (* some aliases *)
    let at_index = List.nth
    let length = List.length
    let set_at = Aux.set_at
    let append = Aux.append
    (* pretty printing is a must at this point *)
    let to_string m =
        let ts = List.map SortTerm.to_string m in
        String.concat " & " ts
    (* what sorts are actually in scope? *)
    let global_sorts _ = fst (List.split !Problem.globals.variables)
    (* and what sorts can we find in our terms> *)
    let get_sorts mt =
        let f ss t =
            let ins, out = SortTerm.get_sorts t in
            ss @ (fst (List.split ins)) @ [out] in
        List.fold_left f [] mt
    (* and the search, in terms of sortterms *)
    let children (mt: t): t list =
        let output = ref [] in
        (* update existing terms *)
        List.iteri (fun i t ->
                let mts = List.map (fun t' ->
                        set_at mt i t')
                    (SortTerm.children t) in
                output := !output @ mts;)
            mt;
        (* and maybe add some stuff to the end *)
        if (length mt) < !Problem.globals.max_terms then
            List.iter (fun t ->
                    output := (append mt t) :: !output;)
                (Aux.flat_map
                        SortTerm.children
                    (List.map (fun s -> L s)
                        (global_sorts ())));
        (* and finally return the results *)
        !output
    let parents (mt: t): t list =
        let output = ref [] in
        (* if we see terminal, remove it. otherwise just get parents *)
        let f i t = match t with
            | N (s, ts) -> if (Term.is_terminal t)
                then output := (Aux.delete_at mt i) :: !output
                else
                    let mts = List.map (fun t' ->
                            set_at mt i t')
                        (SortTerm.parents t) in
                    output := mts @ !output
            | _ -> output := (Aux.delete_at mt i) :: !output in
        List.iteri f mt;
        !output
    (* oh, and we need to be able to break ties *)
    let metric mt =
        let size = List.fold_left (+) 0 (List.map Term.size mt) in
        (size, mt)
    let compare l r = Pervasives.compare (metric l) (metric r)
end

module Root = struct
    type root = Root of (var, symbol) term * var
    (* we'll need to make new variables on occasion *)
    let fresh_var_counter = ref 0
    let fresh_var _ = begin
        incr fresh_var_counter;
        "v_" ^ (string_of_int !fresh_var_counter)
    end
    (* we only want these so we can make cubes anyways *)
    let rec inner_to_cube (t : (var, symbol) term) : cube * var = match t with
        | L v -> Cube.empty, v
        | N (s, ts) ->
            let cs, vs = List.split (List.map inner_to_cube ts) in
            let fresh_v = fresh_var () in
            let c = Symbol.apply s (Aux.append vs fresh_v) in
            List.fold_left Cube.conjoin (Cube [c]) cs, fresh_v
    let to_cube (r : root) : cube = match r with
        Root (t, v) -> match t with
            | L _ -> invalid_arg "to_cube"
            | N (s, ts) ->
                let cs, vs = List.split (List.map inner_to_cube ts) in
                let c = Symbol.apply s (Aux.append vs v) in
                List.fold_left Cube.conjoin (Cube [c]) cs
    (* and of course, once we know vars we want to construct a root *)
    let concretize (s : SortTerm.t) (vars : var list) : root =
        let in_vars, out_var = Aux.rev_cons vars in
        let vps = List.combine in_vars (Term.filter Term.is_leaf s) in
        let t = List.fold_left (fun t' (v, p) ->
                Term.set_at t' p (L v))
            s vps in
        Root (t, out_var)
    let to_sterm (r : root) : SortTerm.t = match r with
        Root (t, v) -> Term.cata (fun v -> Variables.get_sort v) (fun s -> s) t
    let udpate_variables (r : root) (vs : var list) : root =
        concretize (to_sterm r) vs
    (* for printing, we need some info *)
    let input_variables (r : root) = match r with
        Root (t, v) -> List.map (fun p ->
                let t' = Term.at_position t p in match t' with
                    | L v' -> v'
                    | _ -> invalid_arg "input_variables")
            (Term.filter Term.is_leaf t)
    let output_variable (r : root) = match r with
        Root (t, v) -> v
    let variables (r : root) : var list =
        Aux.append (input_variables r) (output_variable r)
    let positive (name : string) (r : root) =
        name ^ "(" ^ (Aux.concat (variables r)) ^ ")"
    let negative (name : string) (r : root) =
        let in_vars = input_variables r in
        let fresh_in_vars = List.map (fun _ -> "_") in_vars in
        let out_var = output_variable r in
        let fresh_out_var = "fr_" ^ out_var in
        let forwards =
            name ^ "(" ^ (Aux.concat (Aux.append in_vars fresh_out_var)) ^ ")" in
        let backwards =
            name ^ "(" ^ (Aux.concat (Aux.append fresh_in_vars out_var)) ^ ")" in
        let twist = out_var ^ " != " ^ fresh_out_var in
        Aux.concat [forwards; twist; backwards]
    (* sometimes we want to print the root, tho --- more readable *)
    let rec vterm_to_string (t : (var, symbol) term) : string = match t with
        | L v -> v
        | N (s, ts) ->
            let f = Symbol.name s in
            let args = List.map vterm_to_string ts in
            f ^ "(" ^ (Aux.concat args) ^ ")"
    let to_string (r : root) : string = match r with
        Root (t, v) ->
            let t_string = vterm_to_string t in
            t_string ^ " = " ^ v
    let fst (r : root) = match r with
        Root (t, v) -> t
    let metric (r : root) = match r with
        Root (t, v) -> Term.size t
end

(* this module just helps with printing --- needed for souffle stuff *)
module ConcretizedMT = struct
    type t = Truth | Concretized of Root.root list
    (* we can combine these things! *)
    let conjoin (l : t) (r : t) : t = match l with
        | Truth -> r
        | Concretized lhs -> match r with
            | Truth -> l
            | Concretized rhs -> Concretized (lhs @ rhs)
    (* we need to declare each root in the mt *)
    let decl_strings (base : string) (cmt : t) : string list = match cmt with
        | Truth -> []
        | Concretized rs ->
            let set_ith i r =
                let vars = Root.variables r in
                let var_decls = List.mapi (fun j _ ->
                        "v_" ^ (string_of_int j) ^ " : T")
                    vars in
                let name = base ^ "_" ^ (string_of_int i) in
                ".decl " ^ name ^ "(" ^ (Aux.concat var_decls) ^ ")"
            in List.mapi set_ith rs
    let defn_strings (base : string) (cmt : t) : string list = match cmt with
        | Truth -> []
        | Concretized rs ->
            let set_ith i r =
                let hd = Root.positive (base ^ "_" ^ (string_of_int i)) r in
                let body = Cube.to_string (Root.to_cube r) in
                hd ^ " :- " ^ body ^ "."
            in List.mapi set_ith rs
    let pos_strings (base : string) (cmt : t) : string list = match cmt with
        | Truth -> ["bool(\"true\")"]
        | Concretized rs ->
            let set_ith i r =
                Root.positive (base ^ "_" ^ (string_of_int i)) r
            in List.mapi set_ith rs
    let neg_strings (base : string) (cmt : t) : string list list = match cmt with
        | Truth -> [["bool(\"false\")"]]
        | Concretized rs ->
            let set_ith i r =
                let neg = Root.negative (base ^ "_" ^ (string_of_int i)) r in
                let pos = pos_strings base cmt in
                Aux.append (Aux.delete_at pos i) neg
            in List.mapi set_ith rs
    (* and it's good to know what variables we're actually using *)
    let variables (cmt : t) : var list = match cmt with
        | Truth -> []
        | Concretized rs -> Aux.flat_map Root.variables rs
    (* as well as how to print this *)
    let to_string (cmt : t) : string = match cmt with
        | Truth -> "T"
        | Concretized rs -> Aux.concat (List.map Root.to_string rs)
    (* to limit filesize, need exactly the input relations that appear here *)
    let symbols_used (cmt : t) : symbol list = match cmt with
        | Truth -> []
        | Concretized rs -> Aux.flat_map (fun t ->
                Term.node_values t)
            (List.map Root.fst rs)
    (* metrics for dayyyyyys *)
    let metric (cmt : t) : int = match cmt with
        | Truth -> 1
        | Concretized rs -> List.fold_left (+) 0 (List.map Root.metric rs)
    (* we'll want to check if mts are well quantified *)
    let input_variables (cmt : t) : var list = match cmt with
        | Truth -> []
        | Concretized rs -> List.sort_uniq Pervasives.compare (Aux.flat_map
                Root.input_variables
            rs)
    let output_variables (cmt : t) : var list = match cmt with
        | Truth -> []
        | Concretized rs -> List.sort_uniq Pervasives.compare (List.map
                Root.output_variable
            rs)
    let variables (cmt : t) : var list = List.sort_uniq Pervasives.compare
        ((input_variables cmt) @ (output_variables cmt))
    (* to constrain: output vars have to show up somewhere else *)
    let well_constrained (l : t) (r : t) : bool =
        let cmt = conjoin l r in
        let roots = match cmt with
            | Truth -> []
            | Concretized rs -> rs in
        let all_but_output index = List.concat (List.mapi (fun i r ->
                if i = index then Root.input_variables r
                else Root.variables r)
            roots) in
        List.for_all (fun b -> b) (List.mapi (fun i r ->
                let o = Root.output_variable r in
                let vars = all_but_output i in
                List.mem o vars)
            roots)
end

module LiftedMT = struct
    type t = Truth | Lifted of Multiterm.t
    (* conversion between *)
    let from_mt mt = match mt with
        | [] -> Truth
        | _ -> Lifted (List.sort SortTerm.compare mt)
    let to_mt lmt = match lmt with
        | Truth -> []
        | Lifted mt -> mt
    (* we have a natural comparison *)
    let compare a b = match a with
        | Truth -> -1
        | Lifted ml -> match b with
            | Truth -> 1
            | Lifted mr -> Multiterm.compare ml mr
    (* and now we lift the search cleanly *)
    let children lmt =
        let mt_kids = Multiterm.children (to_mt lmt) in
        let kids = List.map from_mt mt_kids in
        List.sort_uniq compare kids
    let parents lmt =
        let mt_parents = Multiterm.parents (to_mt lmt) in
        let parents = List.map from_mt mt_parents in
        List.sort_uniq compare parents
    (* we have to concretize as some point *)
    let sort_list lmt = match lmt with
        | Truth -> []
        | Lifted mt -> Aux.flat_map SortTerm.sort_list mt
    let concretize (lmt : t) (vars : var list) : ConcretizedMT.t * var list = match lmt with
        | Truth -> ConcretizedMT.Truth, vars
        | Lifted mt ->
            let roots, leftovers = List.fold_left (fun (rs, vs) s ->
                    let k = SortTerm.num_variables s in
                    let to_use = Aux.take_from vs k in
                    let vs' = Aux.drop_from vs k in
                    (Aux.append rs (Root.concretize s to_use), vs'))
                ([], vars) mt
            in ConcretizedMT.Concretized roots, leftovers
    (* I do enjoy me some printing *)
    let to_string (lmt : t) : string = match lmt with
        | Truth -> "T"
        | Lifted mt -> Aux.concat (List.map SortTerm.to_string mt)
end

module AbstractSearch = Deadbeat(LiftedMT)
