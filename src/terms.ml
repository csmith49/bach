open Core
open Frontier
open Problem

type ('a, 'b) term = L of 'a | N of 'b * (('a, 'b) term) list

module Term = struct
    type position = int list
    exception Bad_position
    (* now we get to actually manipulate some of this stuff *)
    let rec positions t: position list = match t with
        | L _ -> [[]]
        | N (_, ts) -> [] :: (List.concat
            (List.mapi (fun i t ->
                    List.map (fun p -> i :: p) (positions t))
                ts))
    let rec at_position t p = match p with
        | [] -> t
        | i :: rest -> match t with
            | L _ -> raise Bad_position
            | N (_, ts) -> at_position (List.nth ts i) rest
    let rec set_at t p nt = match p with
        | [] -> nt
        | i :: rest -> match t with
            | L _ -> raise Bad_position
            | N (s, ts) -> let nts =
                List.mapi (fun j t ->
                        if i == j then
                            set_at t rest nt
                        else t) ts
                    in N (s, nts)
    (* of course, we can filter some positions *)
    let filter (f: ('a, 'b) term -> bool)
               (t: ('a, 'b) term): position list =
        let ps = List.sort Pervasives.compare (positions t) in
        List.filter (fun p -> f (at_position t p)) ps
    (* terminal means not leaf, but all children are leaves *)
    let is_leaf t = match t with
        | L _ -> true
        | N (_, _) -> false
    let is_terminal t = match t with
        | L _ -> false
        | N (_, ts) -> List.for_all is_leaf ts
    (* and do some modifications of the temrs *)
    let rec cata (f: 'a -> 'p)
                 (g: 'b -> 'q)
                 (t: ('a, 'b) term): ('p, 'q) term = match t with
        | L v -> L (f v)
        | N (n, ts) ->
            let n' = g n in
            let ts' = List.map (cata f g) ts in
                N (n', ts')
    let pos_map (f : ('a, 'b) term -> 'c)
                (t : ('a, 'b) term)
                (ps : position list): 'c list =
        List.map (fun p -> f (at_position t p)) ps
    let size t = List.length (positions t)
end

module VarTerm = struct
    (* we'll need to make new variables on occasion *)
    let fresh_var_counter = ref 0
    let fresh_var _ = begin
        incr fresh_var_counter;
        "v_" ^ (string_of_int !fresh_var_counter)
    end
    (* and we'll want a type of our very own *)
    type t = (var, symbol) term
    (* if we follow the struct from a sterm, we just have dummy vars *)
    let update_variables (t : t)
                         (vs : (var * Term.position) list): t =
        List.fold_left (fun t' (v, p) -> Term.set_at t' p (L v)) t vs
    (* and finally, we want to convert to a list of relations *)
    let rec inner_to_cube (t : t): cube * var = match t with
        | L v -> Cube.empty, v
        | N (s, ts) ->
            let cs, vs = List.split (List.map inner_to_cube ts) in
            let v = fresh_var () in
            let c = Symbol.apply s (Aux.append vs v) in
            let csi = Aux.flat_map (fun x ->
                    match x with Cube rs -> rs)
                cs in
            Cube (Aux.append csi c), v
    let to_cube (t : t) (output_var : var): cube = match t with
        | L _ -> invalid_arg "to_cube"
        | N (s, ts) ->
            let cs, vs = List.split (List.map inner_to_cube ts) in
            let c = Symbol.apply s (Aux.append vs output_var) in
            let csi = Aux.flat_map (fun x ->
                    match x with Cube rs -> rs)
                cs in
            Cube (Aux.append csi c)
end

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
    (* and we'll want to conver them to varterms, easily *)
    let to_vterm: t -> VarTerm.t =
        Term.cata (fun s -> "v_" ^ s) (fun s -> s)
    (* we'll want to know which sorts we care about *)
    let input_sorts (t: t): (sort * Term.position) list =
        let sort_positions = Term.filter Term.is_leaf t in
        let sorts = Term.pos_map get_sort t sort_positions in
        List.combine sorts sort_positions
    let get_sorts (t: t): (sort * Term.position) list * sort =
        (input_sorts t, get_sort t)
    let to_cube (t : t) (vs : (var * Term.position) list) (output : var): cube =
        VarTerm.to_cube (VarTerm.update_variables (to_vterm t) vs) output
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

module AbstractSearch = Deadbeat(Multiterm)

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
    let next_vars (vs : var list) (s : sort): var list =
        let vs' = vars_with_sort vs s in
        let leftover = Aux.subtract
            (List.assoc s !Problem.globals.variables)
            vs' in
        match leftover with
            | [] -> vs
            | x :: xs -> Aux.append vs' x
    (* and given a list of sorts, we can get all assignments *)
    (* let valid_assignments (ss : sort list): (var list) list =
        let f a s = Aux.flat_map (fun a' ->
                List.map
                        (Aux.append a')
                    (next_vars a' s))
            a in
        List.fold_left f [] ss *)

    let valid_assignments (ss : sort list): (var list) list =
        let extend path s =
            List.map (fun v -> path @ [v]) (next_vars path s) in
        let extend_all paths s =
            Aux.flat_map (fun p -> extend p s) paths in
        let paths = List.fold_left extend_all [[]] ss in
        List.sort_uniq Pervasives.compare paths
end

module Form = struct
    (* base type to help keep track of things *)
    type form = cube * var list * var
    (* some aux functions for manipulating variables *)
    let peel_inputs (s : SortTerm.t)
                    (vs : var list): ((var * Term.position) list) * (var list) =
        let ps = Term.filter Term.is_leaf s in
        let k = List.length ps in
        (List.combine (Aux.take_from vs k) ps , Aux.drop_from vs k)
    let peel_output (s : SortTerm.t)
                    (vs : var list): var * (var list) =
        (List.nth vs 0, Aux.drop_from vs 1)
    (* now we can also peel whole cubes *)
    let peel_term (s : SortTerm.t)
                  (vs : var list): form * (var list) =
        let (ins, vs') = peel_inputs s vs in
        let (out, vs'') = peel_output s vs' in
        let c = SortTerm.to_cube s ins out in
        ((c, fst (List.split ins), out), vs'')
    (* and consequently multiterms *)
    let peel_multiterm (mt : Multiterm.t)
                       (vs : var list): (form list) * (var list) =
        let f (cs, vs') t =
            let c, vs'' = peel_term t vs' in
            (Aux.append cs c, vs'') in
        List.fold_left f ([], vs) mt
    (* finally, we can convert forms to list of pairs of cubes *)
    let convert (lhs : Multiterm.t)
                (rhs : Multiterm.t): (form list * form list) list =
        let sorts = (Multiterm.get_sorts lhs) @ (Multiterm.get_sorts rhs) in
        let vss = Variables.valid_assignments sorts in
        List.map (fun vs ->
                let lc, vs' = peel_multiterm lhs vs in
                let rc, _ = peel_multiterm rhs vs in
                (lc, rc))
            vss
    let name_forms (lhs : form list) (rhs : form list) =
        let name_forms base i f = (base ^ (string_of_int i), f) in
        let lhs' = List.mapi (name_forms "lhs_") lhs in
        let rhs' = List.mapi (name_forms "rhs_") rhs in
        (lhs', rhs')
    (* now we have a section for just the auxilliary printing stuff *)
    let variables (f : form): var list =
        let (c, iv, ov) = f in
            Aux.append iv ov
    let decl_string (n : string) (f : form): string =
        let typed_vars = List.mapi (fun i _ ->
                "v_" ^ (string_of_int i) ^ " : T")
            (variables f) in
        ".decl" ^ n ^ "(" ^ (String.concat ", " (typed_vars)) ^ ")"
    let body_string (f: form): string =
        let (c, iv, ov) = f in match c with
            | Cube [] -> "true(_)"
            | Cube xs ->
                let rels = List.map Relation.to_string xs in
                String.concat ", " rels
    let definition_string (n : string) (f : form): string =
        let hd = n ^ "(" ^ (String.concat ", " (variables f)) ^ ")" in
        let bdy = body_string f in
        hd ^ " :- " ^ bdy ^ "."
    let pos_string (n : string) (f : form): string =
        let (c, iv, ov) = f in match c with
            | Cube [] -> "true(_)"
            | Cube xs ->
                n ^ "(" ^ (String.concat ", " (variables f)) ^ ")"
    let neg_string (n : string) (f : form): string =
        let (c, iv, ov) = f in match c with
            | Cube [] -> "false(_)"
            | Cube _ ->
                let fresh_ov = "fr_" ^ ov in
                let forward =
                    let vars = (Aux.append iv fresh_ov) in
                    n ^ "(" ^ (String.concat ", " vars) ^ ")" in
                let backward =
                    let vars = (Aux.append (List.map (fun _ -> "_") iv) ov) in
                    n ^ "(" ^ (String.concat ", " vars) ^ ")" in
                String.concat ", " [forward; fresh_ov ^ " != " ^ ov; backward]
    let to_string ps =
        Cube.to_string (List.fold_left (fun c (n, f) ->
                let c', _, _ = f in Cube.conjoin c c')
            Cube.empty ps)

end
