open Core
open Decision
open Problem
open Souffle

let global_preds = ref []

let usable_preds var_sorts =
    let sorts = fst (List.split var_sorts) in
    let usable p = match (snd p) with
        Symbol (s, ts) -> List.for_all (fun s ->
                List.mem s sorts)
            ts in
    List.filter usable !global_preds

module Domain = struct
    type elt = string list
    type tag = relation
    let elt_to_string = Aux.concat
    let tag_to_string = Relation.to_string
    let is_error = List.mem "Error"
    let well_defined_tag r = match r with
        Relation (n, vs) ->
            let unique = List.sort_uniq Pervasives.compare vs in
            (List.length vs) = (List.length unique)
end

(* we now make our special instance of the id3 module *)
module AbductionLearner = IDTree(Domain)
module ConjunctLearner = Conjuncts(Domain)

(* to convert our learned tree into something reasonable, we should convert to labeled paths *)
type path_label = PLabel | NLabel | MLabel
type path = Path of (relation list) * (relation list) * path_label
type guard = path list

module Path = struct
    type t = path

    let to_string p = match p with
        Path (l, r, m) ->
            let pos = List.map Relation.to_string l in
            let neg = List.map (fun rel -> "!" ^ Relation.to_string rel) r in
            let rels = String.concat ", " (pos @ neg) in
            match m with
                | PLabel -> "+: " ^ rels
                | NLabel -> "-: " ^ rels
                | MLabel -> "?: " ^ rels

    let metric p = match p with
        Path (l, r, m) -> List.length (l @ r)
end

module Guard = struct
    type t = guard
    (* by recursive traversal, we can pick up labeled paths easily *)
    let rec paths t = match t with
        | Pos -> [Path ([], [], PLabel)]
        | Neg -> [Path ([], [], NLabel)]
        | Mixed -> [Path ([], [], MLabel)]
        | Attribute ((f, a), l, r) ->
            let lps = List.map (fun p -> match p with
                    Path (ll, rr, m) -> Path (a :: ll, rr, m)
                ) (paths l) in
            let rps = List.map (fun p -> match p with
                    Path (ll, rr, m) -> Path (ll, a :: rr, m)
                ) (paths r) in
            lps @ rps
    (* a neg path can become a pos path by negation *)
    let positivize g = List.map (fun p -> match p with
            Path (l, r, m) -> match m with
                | NLabel -> Path (r, l, PLabel)
                | _ -> p
        ) g
    (* and we don't care about mixed labels, they're no good to us *)
    let exact_paths g = List.filter (fun p -> match p with
            Path (_, _, m) -> match m with
                | MLabel -> false
                | _ -> true
        ) g
    (* and the ever-helpful printer *)
    let to_string g =
        String.concat " V " (List.map Path.to_string g)
    (* we must construct additional metrics *)
    let metric g = List.fold_left (+) 0 (List.map Path.metric g)
    (* and of course, the important part is post-processing *)
    let clean g = positivize (exact_paths g)
    let reduce g =
        (* first, we have to canonicalize all paths *)
        let paths = List.map (fun p -> match p with
                Path (pos, neg, label) ->
                    let pos' = List.sort Pervasives.compare pos in
                    let neg' = List.sort Pervasives.compare neg in
                    Path (pos', neg', label))
            g in
        ()
    let decides g = (List.length g) > 0
    let empty = []
end

(* types and stuff for predicates we're searching over *)
type predicate = ((int list) -> ((string list) -> bool)) * symbol

let pred (f : (string list) -> bool) : (int list) -> ((string list) -> bool) =
    let selecter (xs : int list) =
        let f' (ss : string list) =
            let args = List.map (fun i -> List.nth ss i) xs in
            f args
        in f'
    in selecter

let register_predicate (name : string) (sorts : sort list) (f : (string list) -> bool) =
    let s = Symbol (name, sorts) in
    let f' = pred f in
    global_preds := Aux.append !global_preds (f', s)

(* given predicates and knowledge about variables, we'll make attributes for id3 *)
let create_attributes (preds : predicate list)
                      (var_order : int VarMap.t) (* gives index in data point *)
                      (var_sorts : (sort * var list) list) (* gives all vars of a sort *) =
    let used_vars = fst (List.split (VarMap.bindings var_order)) in
    let make_single_attr p vs =
        let vi = List.map (fun k -> VarMap.find k var_order) vs in
        ((fst p) vi , Symbol.apply (snd p) vs) in
    let vars_from_sorts ss = List.map (fun s ->
            let possible = List.assoc s var_sorts in
            Aux.intersect used_vars possible)
        ss in
    let make_attrs p = List.map
            (make_single_attr p)
        (Aux.cart_prod (vars_from_sorts (Symbol.sorts (snd p)))) in
    Aux.flat_map make_attrs preds

(* have to convert the evidence that we expect to see into usable forms *)
let create_evidence (pos_list : string list)
                    (neg_list : string list)
                    (results : (string list) list StrMap.t) : (string list * bool) list =
    let pos_ev = List.map (fun v -> (v, true))
        (Aux.flat_map (fun p -> StrMap.find p results) pos_list) in
    let neg_ev = List.map (fun v -> (v, false))
        (Aux.flat_map (fun n -> StrMap.find n results) neg_list) in
    pos_ev @ neg_ev

(* and apply it to abduction in the obvious way *)
let abduce (var_order : int VarMap.t)
           (evidence : AbductionLearner.labeled list) =
    let var_sorts = !Problem.globals.variables in
    let attributes = create_attributes (usable_preds var_sorts) var_order var_sorts in
    let classifier = AbductionLearner.learn_to_depth attributes evidence !Problem.globals.abduction_depth in
    Guard.clean (Guard.paths classifier)

let simple_abduce (var_order : int VarMap.t)
                  (evidence : ConjunctLearner.labeled list) =
    let var_sorts = !Problem.globals.variables in
    let attributes = create_attributes (usable_preds var_sorts) var_order var_sorts in
    ConjunctLearner.partial_learn attributes evidence !Problem.globals.abduction_depth
