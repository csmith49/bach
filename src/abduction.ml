open Core
open Decision
open Checker

let global_preds = []

(* we now make our special instance of the id3 module *)
module AbductionLearner =
    IDTree(struct
        type elt = int list
        type tag = relation
    end)

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
end

(* types and stuff for predicates we're searching over *)
type predicate = ((int list) -> ((int list) -> bool)) * symbol

(* given predicates and knowledge about variables, we'll make attributes for id3 *)
let create_attributes (preds : predicate list)
                      (var_order : int VarMap.t) (* gives index in data point *)
                      (var_sorts : (var list) SortMap.t) (* gives all vars of a sort *)=
    let make_single_attr p vs =
        let vi = List.map (fun k -> VarMap.find k var_order) vs in
        ((fst p) vi , Symbol.apply (snd p) vs) in
    let vars_from_sorts ss = List.map (fun s -> SortMap.find s var_sorts) ss in
    let make_attrs p = List.map
            (make_single_attr p)
        (Aux.cart_prod (vars_from_sorts (Symbol.sorts (snd p)))) in
    Aux.flat_map make_attrs preds

(* and apply it to abduction in the obvious way *)
let abduce (var_order : int VarMap.t)
           (var_sorts : (var list) SortMap.t)
           (evidence : AbductionLearner.labeled list) =
    let attributes = create_attributes global_preds var_order var_sorts in
    let classifier = AbductionLearner.learn attributes evidence in
    Guard.positivize (Guard.exact_paths (Guard.paths classifier))
