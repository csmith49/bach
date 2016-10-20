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

(* our end goal is to find guards, which look like the following *)
(* paths have pos (on left) and neg (on right) relations *)
type path = Top | Bottom | Path of (relation list) * (relation list)
(* and guards are really just disjunctions of paths *)
type guard = path list

(* paths form a lattice, some of which is detailed here *)
module Path = struct
    type t = path
    (* basic constructors  *)
    let p_path a = Path ([snd a], [])
    let n_path a = Path ([], [snd a])
    (* lower semi-lattice, should be commutative, effectively is *)
    let meet a b = match a with
        | Top -> b
        | Bottom -> Bottom
        | Path (la, ra) -> match b with
            | Top -> a
            | Bottom -> Bottom
            | Path (lb, rb) -> Path (la @ lb, ra @ rb)
end

(* importantly, we convert trees to paths by a form of flattening *)
let rec to_guard t = match t with
    | Pos -> [Top]
    | Neg -> [Bottom]
    | Attribute (a, l, r) ->
        let pos_paths = List.map (Path.meet (Path.p_path a)) (to_guard l) in
        let neg_paths = List.map (Path.meet (Path.n_path a)) (to_guard r) in
        pos_paths @ neg_paths

(* guards are disjunctions, so having top means g is trivially true
we can also just get rid of any bottoms, they're effectively paths we'll never take *)
let simplify_guard g =
    if List.mem Top g then [Top] else
    List.filter (fun p -> p != Bottom) g

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
    let classifier =
        try AbductionLearner.learn attributes evidence
        with AbductionLearner.No_exact_classifier -> Neg
    in simplify_guard (to_guard classifier)
