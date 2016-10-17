open Core

type 'a idtree = Attribute of 'a * ('a idtree) * ('a idtree)
              | Pos
              | Neg

module type DECIDABLE = sig
    type elt
    type tag
end

module IDTree = functor (D : DECIDABLE) -> struct
    type elt = D.elt
    type labeled = (elt * bool)
    type tag = D.tag
    type attribute = ((elt -> bool) * tag)
    type t = attribute idtree

    (* break down attributes as necessary *)
    let apply (a : attribute) (e : elt) =
        (fst a) e

    let description (a : attribute) = snd a

    (* for manipulating labeled elements *)
    let positive_elements (ls : labeled list) =
        List.map (fun (e, _) -> e) (List.filter (fun (_, b) -> b) ls)
    let negative_elements (ls : labeled list) =
        List.map (fun (e, _) -> e) (List.filter (fun (_, b) -> not b) ls)

    let entropy (ls : labeled list) =
        let size = float (List.length ls) in
        let p_prop = (float (List.length (positive_elements ls))) /. size in
        let n_prop = (float (List.length (negative_elements ls))) /. size in
        (* the usual entropy calculation *)
        -. ((p_prop *. (log p_prop)) +. (n_prop *. (log n_prop)))

    let info_gain (a : attribute) (ls : labeled list) =
        let size = float (List.length ls) in
        let f l = apply a (fst l) in
        (* break up ls into classes based on application of a *)
        let (p_class, n_class) = List.partition f ls in
        let p_class_prop = (float (List.length p_class)) /. size in
        let n_class_prop = (float (List.length n_class)) /. size in
        (* now compute info gain from entropy calculations *)
        (entropy ls) -.
            (p_class_prop *. (entropy p_class)) -.
            (n_class_prop *. (entropy n_class))

    (* simple classification of an element *)
    let rec classify tree elt = match tree with
        | Attribute (a, l, r) ->
            let f = apply a in
                if (f elt) then classify l elt
                else classify r elt
        | Pos -> true
        | Neg -> false

    (* selects the attribute that has the most info gain *)
    let select_attribute (atts : attribute list) (ls : labeled list) =
        let search_atts = List.map (fun a -> (a, info_gain a ls)) atts in
        let cmp x y = Pervasives.compare (snd x) (snd y) in
        let a, _ = List.hd (List.rev (List.sort cmp search_atts)) in
        (a, Aux.subtract atts [a])

    exception No_exact_classifier

    (* let's learn a tree from a set of tagged elements and attributes *)
    let rec learn (atts : attribute list) (ls : labeled list) =
        (* count positive and negative examples *)
        let num_pos = (List.length (positive_elements ls)) in
        let num_neg = (List.length (negative_elements ls)) in
        (* case 1: all positive elements in ls *)
        if (num_neg == 0) then Pos else
        (* case 2: all negative elements in ls *)
        if (num_pos == 0) then Neg else
        (* case 3: attribute list is empty *)
        if (List.length atts) == 0 then
            raise No_exact_classifier
        else
        (* case 4: let's recurse *)
        let a, new_atts = select_attribute atts ls in
        let f l = apply a (fst l) in
        let p, n = List.partition f ls in
        Attribute (a, (learn new_atts p), (learn new_atts n))
end
