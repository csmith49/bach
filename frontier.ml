(* Here we define our induction strategy, which is essentially just a frontier
search. The frontier is implemented as a priority queue, so inserts and pops
are usually log-time. Still, keeping the size of the frontier down is an easy
way to improve the performance. To this end, we order the elements being put
into the queue in a way that encapsulates the parent-child relation in the
search, then use the deadbeat dad heuristic to ensure elements are put in the
queue only when strictly necessary. To implement the search, simply wrap your
data type in a module that extends the SEARCHABLE type and pass that module to
the Deadbeat functor. *)

module type ORDERED = sig
    type t
    val compare : t -> t -> int
end

module Frontier = functor (Elt : ORDERED) -> struct
    type elt = Elt.t
    type queue = Empty | Node of elt * queue * queue
    let empty = Empty
    let rec push queue e = match queue with
        | Empty -> Node (e, Empty, Empty)
        | Node (ec, left, right) ->
            if (Elt.compare e ec) <= 0
                then Node (e, push right ec, left)
                else Node (ec, push right e, left)
    exception Frontier_is_empty
    let rec remove_top = function
        | Empty -> raise Frontier_is_empty
        | Node (e, left, Empty) -> left
        | Node (e, Empty, right) -> right
        | Node (e, (Node (le, _, _) as left), (Node (re, _, _) as right)) ->
            if (Elt.compare le re) <= 0
                then Node (le, remove_top left, right)
                else Node (re, left, remove_top right)
    let pop = function
        | Empty -> raise Frontier_is_empty
        | Node (e, _, _) as f -> (e, remove_top f)
end

module type SEARCHABLE = sig
    type t
    val compare : t -> t -> int
    val children : t -> t list
    val parents : t -> t list
end

module Deadbeat = functor (Elt : SEARCHABLE) -> struct
    type elt = Elt.t
    module DeadbeatFrontier = Frontier(Elt)
    type t = DeadbeatFrontier.queue
    let can_abandon parent child =
        let better_dad dad = (Elt.compare parent dad) == 1 in
        List.exists better_dad (Elt.parents child)
    let start e = DeadbeatFrontier.push DeadbeatFrontier.empty e
    let next frontier =
        let e, f = DeadbeatFrontier.pop frontier in
        let is_dependent k = not (can_abandon e k) in
        let dependents = List.filter is_dependent (Elt.children e) in
        let new_frontier = List.fold_left DeadbeatFrontier.push f dependents in
        (e, new_frontier)
end
