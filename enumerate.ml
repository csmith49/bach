open Core

(* we enumerate by a frontier search, so we need a priority queue *)
module PQueue = struct
    type priority = int
    type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
    let empty = Empty
    let rec push queue p e = match queue with
        | Empty -> Node (p, e, Empty, Empty)
        | Node (pc, ec, left, right) ->
            if p <= pc
                then Node(p, e, push right pc ec, left)
                else Node(pc, ec, push right p e, left)
    exception Queue_is_empty
    let rec remove_top = function
        | Empty -> raise Queue_is_empty
        | Node (p, e, left, Empty) -> left
        | Node (p, e, Empty, right) -> right
        | Node (p, e, (Node (lp, le, _, _) as left), (Node (rp, re, _, _) as right)) ->
            if lp <= rp
                then Node (lp, le, remove_top left, right)
                else Node (rp, re, left, remove_top right)
    let pop = function
        | Empty -> raise Queue_is_empty
        | Node (p, e, _, _) as queue -> (p, e, remove_top queue)
end

(* we need cartesian product for generation purposes *)
let rec cart_prod = function
    | [] -> [[]]
    | x :: xs -> let rest = cart_prod xs in
        List.concat (List.map (fun i -> List.map (fun rs -> i :: rs) rest) x)

(* record type simplifies signature of search and aux functions *)
type config = {
    signature : symbol list;
    max_terms : int;
    max_heuristic : int;
}

(* metric : Partition -> int *)
(* for enumeration purposes - must subsume parent-child relation *)
(* TODO : finish *)
let metric p = 0

(* need a way to create fresh variables for enumeration *)
let fresh_var = "FRESH"
(* we treat FRESH as a symbolic var - canonicalize converts symbolic part to all concrete parts *)
(* concretize : Partition -> Partition list *)
let concretize part = []

(* aux functions *)
let list_sub l r = List.filter (fun e -> not (List.mem e r)) l

let cube_vars c = DependenceGraph.nodes (DependenceGraph.from_cube c)

let flat_map f xs = List.concat (List.map f xs)

let symbol_combos (v_in: Core.var list) (v_out: Core.var list) (symb: Core.symbol) = begin
    let f vs = apply_symbol symb vs in
    let variables = ref [] in
    for i = 0 to ((symbol_arity symb) - 1) do
        variables := v_in :: !variables;
    done;
    variables := v_out :: !variables;
    List.map f (cart_prod !variables)
end

(* children : config -> Partition -> Partition list *)
(* TODO : not enumerating type-safe *)
(* TODO : ignoring most of the configuration params *)
let children conf p = begin
    let output = ref [] in
    (* we need to know which variables we can use *)
    let p_outputs = Partition.total_outputs p in
    let possible = ("FRESH" :: Partition.variables p) in
    (* iterate over each cube in the partition to find out where we can put things *)
    for i = 0 to (Partition.num_partitions p) do
        let c = Partition.get_cube i p in
        (* we allow anuthing for input variables, but only local vars for outputs *)
        let c_inputs = possible in
        let c_outputs = list_sub (cube_vars c) p_outputs in
        (* we apply the symbol to all variables and concretize the fresh vars *)
        let symbolic = flat_map (symbol_combos c_inputs c_outputs) conf.signature in
        let new_parts = List.map (fun s -> Partition.insert_into i s p) symbolic in
        output := !output @ (flat_map concretize new_parts);
    done;
    (* and for the case where we make a new cube *)
    let symbolic = flat_map (symbol_combos possible possible) conf.signature in
    let new_parts = List.map (fun s -> Partition.insert s p) symbolic in
    output := !output @ (flat_map concretize new_parts);
    !output
end

(* parents : config -> Partition -> Partition list *)
let parents conf p = []

(* aux functions *)
let add_to_frontier queue part =
    PQueue.push queue (metric part) part

let not_my_problem conf dad kid =
    List.exists (fun d -> (metric d) > (metric dad)) (parents conf kid)

(* enumerate : config -> Partition list *)
let enumerate conf =
    (* construct data structs as refs that we can update in place *)
    let loop = ref true in
    let frontier = ref PQueue.empty in
    let output = ref [] in
    (* loop until our config tells us to break *)
    while !loop; do
        (* get new element, update queue *)
        let (p, e, q) = PQueue.pop !frontier in
        frontier := q;
        (* see if we need to exit *)
        if conf.max_heuristic < p then loop := false;
        (* filter by kids we need to keep *)
        let kids = List.filter (not_my_problem conf e) (children conf e) in
        (* add to output and frontier  *)
        List.iter (fun k -> begin
            output := (k :: !output);
            frontier := add_to_frontier !frontier k;
        end) kids;
    done;
    (* return the final value, reversing output *)
    List.rev !output
