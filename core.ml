(* string aliases for type safety *)
type sort = string
type var = string

(* types for symbols and applied symbols  *)
type symbol = Symbol of string * sort list
type relation = Relation of string * var list
let input_variables =
    function Relation (r, vs) -> List.rev (List.tl (List.rev vs))
let output_variable =
    function Relation (r, vs) -> List.hd (List.rev vs)
let string_of_relation =
    function Relation (r, vs) -> r ^ "(" ^ (String.concat ", " vs) ^ ")"

(* type for representing conjuntion of relations *)
type cube = Cube of relation list
(* modifying cubes *)
let empty_cube = Cube []
let conjoin c r = match c with Cube rs -> Cube (r :: rs)
(* extracting useful information from cubes *)
module VarMap = Map.Make(struct type t = var let compare = compare end)
module DependenceGraph = struct
    type t = (var list) VarMap.t
    let empty = VarMap.empty
    (* build up graph by inserting edges one at a time *)
    let insert source dest graph =  try
        VarMap.add source (dest :: (VarMap.find source graph)) graph
        with Not_found -> VarMap.add source (dest :: []) graph
    let insert_relation rel graph =
        let dest = output_variable rel in
            let f g v = insert v dest g in
                List.fold_left f graph (input_variables rel)
    let from_cube = function
        Cube rs -> let f g r = insert_relation r g in
            List.fold_left f empty rs
    (* extract all the nodes in the graph by seeing which have edges *)
    let sources graph = match (List.split (VarMap.bindings graph)) with
        (l, _) -> l
    let destinations graph = match (List.split (VarMap.bindings graph)) with
        (_, r) -> List.sort_uniq compare (List.concat r)
    let nodes graph = match (List.split (VarMap.bindings graph)) with
        (l, r) -> List.sort_uniq compare (l @ List.concat r)
    (* which nodes are for output, and which are for input? *)
    let is_output n graph = not (List.mem n (sources graph))
    let is_input n graph = not (List.mem n (destinations graph))
    let output_nodes graph = let f n = is_output n graph in
        List.filter f (destinations graph)
    let input_nodes graph = let f n = is_input n graph in
        List.filter f (sources graph)
end
(* finding inputs and outputs of cube *)
let inputs c = DependenceGraph.input_nodes (DependenceGraph.from_cube c)
let outputs c = DependenceGraph.output_nodes (DependenceGraph.from_cube c)

(* printing cubes *)
let string_of_cube =
    function Cube rs -> String.concat " & " (List.map string_of_relation rs)

(* now we deal with partitions of relations in a cube *)
module RelMap = Map.Make(struct type t = int let compare = compare end)

module Partition = struct
    type t = cube RelMap.t
    let empty = RelMap.empty
    (* count the number of partitions, for finding new labels  *)
    let num_partitions part = try
        match (RelMap.max_binding part) with (n, c) -> n
        with Not_found -> 0
    (* add a relation to a partition *)
    let insert_into index rel part = try
        RelMap.add index (conjoin (RelMap.find index part) rel) part
        with Not_found -> RelMap.add index (conjoin empty_cube rel) part
    (* shortcut for making a brand new bucket in the partition with rel *)
    let insert rel part = insert_into (num_partitions part) rel part
end
