open Sexplib.Std

(* aux functions go here *)
module Aux = struct
    let rec list_pop xs i = match xs with
        | [] -> []
        | y :: ys -> if i == 0
            then ys
            else y :: (list_pop ys (i - 1))
    let rec cart_prod = function
        | [] -> [[]]
        | x :: xs -> let rest = cart_prod xs in
            List.concat (List.map (fun i -> List.map (fun rs -> i :: rs) rest) x)
    let flat_map f xs = List.concat (List.map f xs)
    let subtract b s = List.filter (fun e -> not (List.mem e s)) b
    let rec list_copy xs i =
        if i == 0 then []
        else xs :: (list_copy xs (i - 1))
    let syscall cmd =
        let ic, oc = Unix.open_process cmd in
        let buffer = Buffer.create 16 in
        (try
            while true do
                Buffer.add_channel buffer ic 1
            done
        with End_of_file -> ());
        let _ = Unix.close_process (ic, oc) in
        (Buffer.contents buffer)
    let load_lines fname =
        let lines = ref [] in
        let chan = open_in fname in
        try
            while true; do
                lines := input_line chan :: !lines
            done; !lines
        with End_of_file ->
            close_in chan;
            List.rev !lines
end

(* string aliases for type safety *)
type sort = string [@@deriving of_sexp]
type var = string [@@deriving of_sexp]

(* types for symbols and applied symbols  *)
type symbol = Symbol of string * (sort list) [@@deriving of_sexp]
type relation = Relation of string * (var list)

(* wrapper for relation functions *)
module Relation = struct
    type t = relation
    let inputs = function
        Relation (r, vs) -> List.rev (List.tl (List.rev vs))
    let output = function
        Relation (r, vs) -> List.hd (List.rev vs)
    let to_string = function
        Relation (r, vs) -> r ^ "(" ^ (String.concat ", " vs) ^ ")"
    let size = function
        Relation (r, vs) -> List.length vs
end

(* wrapper for symbol functions *)
module Symbol = struct
    type t = symbol
    let to_string = function
        Symbol (r, ts) -> r ^ " :" ^ (String.concat " -> " ts)
    let arity = function
        | Symbol (r, ts) -> (List.length ts) - 1
    let apply s vars = match s with
        Symbol (r, ts) -> Relation (r, vars)
    let sorts s = match s with
        Symbol (r, ts) -> ts
end

(* dependence graph gives us input, output, and local vars *)
module VarMap = Map.Make(struct type t = var let compare = compare end)
module DependenceGraph = struct

    type t = (var list) VarMap.t

    let empty = VarMap.empty

    (* build up graph by inserting edges one at a time *)
    let insert source dest graph =  try
        VarMap.add source (dest :: (VarMap.find source graph)) graph
        with Not_found -> VarMap.add source (dest :: []) graph

    let insert_relation rel graph =
        let dest = Relation.output rel in
            let f g v = insert v dest g in
                List.fold_left f graph (Relation.inputs rel)

    (* extract all the nodes in the graph by seeing which have edges *)
    let sources graph = match (List.split (VarMap.bindings graph)) with
        (l, _) -> l

    let destinations graph = match (List.split (VarMap.bindings graph)) with
        (_, r) -> List.sort_uniq compare (List.concat r)

    let nodes graph = match (List.split (VarMap.bindings graph)) with
        (l, r) -> List.sort_uniq compare (l @ List.concat r)

    let self_loop (l, r) = (l == r)

    let edges graph = Aux.flat_map
            (fun (v, xs) -> List.map (fun x -> (v, x)) xs)
        (VarMap.bindings graph)

    let nt_edges graph = List.filter (fun e -> not (self_loop e)) (edges graph)

    let output_nodes graph = Aux.subtract (nodes graph)
        (List.map fst (nt_edges graph))

    let input_nodes graph = Aux.subtract (nodes graph)
        (List.map snd (nt_edges graph))

    (* which nodes are for output, and which are for input? *)
    (* let is_output n graph = not (List.mem n (sources graph))

    let is_input n graph = not (List.mem n (destinations graph))

    let output_nodes graph = let f n = is_output n graph in
        List.filter f (destinations graph)

    let input_nodes graph = let f n = is_input n graph in
        List.filter f (sources graph) *)
end

(* type for representing conjuntion of relations *)
type cube = Cube of relation list

module Cube = struct
    type t = cube
    let extend c r = match c with Cube rs -> Cube (r :: rs)
    let conjoin cl cr = match cl with Cube ls ->
        match cr with Cube rs -> Cube (ls @ rs)
    let length = function
        | Cube xs -> List.length xs
    let size = function
        | Cube xs -> List.fold_left (+) 0 (List.map Relation.size xs)
    let select c i = match c with
        | Cube xs -> List.nth xs i
    let pop c i =  match c with
        | Cube xs -> Cube (Aux.list_pop xs i)
    let to_string = function
        Cube rs -> String.concat " , " (List.map Relation.to_string rs)
    let empty = Cube []
    let to_graph = function
        Cube rs -> let f g r = DependenceGraph.insert_relation r g in
            List.fold_left f DependenceGraph.empty rs
    let inputs c = DependenceGraph.input_nodes (to_graph c)
    let outputs c = DependenceGraph.output_nodes (to_graph c)
end

(* now we deal with partitions of relations in a cube *)
module RelMap = Map.Make(struct type t = int let compare = compare end)
module StrMap = Map.Make(struct type t = string let compare = compare end)

module Partition = struct
    type t = cube RelMap.t
    let empty = RelMap.empty

    (* count the number of partitions, for finding new labels  *)
    let length part = try
        match (RelMap.max_binding part) with (n, c) -> n
        with Not_found -> 0

    (* add a relation to a partition *)
    let insert_into index rel part = try
        RelMap.add index (Cube.extend (RelMap.find index part) rel) part
        with Not_found -> RelMap.add index (Cube.extend Cube.empty rel) part

    (* shortcut for making a brand new bucket in the partition with rel *)
    let insert rel part = insert_into (length part) rel part

    (* accessing the ith cube in a partition  *)
    let find index part = RelMap.find index part

    (* convert back to a cube *)
    let flatten part =
        let f i cl cr = Cube.conjoin cl cr in
            RelMap.fold f part Cube.empty

    (* get input, output information from partition as a whole *)
    let inputs part = Cube.inputs (flatten part)
    let outputs part = Cube.outputs (flatten part)

    let variables part = DependenceGraph.nodes (Cube.to_graph (flatten part))

    let pop part = RelMap.remove ((length part) - 1) part
    let update part i c = RelMap.add i c part

    let size part =
        let f t = Cube.size (snd t) in
        List.fold_left (+) 0 (List.map f (RelMap.bindings part))

    let to_string part =
        Cube.to_string (flatten part)

    let to_cube_list part =
        List.map snd (RelMap.bindings part)

end
