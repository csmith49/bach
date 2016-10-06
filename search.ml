open Core
open Frontier

(* GLOBAL_CONFIG holds particular values that help to define the parent and
children functions in SearchablePartition, because I'm not sure how to do
dynamic closures of modules *)

type config = {
    signature : symbol list;
    max_terms : int;
    search_depth : int;
    allowed_variables : var list;
}

let global_config = ref {
    signature = [Symbol ("f", ["x";"x";"x"])];
    max_terms = 20;
    search_depth = 10;
    allowed_variables = ["x";"y";"z"]
}

(* We require quite a few auxillary functions---they're all defined here *)

let rec cart_prod = function
    | [] -> [[]]
    | x :: xs -> let rest = cart_prod xs in
        List.concat (List.map (fun i -> List.map (fun rs -> i :: rs) rest) x)

let flat_map f xs = List.concat (List.map f xs)

let subtract b s = List.filter (fun e -> not (List.mem e s)) b

let list_copy i x = begin
    let copies = ref [] in
    for k = 0 to i do
        copies := x :: !copies;
    done;
    !copies
end

let new_relation v_in v_out s = begin
    let variables = (list_copy ((symbol_arity s) - 1) v_in) @ [v_out] in
    List.map (apply_symbol s) (cart_prod variables)
end

(* TEMPORARY METRIC *)
let metric p =
    let f t = cube_size (snd t) in
    let l = List.fold_left (+) 0 (List.map f (RelMap.bindings p)) in
    let r = 0 in
    (l, r)



(* now we can define our search proper *)

module SearchablePartition = struct
    type t = Partition.t

    let compare a b = Pervasives.compare (metric a) (metric b)

    let children p = begin
        let output = ref [] in
        let partition_outputs = Partition.total_outputs p in
        let partition_inputs = Partition.total_inputs p in
        (* iterate over each cube in the partition to find out where to stick em *)
        let process_cube p t = match t with
            | (i, c) ->
                let possible_outputs = inputs c in
                let possible_inputs = subtract !global_config.allowed_variables (outputs c) in
                let new_rel = new_relation possible_inputs possible_outputs in
                let relations = flat_map new_rel !global_config.signature in
                output := !output @ (List.map (fun r -> Partition.insert_into i r p) relations);
        in List.iter (process_cube p) (RelMap.bindings p);
        if (Partition.num_partitions p) < !global_config.max_terms
        then
            let in_n_out = partition_inputs @ partition_outputs in

            let local_vars = subtract (Partition.variables p) in_n_out in
            let possible = subtract !global_config.allowed_variables local_vars in
            let new_rel = new_relation possible possible in
            let relations = flat_map new_rel !global_config.signature in
            List.iter (fun p -> print_endline (string_of_symbol p)) !global_config.signature;
            output := !output @ (List.map (fun r -> Partition.insert r p) relations);
        else ();
        (* finally, return what we've built *)
        !output
    end

    let parents p = begin
        let output = ref [] in
        (* for every possible index *)
        let process_cube p t = match t with
            | (i, c) ->
                if (cube_length c) > 1
                    then begin
                        for j = 0 to (cube_length c) do
                            let r = (cube_select c j) in
                            if not (List.mem (output_variable r) (outputs c)) then
                                let small_c = cube_pop c j in
                                let small_p = Partition.update_cube p j small_c in
                                output := small_p :: !output;
                        done;
                    end
                else if i == (Partition.num_partitions p) - 1 then
                    output := (Partition.pop_cube p) :: !output;
        in List.iter (process_cube p) (RelMap.bindings p);
        !output;
    end
end

module Search = Deadbeat(SearchablePartition)
