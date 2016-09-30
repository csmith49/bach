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
    signature = [];
    max_terms = 2;
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

(* now we can define our search proper *)

module SearchablePartition = struct
    type t = Partition.t

    let compare a b = Pervasives.compare a b

    let children p = begin
        let output = ref [] in
        let partition_outputs = Partition.total_outputs p in
        let partition_inputs = Partition.total_inputs p in
        (* iterate over each cube in the partition to find out where to stick em *)
        for i = 0 to (Partition.num_partitions p) do

            let c = Partition.get_cube i p in
            let possible_outputs = inputs c in
            let possible_inputs =
                subtract !global_config.allowed_variables (outputs c) in
            let new_rel = new_relation possible_inputs possible_outputs in
            let relations = flat_map new_rel !global_config.signature in

            output := !output @ (List.map (fun r -> Partition.insert_into i r p) relations);

        done;
        (* TODO -- check here for max terms allowed *)
        let in_n_out = partition_inputs @ partition_outputs in
        let local_vars = subtract (Partition.variables p) in_n_out in
        let possible = subtract !global_config.allowed_variables local_vars in
        let new_rel = new_relation possible possible in
        let relations = flat_map new_rel !global_config.signature in

        output := !output @ (List.map (fun r -> Partition.insert r p) relations);

        (* finally, return what we've built *)
        !output
    end

    let parents p = begin
        let output = ref [] in
        let last_index = (Partition.num_partitions p) - 1 in
        (* for every possible index *)
        for i = 0 to (last_index + 1) do
            (* consider the cube at that index *)
            let c = Partition.get_cube i p in
            (* if the cube has more than one relation *)
            if (cube_length c) > 1
                then begin
                    (* let's try removing the ones that aren't at the top *)
                    for j = 0 to (cube_length c) do
                        let r = (cube_select c j) in
                        if not (List.mem (output_variable r) (outputs c)) then
                            let small_c = cube_pop c j in
                            let small_p = Partition.update_cube p j small_c in
                            output := small_p :: !output;
                    done;
                end
            (* otherwise, we might still remove the last cube entirely *)
            else if i == last_index then
                output := (Partition.pop_cube p) :: !output;
        done;
        !output
    end
end

module Search = Deadbeat(SearchablePartition)
