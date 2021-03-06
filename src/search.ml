open Core
open Frontier
open Problem

let new_relation var_in var_out s =
    List.map
        (Symbol.apply s)
        (Aux.cart_prod (
            (Aux.list_copy var_in (Symbol.arity s)) @ [var_out]))

let metric p = Partition.size p

module SearchablePartition = struct
    type t = Partition.t

    let compare a b = Pervasives.compare (metric a) (metric b)

    let children p = begin
        let output = ref [] in
        let process_cube p t = match t with
            | (i, c) ->
                let outs = Cube.inputs c in
                let ins = Aux.subtract !Problem.globals.variables (Cube.outputs c) in
                let relations = Aux.flat_map (new_relation ins outs) !Problem.globals.signature in
                output := !output @ (List.map
                        (fun r -> Partition.insert_into i r p)
                    relations)
        in List.iter (process_cube p) (RelMap.bindings p);
        if (Partition.length p) < !Problem.globals.max_terms then
            let p_vars = Aux.subtract
                !Problem.globals.variables
                (Aux.subtract
                    (Partition.variables p)
                    ((Partition.inputs p) @ (Partition.outputs p)))
            in let relations = Aux.flat_map (new_relation p_vars p_vars) !Problem.globals.signature in
            output := !output @ (List.map
                    (fun r -> Partition.insert r p)
                relations)
        else ();
        !output
    end

    let parents p = begin
        let output = ref [] in
        let process_cube p t = match t with
            | (i, c) ->
                if (Cube.length c) > 1 then
                    begin
                        for j = 0 to (Cube.length c) do
                            let r = Cube.select c j in
                            if not (List.mem (Relation.output r) (Cube.outputs c)) then
                                let small_c = Cube.pop c j in
                                let small_p = Partition.update p j small_c in
                                    output := small_p :: !output;
                        done;
                    end
                else if i == (Partition.length p) - 1 then
                    output := (Partition.pop p) :: !output;
        in List.iter (process_cube p) (RelMap.bindings p);
        !output
    end
end

module Search = Deadbeat(SearchablePartition)
