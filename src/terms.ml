open Core
open Frontier
open Problem

(* counter for fresh variables *)
let fresh_var_counter = ref 0
let fresh_var _ = begin
    incr fresh_var_counter;
    "v_" ^ (string_of_int !fresh_var_counter)
end

(* we search over terms, although the top-most node has a var *)
type term = Var of var | App of symbol * term list
type root = Root of term * var
type multiterm = root list

module Term = struct
    type t = term
    type position = int list

    exception Bad_position
    (* printing stuff *)
    let pos_to_string p = String.concat ":" (List.map string_of_int p)
    let rec to_string (t: term) = match t with
        | Var v -> v
        | App (s, ts) ->
            (Symbol.name s) ^ "(" ^
                (String.concat ", " (List.map to_string ts))
             ^ ")"
    (* lets manipulate some positions *)
    let rec get_positions (t : term): position list = match t with
        | Var _ -> [[]]
        | App (_, ts) -> [] :: (List.concat
            (List.mapi (fun i t ->
                    List.map (fun p -> i :: p) (get_positions t))
                ts))

    let rec at_position (t : term) (p : position) =
        match p with
            | [] -> t
            | i :: rest -> match t with
                | Var _ -> raise Bad_position
                | App (_, ts) -> at_position (
                    try
                        List.nth ts i
                    with
                    | _ -> failwith
                        (
                            (to_string t) ^ " | " ^ (String.concat ", " (List.map pos_to_string (get_positions t)))
                        )
                    ) rest

    let rec set_position (t: term) (p: position) (nt: term): term = match p with
        | [] -> nt
        | i :: rest -> match t with
            | Var _ -> raise Bad_position
            | App (s, ts) -> let nts =
                    List.mapi (fun j t ->
                        if i == j then
                            set_position t rest nt
                        else t) ts
                in App (s, nts)
    let size t = List.length (get_positions t)
    (* the helper maps terms to cube * var *)
    let rec cube_rep t = match t with
        | Var v -> Cube.empty, v
        | App (s, ts) ->
            let cs, vs = List.split (List.map cube_rep ts) in
            let v = fresh_var () in
            let c = Symbol.apply s (vs @ [v]) in
            let csi = Aux.flat_map
                (fun x -> match x with
                    Cube rs -> rs)
                cs in
            (Cube (csi @ [c]), v)
end

module Root = struct
    (* our base type, no surprise *)
    type t = root
    (* stuff to help the search *)
    let variable_positions r = match r with
        Root (t, v) -> List.filter (fun p ->
                match (Term.at_position t p) with
                    | Var v -> true
                    | App (_, _) -> false
            ) (Term.get_positions t)
    (* wrapper for term set position *)
    let set_position (r: root) (p: Term.position) (nt: term): root = match r with
        Root (t, v) -> Root (Term.set_position t p nt, v)
    (* utility function *)
    (* while the function ingnores the top-most output var *)
    let cube_rep r = match r with
        Root (t, v) -> match t with
            | Var v -> Cube.empty
            | App (s, ts) ->
                let cs, vs = List.split (List.map Term.cube_rep ts) in
                let c = Symbol.apply s (vs @ [v]) in
                let csi = Aux.flat_map
                    (fun x -> match x with Cube rs -> rs) cs
                in Cube (csi @ [c])
    (* iterates over all nodes of the term collecting root vars *)
    let rec input_vars_helper t = match t with
        | Var vp -> [vp]
        | App (s, ts) -> Aux.flat_map input_vars_helper ts
    let input_vars r = match r with
        Root (t, v) -> input_vars_helper t
    (* converts root -> (cube, inputs, output) *)
    let to_cube r = match r with
        Root (t, v) -> (cube_rep r, input_vars r, v)
    (* now lets print *)
    let to_string = function
        Root (t, v) -> (Term.to_string t) ^ " = " ^ v
    let size = function
        Root (t, v) -> Term.size t
end

module Multiterm = struct
    type t = multiterm
    (* TODO : extend this appropriately *)
    let size mt = List.fold_left (+) 0 (List.map Root.size mt)
    let metric mt = (size mt, mt)
    let compare a b =
        let x = metric a in
        let y = metric b in
            Pervasives.compare x y
    (* we're doing index manipulations that might fail *)
    exception Bad_index
    (* see? *)
    let at_index m i = List.nth m i
    let rec set_index m i n = match m with
        | [] -> if i == 0 then [n] else raise Bad_index
        | r :: rs -> if i == 0 then n :: rs else r :: (set_index rs (i - 1) n)
    (* how big is this thign *)
    let size m = List.length m
    (* set_index with special index value *)
    let add_to_end m n = let l = size m in
        set_index m l n
    (* and now lets print *)
    let to_string mt = String.concat " & " (List.map Root.to_string mt)
    (* makes all terms that are f(vars) for some f, some vars *)
    let depth_one_terms (vars: (var list) SortMap.t) =
        let vars_from_sorts ss = List.map (fun s -> SortMap.find s vars) ss in
        let terms_from_vars vs = List.map (fun v -> Var v) vs in
        Aux.flat_map (fun s ->
            List.map (fun vs ->
                    App (s, terms_from_vars vs))
            (Aux.cart_prod (vars_from_sorts (Symbol.inputs s))))
        !Problem.globals.signature
    (* wraps depth one terms with output vars *)
    let depth_one_roots (vars: (var list) SortMap.t) =
        let ts = depth_one_terms vars in
        Aux.flat_map (fun t -> match t with
                | App (s, vs) -> List.map (fun v ->
                        Root (t, v))
                    (SortMap.find (Symbol.output s) vars)
            )
        ts
    (* needed for enumeration *)
    let children mt = begin
        let var_sorts = to_sort_map !Problem.globals.variables in
        let terms = depth_one_terms var_sorts in
        let output = ref [] in
        let process_term mt i r =
            (* and all possible positions in r *)
            let ps = Root.variable_positions r in
            (* so compute all new roots *)
            let rs = List.map (fun (p, t) ->
                    Root.set_position r p t)
                (Aux.cross_prod ps terms) in
            (* and put them in the right spot *)
            List.iter (fun r -> begin
                output := (set_index mt i r) :: !output;
            end) rs
        in List.iteri (process_term mt) mt;
        (* but now we can stick another term on the end *)
        if (size mt) < !Problem.globals.max_terms
            then
                List.iter (fun r ->
                        output := (add_to_end mt r) :: !output)
                    (depth_one_roots var_sorts)
            else
                ();
        !output
    end
    (* TODO: extend this with method of search.ml *)
    let parents r = []
    (* we need to convert our multiterms to cubes with inputs and outputs done right *)
    let to_cube mt =
        let cubes = List.map Root.to_cube mt in
        List.fold_left (fun (cb, ib, ob) (c, i, b) ->
                (Cube.conjoin cb c, i @ ib, b :: ob))
            (Cube.empty, [], []) cubes
end

module TermSearch = Deadbeat(Multiterm)
