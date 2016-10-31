open Core
open Frontier
open Problem

(* fresh variable counter and generator *)
let fv_counter = ref 0
let fresh_var _ = begin
    incr fv_counter;
    "v_" ^ (string_of_int !fv_counter)
end

(* our terms don't have variables, just sorts --- we instantiate them during checking *)
type term = Sort of sort | App of symbol * term list
type multiterm = term list

(* terms can be accessed through all the usual ways *)
module Term = struct
    type t = term
    type position = int list
    (* we'll index terms by position, which might be bogus *)
    exception Bad_position
    let rec positions (t: term): position list = match t with
        | Sort _ -> [[]]
        | App (_, ts) -> [] :: (List.concat
            (List.mapi (fun i t ->
                    List.map (fun p -> i :: p) (positions t))
                ts))
    let rec at_position (t: term) (p: position) = match p with
        | [] -> t
        | i :: rest -> match t with
            | Sort _ -> raise Bad_position
            | App (_, ts) -> at_position (List.nth ts i) rest
    let rec set_at (t: term) (p: position) (nt: term): term = match p with
        | [] -> nt
        | i :: rest -> match t with
            | Sort _ -> raise Bad_position
            | App (s, ts) -> let nts =
                List.mapi (fun j t ->
                        if i == j then
                            set_at t rest nt
                        else t) ts
                in App (s, nts)
    (* some sort helper functions *)
    let is_sort t = match t with
        | Sort _ -> true
        | _ -> false
    let sort_positions t = List.filter (fun p ->
            is_sort (at_position t p))
        (positions t)
    let get_sort t = match t with
        | Sort s -> s
        | App (s, _) -> Symbol.output s
    (* sometimes we need to find recently added things *)
    let is_terminal t = match t with
        | Sort _ -> false
        | App (s, ts) -> List.for_all is_sort ts
    let terminal_positions t = List.filter (fun p ->
            is_terminal (at_position t p))
        (positions t)
    (* and of course, number of symbols related to size *)
    let size t = List.length (positions t)
    (* of course, printing is nice sometimes *)
    let rec to_string (t: term) = match t with
        | Sort s -> s
        | App (s, ts) ->
            (Symbol.name s) ^ "(" ^
                (String.concat ", " (List.map to_string ts))
            ^ ")"
    (* we might need a natural way to compare *)
    (* and perhaps a metric focusing on size *)
    let metric x = (size x, x)
    let metric_compare x y = Pervasives.compare (metric x) (metric y)
    (* we'll have to construct some terms occasionally *)
    let from_symbol (s: symbol): term =
        App (s, List.map (fun s' -> Sort s') (Symbol.inputs s))
    let from_sort (s: sort): term list = List.map
            from_symbol
        (List.filter (fun s' ->
                (Symbol.output s') == s)
            !Problem.globals.signature)
    (* now we might worry about the natural search order on terms *)
    let children (t: term): term list =
        let sp = List.map (fun p ->
                (get_sort (at_position t p), p))
            (sort_positions t) in
        Aux.flat_map (fun (s, p) ->
                List.map (fun t' ->
                        set_at t p t')
                    (from_sort s))
            sp
    let parents (t: term): term list =
        let tp = List.map (fun p ->
                (get_sort (at_position t p), p))
            (terminal_positions t) in
        List.map (fun (s, p) ->
                set_at t p (Sort s))
            tp
end

(* when we search, we maintain a multiterm on either side *)
module Multiterm = struct
    type t = multiterm
    (* need this to lift our search higher *)
    let size mt = List.fold_left (+) 0 (List.map Term.size mt)
    (* we do index manipulations, some of which are error-prone *)
    exception Bad_index
    let at_index m i = List.nth m i
    let rec set_at m i n = match m with
        | [] -> if i == 0 then [n] else raise Bad_index
        | r :: rs -> if i == 0
            then n :: rs
            else r :: (set_at rs (i - 1) n)
    (* how many terms we got *)
    let length m = List.length m
    (* now we can add just to the end *)
    let append m n = set_at m (length m) n
    (* I like pretty printing *)
    let to_string m = ""
    (* we'll need to know which sorts we can actually use *)
    (* need to make this a function, or it won't update upon global updates *)
    let allowed_sorts _ = fst (List.split !Problem.globals.variables)
    (* we have a natural partial order that we use for search *)
    let children (mt: multiterm): multiterm list =
        let output = ref [] in
        (* update existing terms *)
        List.iteri (fun i t ->
                let mts = List.map (fun t' ->
                        set_at mt i t')
                    (Term.children t) in
                output := !output @ mts;)
            mt;
        (* and maybe add to the end *)
        if (length mt) < !Problem.globals.max_terms then
            List.iter (fun s ->
                    output := (append mt (Sort s)) :: !output;)
                (allowed_sorts ());
        (* and now we eliminate any that might not be kosher *)
        (* TODO: actually filter *)
        !output
    let parents (mt: multiterm): multiterm list =
        let output = ref [] in
        (* if we see a sort, just remove it, otherwise get term parents *)
        List.iteri (fun i t ->
                match t with
                    | Sort _ ->
                        output := (Aux.delete_at mt i) :: !output;
                    | App (s, ts) ->
                        let mts = List.map (fun t' ->
                                set_at mt i t')
                            (Term.parents t) in
                        output := mts @ !output;)
            mt;
        (* and then return --- probs filter like in children *)
        (* TODO: also filter *)
        !output
end
