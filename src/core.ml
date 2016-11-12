open Sexplib.Std

(* aux functions go here *)
module Aux = struct
    let rec list_pop xs i = match xs with
        | [] -> []
        | y :: ys -> if i == 0
            then ys
            else y :: (list_pop ys (i - 1))
    let cross_prod l r =
        List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) r) l)
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
    let delete_at l i =
        if i < 0 || i >= (List.length l) then
            invalid_arg "delete_at"
        else let rec del l' i' =
            match l' with
                | [] -> []
                | h :: t when i = 0 -> t
                | h :: t -> h :: (del t (i' - 1))
            in del l i
    let rec drop_from l i = if i == 0
        then l
        else drop_from (List.tl l) (i - 1)
    let rec set_at m i n = match m with
        | [] -> if i == 0 then [n] else invalid_arg "set_at"
        | r :: rs -> if i == 0
            then n :: rs
            else r :: (set_at rs (i - 1) n)
    let append l n =
        List.rev (n :: (List.rev l))
    let rec take_from l i = if i == 0
        then []
        else (List.hd l) :: (take_from (List.tl l) (i - 1))
    let rec rev_cons ls = match ls with
        | [] -> invalid_arg "rev_cons"
        | x :: [] -> ([], x)
        | x :: xs -> let tl, hd = rev_cons xs in
            (x :: tl, hd)
    let concat ls =
        String.concat ", " ls
    let wait _ = begin
        let _ = input_line stdin in ()
    end
    let intersect ls rs = List.filter (fun l -> List.mem l rs) ls
    (* we need transpose for picking out columns in the data *)
    let rec transpose l = match l with
        | [] -> []
        | [] :: xs -> transpose xs
        | (x::xs) :: xss ->
            (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)
    let contains l r = List.for_all (fun e -> List.mem e l) r
    let upto i =
        let rec upto' v =
            if v == i then v :: []
            else v :: (upto' (v + 1))
        in upto' 0
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
    let inputs s = List.rev (List.tl (List.rev (sorts s)))
    let output s = List.hd (List.rev (sorts s))
    let name = function
        Symbol (r, _) -> r
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
end

(* maps are the best *)
module RelMap = Map.Make(struct type t = int let compare = compare end)
module StrMap = Map.Make(struct type t = string let compare = compare end)
module SortMap = Map.Make(struct type t = sort let compare = compare end)
module VarMap = Map.Make(struct type t = var let compare = compare end)

(* terms, can't live with em or without em *)
type ('a, 'b) term = L of 'a | N of 'b * (('a, 'b) term) list

module Term = struct
    type position = int list
    exception Bad_position
    (* now we get to actually manipulate some of this stuff *)
    let rec positions t: position list = match t with
        | L _ -> [[]]
        | N (_, ts) -> [] :: (List.concat
            (List.mapi (fun i t ->
                    List.map (fun p -> i :: p) (positions t))
                ts))
    let rec at_position t p = match p with
        | [] -> t
        | i :: rest -> match t with
            | L _ -> raise Bad_position
            | N (_, ts) -> at_position (List.nth ts i) rest
    let rec set_at t p nt = match p with
        | [] -> nt
        | i :: rest -> match t with
            | L _ -> raise Bad_position
            | N (s, ts) -> let nts =
                List.mapi (fun j t ->
                        if i == j then
                            set_at t rest nt
                        else t) ts
                    in N (s, nts)
    (* of course, we can filter some positions *)
    let filter (f: ('a, 'b) term -> bool)
               (t: ('a, 'b) term): position list =
        let ps = List.sort Pervasives.compare (positions t) in
        List.filter (fun p -> f (at_position t p)) ps
    (* terminal means not leaf, but all children are leaves *)
    let is_leaf t = match t with
        | L _ -> true
        | N (_, _) -> false
    let is_terminal t = match t with
        | L _ -> false
        | N (_, ts) -> List.for_all is_leaf ts
    (* and do some modifications of the temrs *)
    let rec cata (f: 'a -> 'p)
                 (g: 'b -> 'q)
                 (t: ('a, 'b) term): ('p, 'q) term = match t with
        | L v -> L (f v)
        | N (n, ts) ->
            let n' = g n in
            let ts' = List.map (cata f g) ts in
                N (n', ts')
    let pos_map (f : ('a, 'b) term -> 'c)
                (t : ('a, 'b) term)
                (ps : position list): 'c list =
        List.map (fun p -> f (at_position t p)) ps
    let size t = List.length (positions t)
    let node_values (t : ('a, 'b) term) : 'b list =
        let inner_positions = filter (fun t -> not (is_leaf t)) t in
        pos_map (fun t' -> match t' with
                    | L _ -> invalid_arg "uhh"
                    | N (s, ts) -> s) t inner_positions
end
