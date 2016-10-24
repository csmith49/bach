open Core
open Sexplib.Std

(* this section lets us define the search space in terms of s-expressions *)
type search_params = {
    max_terms : int [@default 2];
    variables : (sort * (var list)) list;
    signature : symbol list;
} [@@deriving of_sexp]

module SearchParams = struct
    type t = search_params

    let of_string s = [%of_sexp : search_params] (Sexplib.Sexp.of_string s)
end

let globals = ref {
    max_terms = 2;
    variables = [];
    signature = [];
}

let parse_problem_file fname =
    globals := SearchParams.of_string (String.concat " " (Aux.load_lines fname))

let to_sort_map (vs : (sort * (var list)) list) =
    List.fold_left (fun s (k, v) -> SortMap.add k v s) SortMap.empty vs

(* and this lets us provide a config file to parameterize the connections *)
type work_params = {
    souffle : string;
    work_dir : string [@default "/tmp/"];
    work_file : string [@default "tmp.dl"];
} [@@deriving of_sexp]

module WorkParams = struct
    type t = work_params
    let of_string s = [%of_sexp : work_params] (Sexplib.Sexp.of_string s)
end

let work_globals = ref {
    souffle = "";
    work_dir = "/tmp/";
    work_file = "tmp.dl";
}

let parse_work_file fname =
    work_globals := WorkParams.of_string (String.concat " " (Aux.load_lines fname))

(* finally, we have some more globals we might want to mess with *)
let fact_dir = ref ""
