open Core
open Sexplib.Std

type params = {
    max_terms : int [@default 2];
    variables : (sort * (var list)) list;
    signature : symbol list;
} [@@deriving of_sexp]

module Params = struct
    type t = params

    let of_string s = [%of_sexp : params] (Sexplib.Sexp.of_string s)
end

let globals = ref {
    max_terms = 2;
    variables = [];
    signature = [];
}

let parse_problem_file fname =
    globals := Params.of_string (String.concat " " (Aux.load_lines fname))

let to_sort_map (vs : (sort * (var list)) list) =
    List.fold_left (fun s (k, v) -> SortMap.add k v s) SortMap.empty vs
