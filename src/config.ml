open Core
open Sexplib.Std

type config = {
    max_terms : int [@default 2];
    variables : var list;
    symbols : symbol list;
} [@@deriving of_sexp]
