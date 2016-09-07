open Core
open Checker
open Printf

let noisy = ref false

let spec_list = [
    ("-noisy", Arg.Set noisy, " Print additional information");
]

let usage_msg = "todo"
let anon_fun s = raise (Arg.Bad (s ^ " is not recognized"))

let _ =
    let t = Sys.time() in

    Arg.parse (Arg.align spec_list) anon_fun usage_msg;

    let s = Symbol ("R", ["int";"int"]) in
    let r = Relation ("R", ["X";"Y"]) in
    let c = Cube [r] in

    (* XXX: bug *)
    printf "# of Inputs: %d\n" (List.length (inputs c));
    print_endline ("This is a symbol: " ^ (string_of_symbol s));

    let m = check [s] [c;c] [c] true in
    
    if !noisy then begin
        print_endline "\n==> STATS ==>";
        Printf.printf "time %fs\n" (Sys.time() -. t);
    end;
