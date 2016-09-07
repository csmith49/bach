open Core

let noisy = ref false

let spec_list = [
    ("-noisy", Arg.Set noisy, " Print additional information");
]

let usage_msg = "todo"
let anon_fun s = raise (Arg.Bad (s ^ " is not recognized"))

let _ =
    let t = Sys.time() in
    Arg.parse (Arg.align spec_list) anon_fun usage_msg;

    if !noisy then begin
        print_endline "\n==> STATS ==>";
        Printf.printf "time %fs\n" (Sys.time() -. t);
    end;
