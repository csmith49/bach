open Core
open Checker
open Search
open Printf

let noisy = ref false

let spec_list = [
    ("-noisy", Arg.Set noisy, " Print additional information");
]

let usage_msg = "todo"
let anon_fun s = raise (Arg.Bad (s ^ " is not recognized"))

let _ =
    let f = ref (Search.start Partition.empty) in
    let t = Sys.time() in
    let wrapped_check l r = check !global_config.signature l r true in
    let seen = ref [] in
    let to_cube p = List.map snd (RelMap.bindings p) in

    Arg.parse (Arg.align spec_list) anon_fun usage_msg;

    for i = 0 to 100 do
        (* get the newest element and update the frontier *)
        let e, fp = Search.next !f in
        f := fp;
        (* let's see what we're working with *)
        print_endline (string_of_cube (Partition.flatten e));
        (* for every already-seen cube, we'll make an equation and check *)
        List.iter (fun c -> begin
            wrapped_check (to_cube c) (to_cube e); ()
        end) !seen;
        (* and then update our list of seen cubes *)
        seen := e :: !seen;
    done;

    (* now we wrap up by printing stats that we care about *)
    if !noisy then begin
        print_endline "\n==> STATS ==>";
        Printf.printf "time %fs\n" (Sys.time() -. t);
    end;

    for i = 0 to 10 do
        let e, fp = Search.next !f in
        f := fp;
        print_endline (string_of_cube (Partition.flatten e));
    done;
