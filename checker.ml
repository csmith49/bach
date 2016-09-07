open Core
open Printf

(*
  NOTES:
  assumes that souffle is in ~/git/souffle-lang/src/souffle
  it's hardcoded for now.

  input facts should be in RELNAME.facts file - tab-separated
  (make sure it's actually tabs when testing, in case you're a spaceman)
*)

let syscall cmd =
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let _ = Unix.close_process (ic, oc) in
  (Buffer.contents buf)

let rec makeDummyType n =
  if n == 0 then [] else "T" :: (makeDummyType (n-1))

let makeDummyVar n =
  let rec makeDummyVar' n m =
    if m >= n+1 then [] else ("v"^(string_of_int m)) :: (makeDummyVar' n (m+1))
  in
  makeDummyVar' n 1

(* checker receives a list of symbols and lists of subcubes *)

let check (rs: symbol list) (lhs: cube list) (rhs: cube list) (impl: bool)  =

  let count = ref 0 in

  let cubeRep c name =
    count := !count + 1;
    let ivs = inputs c in
    let ovs = outputs c in
    let arr = (List.length ivs) + (List.length ovs) in
    let sname = name^"_"^(string_of_int !count) in
    let sym  = Symbol (sname, makeDummyType arr) in
    let rel = Relation (sname, ivs @ ovs) in
    (c, string_of_cube c, sym, rel)
  in

  let cubeRepString cr =
    let (c, body, sym, rel) = cr in
      (string_of_relation rel) ^ " :- " ^ body ^ "."
  in

  let relString =
    fun (Symbol (sname, types)) ->
      let vars = makeDummyVar (List.length types) in
      let args = List.map (fun v -> v ^ " : " ^ "T") vars in
        ".decl " ^ sname ^ "(" ^
          (String.concat ", " args) ^ ")"
  in

  let inputRelString s =
    (relString s) ^ " input"
  in


  let outputRelString s =
    (relString s) ^ " output printsize"
  in

  let makePositiveCl lhsReps rhsReps =
    let body =
      Cube (List.map (fun (_,_,_,rel) -> rel) (lhsReps @ rhsReps)) in

    let bodyString = string_of_cube body in

    let ivs = inputs body in
    let ovs = outputs body in
    let arr = (List.length ivs) + (List.length ovs) in

    let sname = "pos" in
    let sym  = Symbol (sname, makeDummyType arr) in
    let rel = Relation (sname, ivs @ ovs) in

    (sym, (string_of_relation rel) ^ " :- " ^ bodyString ^ ".")
  in

  let makeNegativeCls lhsReps rhsReps left =
    let negateRel =
      fun (c, body, sym, rel) ->
        let ov = output_variable rel in
        let ivs = input_variables rel in
        let ovfresh = "fr_" ^ ov in
        let negRel =
          match rel with
            Relation (n,vs) -> Relation (n,(ivs @ [ovfresh]))
        in
        (string_of_relation negRel) ^ (", " ^ ov ^ " != " ^ ovfresh)
    in


    let posbody =
      Cube (List.map (fun (_,_,_,rel) -> rel) lhsReps) in

    let bodyString = string_of_cube posbody in

    let ivs = inputs posbody in
    let ovs = outputs posbody in
    let arr = (List.length ivs) + (List.length ovs) in

    let rhsneg = List.map (fun x -> negateRel x) rhsReps in

    let sname = if left then "lneg" else "rneg" in
    let sym  = Symbol (sname, makeDummyType arr) in
    let rel = Relation (sname, ivs @ ovs) in

    (sym,
      List.map (fun x ->
        (string_of_relation rel) ^ " :- " ^ bodyString ^ ", " ^ x ^ ".")
        rhsneg)


    (* for each nagated rhs atom, create a clause *)

  in

  let lhsReps = List.map (fun c -> cubeRep c "lhs") lhs in
  let rhsReps = List.map (fun c -> cubeRep c "rhs") rhs in

  let (psym, pcl) = makePositiveCl lhsReps rhsReps in

  (* dump to file *)
  let oc = open_out "temp.dl" in

  (* TODO: for now, im assuming everything is type T *)
  fprintf oc ".type T\n\n";

  (* declare input relations *)
  List.iter (fun x -> fprintf oc "%s\n" (inputRelString x)) rs;

  (* declare output relations LHS *)
  fprintf oc "\n\n//LHS programs:\n";

  let orelsLhs = List.map (fun (_,_,s,_) -> s) lhsReps in
  List.iter (fun x -> fprintf oc "%s\n" (outputRelString x)) orelsLhs;

  List.iter (fun x -> fprintf oc "%s\n" (cubeRepString x)) lhsReps;

  (* declare output relations RHS *)
  fprintf oc "\n\n//RHS programs:\n";

  let orelsRhs = List.map (fun (_,_,s,_) -> s) rhsReps in
  List.iter (fun x -> fprintf oc "%s\n" (outputRelString x)) orelsRhs;

  List.iter (fun x -> fprintf oc "%s\n" (cubeRepString x)) rhsReps;

  (* declare output relation Positive Evidence *)
  fprintf oc "\n\n//Positive evidence:\n";
  fprintf oc "%s\n" (outputRelString psym);
  fprintf oc "%s\n" pcl;

  let (lnsym, lnegClsList) = makeNegativeCls lhsReps rhsReps true in

  fprintf oc "\n\n//Left Negative evidence:\n";
  fprintf oc "%s\n" (outputRelString lnsym);
  List.iter (fun clsStr -> fprintf oc "%s\n" clsStr) lnegClsList;

  if not impl then begin
    let (rnsym, rnegClsList) = makeNegativeCls rhsReps lhsReps false in

    fprintf oc "\n\n//Right Negative evidence:\n";
    fprintf oc "%s\n" (outputRelString rnsym);
    List.iter (fun clsStr -> fprintf oc "%s\n" clsStr) rnegClsList;
  end;

  close_out oc;

  let souffle = "~/git/souffle-lang/src/souffle -D /tmp" in
  let s = syscall (souffle ^ " temp.dl") in
  let sl = Str.split (Str.regexp "\n") s in
  let slsp = List.map (fun x -> Str.split (Str.regexp "\t") x) sl in

  let m = StrMap.empty in
  let res = List.fold_left
    (fun y x ->
      StrMap.add (List.nth x 0) (int_of_string (List.nth x 1)) y)
      m slsp
  in
  res
