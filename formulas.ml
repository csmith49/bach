(* a type *)
type typ = string

(* a variable *)
type var = string


(* typed symbols *)
type symb =
  {
    name: string;
    sign: typ list;
  }

(* arity of a symbol is its number of inputs *)
let arity (s : symb) : int = (List.length s.sign) - 1

(* return type of a symbol *)
let rtype (s : symb) : typ = List.hd (List.rev s.sign)


(* an atom is an application of a symbol to a list of variables *)
type atom =
  {
    s: symb;
    vars: var list
  }

(* applies a symbol to a list of variables, resulting in an atom *)
let apply (s: symb) (v: var list) : atom = { s = s; vars = v }


(* a cube is a conjunction of atoms *)
type cube = atom list


(* a formula is either an equation or an implication *)
type form =
  | Impl cube * cube
  | Eq cube * cube
