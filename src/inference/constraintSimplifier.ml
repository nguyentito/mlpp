open InferenceTypes
open MultiEquation
open Name

(** [Unsat] is raised if a canonical constraint C ≡ false. *)
exception Unsat

(* There were some exceptions declared below,
   but they're useless if the constraint generator does 
   its job correctly. *)

(* TODO: eliminate global variable by passing it as an argument *)

type class_relations = { 
  mutable equivalences :
    ((tname * variable), (variable list * (tname * variable) list)) Env.t;
  mutable implications :
    (tname, tname list) Env.t
}

let big_global_table = { equivalences = Env.empty;
                         implications = Env.empty }

let retrieve_E'_expansion (k, a) =
  try
    List.map (fun k' -> (k',a)) (Env.lookup big_global_table.implications k)
  with
    | Not_found -> []

let equivalent beta k g predicates =
  big_global_table.equivalences <-
    Env.add big_global_table.equivalences (k, g) (beta, predicates)
  

let canonicalize pos pool c = assert false
  

let add_implication k ks =
  big_global_table.implications <-
    Env.add big_global_table.implications k ks


let entails c1 c2 = (* C1 ||- C2 *)
  (* let's do a graph traversal of the entire hierarchy above our constraint! *)

  let visited_table = Hashtbl.create 42 in
  let visited = Hashtbl.mem visited_table
  and mark x = Hashtbl.add visited_table x () in

  (* we have to eta-expand to convince the typechecker
     [traversals] is a value *)
  let rec traversals ps = List.iter traversal ps
  and traversal p = if not (visited p) then begin
    mark p;
    traversals (retrieve_E'_expansion p)
  end in
  
  traversals c1;
  List.for_all (fun p -> visited p) c2


let contains k1 k2 = (* k1 >= k2 *)
  let v = variable Rigid () in
  entails [(k1, v)] [(k2, v)]
  

