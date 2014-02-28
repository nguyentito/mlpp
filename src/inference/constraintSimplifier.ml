open InferenceTypes
open MultiEquation
open Name

(** [Unsat] is raised if a canonical constraint C ≡ false. *)
exception Unsat

(* There were some exceptions declared below,
   but they're useless if the constraint generator does 
   its job correctly. *)

(* TODO: eliminate global variable by passing it as an argument *)

type equivalence_spec = crterm * variable list * (tname * variable) list

type class_relations = { 
  mutable equivalences : (tname * tname, equivalence_spec) Env.t;
  mutable implications : (tname, tname list) Env.t
}

let big_global_table = { equivalences = Env.empty;
                         implications = Env.empty }

(* Destructuring functions which perhaps should not live here... *)

let rec head_constructor v =
  (* Is this right??????? Who knows! *)
  let desc = UnionFind.find v in
  match desc.structure, desc.name with
    | None, Some tname when desc.kind = Constant -> Some tname
    | Some (Var v), _ -> head_constructor v
    | Some (App (v,_)), _ -> let (Some tname) = variable_name v in Some tname
    | _ -> None

(* E-expansion turns a predicate constraint over a type into
   constraints on subtypes, returning None if no expansion is possible *)
(* TODO: what to do about the exceptions potentially raised
   by unify? Upfront checks should prevent them... *)
(* This code is pure superstition. It is a cargo cult ritual meant
   to make magic happen by imitation of existing working code. *)
let e_expand pos pool (k, v) =
  let open Types in
  match head_constructor v with
  | None -> None
  | Some g ->
    try
      let term, vars, expansion =
        Env.lookup big_global_table.equivalences (k, g) in

      let fresh_vars = List.map (fun _ -> variable Flexible ()) vars in
      let fresh_assoc = List.combine vars fresh_vars in
      
      let fresh_term = change_arterm_vars fresh_assoc term 
      and fresh_expansion =
        List.map (fun (k', a) -> (k', List.assq a fresh_assoc)) expansion in

      List.iter (introduce pool) fresh_vars;
      let t = chop pool fresh_term in
      Unifier.unify pos (register pool) v t;

      Some (fresh_expansion)
    with
      | Not_found -> raise Unsat

let e'_expand (k, a) =
  try
    List.map (fun k' -> (k',a)) (Env.lookup big_global_table.implications k)
  with
    | Not_found -> []


let equivalent beta k g gb predicates =
  big_global_table.equivalences <-
    Env.add big_global_table.equivalences
    (k, g) (gb, beta, predicates)
  

let canonicalize pos pool c =
  (* We let the type externalization handle redundancy removal,
     and just expand the constraint until we reach sth unsatisfiable *)
  let rec loop ps =
    List.concat (List.map (fun p -> f p (e_expand pos pool p)) ps)
  and f p = function
    | None    -> [p]
    | Some [] -> []
    | Some expansion -> loop expansion
  in
  loop c
  

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
    traversals (e'_expand p)
  end in
  
  traversals c1;
  List.for_all (fun p -> visited p) c2


let contains k1 k2 =
  let v = variable Rigid () in
  entails [(k2, v)] [(k1, v)]
  

let setup_class_rules env =
  let open TypingEnvironment in
  let setup_class (k, ClassInfo (ks, _, _)) =
    add_implication k ks
  in
  let setup_instance ((k,g), InstanceInfo (beta, predicates, gb)) =
    equivalent beta k g gb predicates
  in
  List.iter setup_class (class_listing env);
  List.iter setup_instance (instance_listing env)

