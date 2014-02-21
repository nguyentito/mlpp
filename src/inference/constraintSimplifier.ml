open InferenceTypes
open MultiEquation
open Name



(** [Unsat] is raised if a canonical constraint C ≡ false. *)
exception Unsat

(** [OverlappingInstances] is raised if two rules of kind (E) overlap. *)
exception OverlappingInstances of tname * variable

(** [MultipleClassDefinitions k] is raised if two rules of kind (I)
    share the same goal. *)
exception MultipleClassDefinitions of tname

(** [UnboundClass k] is raised if the type class [k] occurs in a
    constraint while it is undefined. *)
exception UnboundClass of tname

(** Student! This is your job! You must implement the following functions: *)

(* TODO: think about what rules of kind (I) are, and where
   MultipleClassDefinitions should be raised *)

(* TODO: eliminate global variable by passing it as an argument *)

type class_relations = { 
  mutable equivalences :
    ((tname * variable), (variable list * (tname * variable) list)) Env.t;
  mutable implications :
    (tname, tname list) Env.t
}

let big_global_table = { equivalences = Env.empty;
                         implications = Env.empty }

(** [equivalent [b1;..;bN] k t [(k_1,t_1);...;(k_N,t_N)]] registers
    a rule of the form (E). *)
let equivalent beta k g predicates =
  big_global_table.equivalences <-
    Env.add big_global_table.equivalences (k, g) (beta, predicates)
  

(** [canonicalize pos pool c] where [c = [(k_1,t_1);...;(k_N,t_N)]]
    decomposes [c] into an equivalent constraint [c' =
    [(k'_1,v_1);...;(k'_M,v_M)]], introducing the variables
    [v_1;...;v_M] in [pool]. It raises [Unsat] if the given constraint
    is equivalent to [false]. *)
let canonicalize pos pool c = assert false

(** [add_implication k [k_1;...;k_N]] registers a rule of the form
    (E'). *)
let add_implication k ks =
  big_global_table.implications <-
    Env.add big_global_table.implications k ks


(** [entails C1 C2] returns true is the canonical constraint [C1] implies
    the canonical constraint [C2]. *)
let entails _ _ = true

(** [contains k1 k2] *)
let contains _ _ = true

