(** This module implements reasoning about canonical constraints of the form:

    K_1 T_1 /\ ... K_N T_N

    using rules of the form:

    - forall b_1 ... b_N, k t ≡ k_1 t_1 /\ ... k_N t_N   (E)
    - forall b, k b => k_1 b /\ ... k_N b                (E')
*)

open Positions
open Name
open MultiEquation

(** [Unsat] is raised if a canonical constraint C ≡ false. *)
exception Unsat

(** [Equivalent [b1;..;bN] k g <g \vec{b}> [(k_1,t_1);...;(k_No,t_N)]]
    registers a rule of the form (E). *)
val equivalent
  : variable list -> tname -> tname -> crterm -> (tname * variable) list -> unit

(** [canonicalize pos pool c] where [c = [(k_1,t_1);...;(k_N,t_N)]]
    decomposes [c] into an equivalent constraint [c' =
    [(k'_1,v_1);...;(k'_M,v_M)]], introducing the variables
    [v_1;...;v_M] in [pool]. It raises [Unsat] if the given constraint
    is equivalent to [false]. *)
val canonicalize
  : position -> pool -> (tname * variable) list -> (tname * variable) list

(** [add_implication k [k_1;...;k_N]] registers a rule of the form
    (E'). *)
val add_implication
  : tname -> tname list -> unit

(** [entails C1 C2] returns true if the canonical constraint [C1] implies
    the canonical constraint [C2]. *)
val entails
  : (tname * variable) list -> (tname * variable) list -> bool

(** [contains k1 k2] *)
val contains
  : tname -> tname -> bool

val setup_class_rules : TypingEnvironment.environment -> unit

(** [canonicalize_class_predicates ts cps] *)
(* TODO: description *)
val canonicalize_class_predicates
  : tname list -> Types.class_predicates -> Types.class_predicates

