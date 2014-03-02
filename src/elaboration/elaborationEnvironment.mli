(** Typing environment for {!XAST}. *)

open XAST
open Positions
open Name
open Types

(* TODO: transpose lookup functions to match the new way of reporting errors (haskell style)
   -> lookup functions only return an optionnal result
   -> but if an immediate error report is needed, a wrapper function (that also takes a position)
      is provided 
*)

(** The type of environments. *)
type t

(** [empty] is the environment with no binding. *)
val empty : t

(** [initial] contains the builtin type constructors of {!XAST}. *)
val initial : t

(** [values env] projects [env] as an environment of bindings
    that associate type scheme to value identifiers. *)
val values : t -> (tnames * binding) list

(** [lookup pos x env] returns the binding of [x]. *)
val lookup : position -> name -> t -> (tnames * binding)

(** [bind_scheme n ts ps ty e] associates the scheme [forall ts. (ps) => ty]
    to the identifier [n] in [e]. *)
val bind_scheme : name -> tnames -> class_predicates -> Types.t -> t -> t

(** [bind_simple n ty e] associates the type [ty] to
    the identifier [n] in [e]. *)
val bind_simple : name -> Types.t -> t -> t

(** [lookup_predicates pos x env] returns the class predicates on [x]. *)
val lookup_scheme : position -> name -> t -> mltypescheme

(** [lookup_type_kind pos t e] returns the kind of [t] in [e]. *)
val lookup_type_kind : position -> tname -> t -> Types.kind

(** [lookup_type_definition pos t e] returns the type definition
    of [t] in [e]. *)
val lookup_type_definition : position -> tname -> t -> type_definition

(** [bind_type t k tdef e] associates a kind [k] and a type definition
    [tdef] to the type identifier [t] in [e]. *)
val bind_type : tname -> Types.kind -> type_definition -> t -> t

(** [bind_type_variable x e] introduces the type variable [x] in [e]. *)
val bind_type_variable : tname -> t -> t

(** [labels_of rtcon e] returns all the labels of the record [rtcon]. *)
val labels_of : tname -> t -> lname list

(** [lookup_class pos c e] returns the class_definition of [c] in [e]. *)
val lookup_class : position -> tname -> t -> class_definition

(** [is_superclass pos k1 k2 e] returns [true] if [k2] is a superclass of
    [k1] in [e]. (The inequality is non-strict.) *)
val is_superclass : position -> tname -> tname -> t -> bool

(** [bind_class c cdef e] associates a class_definition [cdef] to [c] in [e]. *)
val bind_class : tname -> class_definition -> t -> t

(** [bind_label pos l ts lty rtycon e] associates the type parameters [ts],
    the type [lty] and the record type constructor [rtycon] to the label [l]
    in [e]. *)
val bind_label : position -> lname -> tnames -> Types.t -> tname -> t -> t

(** [lookup_label pos l e] returns the type parameters, the type and
    the record type constructor of the label [l] in [e]. *)
val lookup_label : position -> lname -> t -> tnames * Types.t * tname

(** [bind_instance i e] adds the instance [i] to the environment [e] *)
val bind_instance : instance_definition -> t -> t

(** [lookup_instance i e] returns the instance_definition of [i] in [e]. *)
val lookup_instance
  : (type_class_name * type_constr_name) -> t -> instance_definition option

(** [bind_dictionary p e] tells the environment [e] a dictionary variable
    for the predicate [p] exists *)
val bind_dictionary : class_predicate -> t -> t

(** [lookup_dictionary p e] asks the environment [e] about the existence
    of a dictionary variable for the predicate [p] *)
val lookup_dictionary : class_predicate -> t -> bool

(** Returns a list of all dictionaries *)
val dictionaries : t -> class_predicates

