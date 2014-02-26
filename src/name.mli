(** Name scopes. *)

(** Program identifiers. *)
type name = Name of string

(** Data constructor identifiers. *)
type dname = DName of string

(** Label identifiers. *)
type lname = LName of string

(** Type identifiers. *)
type tname = TName of string

module type OrderedType = Map.OrderedType
module OrderedName  : OrderedType with type t = name
module OrderedLName : OrderedType with type t = lname
module OrderedTName : OrderedType with type t = tname

