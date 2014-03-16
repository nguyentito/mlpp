(***** Naming functions / conventions *****)

open Name

(* TODO: evil clash avoidance *)
(* TODO: unify their way of operation (parameter form, etc) *)
(* TODO: find better names (bitch!) *)
let sconcat = String.concat ""
let uconcat = String.concat "_"
let spconcat = String.concat " "

let superclass_accessor_type_name (TName supcl) (TName cl) =
  LName (uconcat ["superclass_field"; supcl; cl])

(* TODO: if we lift restrictions, the "inst" part won't stay that simple *)
(* TODO: replace all occurrences of this by dictionary_var_name? *)
let superinstance_var_name (TName supinst) (TName inst) =
  Name (uconcat ["superinstance_var"; supinst; inst])

(* TODO: should take a class predicate as argument instead... *)
let dictionary_var_name (TName cl_name) (TName tvar_name) =
  Name (uconcat ["dictionary_var"; cl_name; tvar_name])

let class_to_type_name (TName cl_name) =
  TName (uconcat ["class_type"; cl_name])

let instance_to_dict_name (TName cl_name) (TName inst_name) =
  Name (uconcat ["inst_dict"; cl_name; inst_name])
