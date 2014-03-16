(***** Naming functions / conventions *****)

open Name

let sconcat = String.concat ""
let uconcat = String.concat "_"
let spconcat = String.concat " "


(* This functions allows to avoid name clashes
   For instance, if we have K A_B and K_A B, they should both give name K_A_B
   This function ensures that the first will be named K_A_B, then the second K_A_B_1, etc,
   to avoid the clash.
*)
let get_unique_id hashtbl (TName name1) (TName name2) =
  let base_name = name1 ^ "_" ^ name2
  in

  let rec f first n =
    let cur_name =
      if first then base_name
      else base_name ^ "_" ^ (string_of_int n)
    in
    if Hashtbl.mem hashtbl cur_name then
      begin
        let (n1, n2) =  Hashtbl.find hashtbl cur_name
        in
        if n1 = name1 && n2 = name2 then cur_name
        else
          f false (n + 1)
      end
    else
      begin
        Hashtbl.add hashtbl cur_name (name1, name2);
        cur_name
      end
  in
  f true 0


let superclass_accessor_type_name =
  let tbl = Hashtbl.create 277 in
  fun supcl cl ->
    LName (uconcat ["superclass_field"; get_unique_id tbl supcl cl])

(* TODO: if we lift restrictions, the "inst" part won't stay that simple *)
(* TODO: replace all occurrences of this by dictionary_var_name? *)
let superinstance_var_name =
  let tbl = Hashtbl.create 277 in
  fun supinst inst ->
    Name (uconcat ["superinstance_var"; get_unique_id tbl supinst inst])

(* TODO: should take a class predicate as argument instead... *)
let dictionary_var_name =
  let tbl = Hashtbl.create 277 in
  fun cl_name tvar_name ->
    Name (uconcat ["dictionary_var"; get_unique_id tbl cl_name tvar_name])

let class_to_type_name (TName cl_name) =
  TName (uconcat ["class_type"; cl_name])

let instance_to_dict_name =
  let tbl = Hashtbl.create 277 in
  fun cl_name inst_name ->
    Name (uconcat ["inst_dict"; get_unique_id tbl cl_name inst_name])
