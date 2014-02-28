open Positions
open Name
open XAST
open Types
open ElaborationExceptions

module TMap = Map.Make(OrderedTName)
module LMap = Map.Make(OrderedLName)
module NMap = Map.Make(OrderedName)
module NSet = Set.Make(OrderedName)

module InstanceMap = Map.Make
  (
    struct
      type t = type_class_name * type_constr_name (* class name * instance *)
      let compare = compare
    end
  )

(*
type t = {
   values       : (tnames * binding) list;
   types        : (tname * (Types.kind * type_definition)) list;
   classes      : (tname * class_definition) list;
   labels       : (lname * (tnames * Types.t * tname)) list;
}
*)

type t = {
  values       : mltypescheme NMap.t;
  types        : (Types.kind * type_definition) TMap.t;
  labels       : (tnames * Types.t * tname) LMap.t;
  classes      : class_definition TMap.t;
  instances    : instance_definition InstanceMap.t
}

let empty = { values    = NMap.empty;
              types     = TMap.empty;
              classes   = TMap.empty;
              labels    = LMap.empty;
              instances = InstanceMap.empty }

(* TODO: modify values and lookup + occurrences in ElaborateDictionaries
   for now, we have something which maintains compatibility
   with previous code
   TODO as well: re-examine the code which handles pattern branches
   in ElaborateDictionaries *)
let values env =
  List.map (fun (x, TyScheme (ts, _, ty)) -> (ts, (x, ty)))
           (NMap.bindings env.values)

let lookup pos x env =
  try
    let TyScheme (ts, _, ty) = NMap.find x env.values in
    (ts, (x, ty))
  with Not_found -> raise (UnboundIdentifier (pos, x))

let bind_scheme x ts ps ty env = 
  { env with values = NMap.add x (TyScheme (ts, ps, ty)) env.values }

let bind_simple x ty env =
  bind_scheme x [] [] ty env

let bind_type t kind tdef env =
  { env with types = TMap.add t (kind, tdef) env.types }

let lookup_type pos t env =
  try
    TMap.find t env.types
  with Not_found ->
    raise (UnboundTypeVariable (pos, t))

let lookup_type_kind pos t env =
  fst (lookup_type pos t env)

let lookup_type_definition pos t env =
  snd (lookup_type pos t env)

let lookup_class pos k env =
  try
    TMap.find k env.classes
  with Not_found -> raise (UnboundClass (pos, k))

let bind_class k c env =
  try
    let pos = c.class_position in
    ignore (lookup_class pos k env);
    raise (AlreadyDefinedClass (pos, k))
  with UnboundClass _ ->
    { env with classes = TMap.add k c env.classes }

let lookup_superclasses pos k env =
  (lookup_class pos k env).superclasses

let rec is_superclass pos k1 k2 env =
  k1 = k2 || List.exists (fun k1' -> is_superclass pos k1' k2 env)
                         (lookup_superclasses pos k1 env)

let bind_type_variable t env =
  bind_type t KStar (TypeDef (undefined_position, KStar, t, DAlgebraic [])) env

let labels_of rtcon env =
  let p (_, (_, _, rtcon')) = rtcon = rtcon' in
  List.(fst (split (filter p env.labels)))

let lookup_label pos l env =
  try
    LMap.find l env.labels
  with Not_found ->
    raise (UnboundLabel (pos, l))

let bind_label pos l ts ty rtcon env =
  try
    ignore (lookup_label pos l env);
    raise (LabelAlreadyTaken (pos, l))
  with UnboundLabel _ ->
    { env with labels = LMap.add l (ts, ty, rtcon) env.labels }

let initial =
  let primitive_type t k = TypeDef (undefined_position, k, t, DAlgebraic []) in
  List.fold_left (fun env (t, k) ->
    bind_type t k (primitive_type t k) env
  ) empty [
    (TName "->", KArrow (KStar, KArrow (KStar, KStar)));
    (TName "int", KStar);
    (TName "char", KStar);
    (TName "unit", KStar)
  ]

let bind_instance inst env =
  let pos = inst.instance_position
  and inst_key = (inst.instance_class_name, inst.instance_index) in
  begin
    (* In this simplified system, checking for overlapping is trivial:
       just compare the head constructor *)
    if InstanceMap.mem inst_key env.instances
    then raise (OverlappingInstances (pos, inst.instance_index))
  end;
  { env with instances = InstanceMap.add inst_key inst env.instances }


(* let lookup_instance pos inst_key env = *)
(*   (\* In this simplified system, as there is no overlapping, there is *)
(*      at most one instance "proof" derivation, and thus, every instance *)
(*      involved in one derivation is (necessarily) mandatory *\) *)
(*   try *)
(*     TMap.find inst_key env.instances *)
      
(*   (\* CHECK: i'm not sure this is the right exception *\) *)
(*   with Not_found -> raise (CannotElaborateDictionnary (pos, assert false (\* TODO *\))) *)

let lookup_instance inst_key env =
  (* In this simplified system, as there is no overlapping, there is
     at most one instance "proof" derivation, and thus, every instance
     involved in one derivation is (necessarily) mandatory *)
  try
    Some (InstanceMap.find inst_key env.instances)
      
  (* CHECK: i'm not sure this is the right exception *)
  with Not_found -> None
