open Positions
open Name


type kind =
  | KStar
  | KArrow of kind * kind

type type_var_name = tname
type type_constr_name = tname
type type_class_name = tname


type  mltype =
  | TyVar        of position * type_var_name
  | TyApp        of position * type_constr_name * mltype list

(* TODO: mltype (here ↑) has to depend on the following type *)
type mltype' =
  | TyVar' of type_var_name
  | TyApp' of type_constr_name * mltype' list

type scheme = TyScheme of type_var_name list * class_predicates * mltype

and class_predicate = ClassPredicate of type_class_name * type_var_name
and class_predicates = class_predicate list


let tyarrow pos ity oty =
  TyApp (pos, TName "->", [ity; oty])

let ntyarrow pos itys oty =
  List.fold_left (fun oty ity -> tyarrow pos ity oty) oty (List.rev itys)

let destruct_tyarrow = function
  | (TyApp (_, TName "->", [ity; oty])) ->
    Some (ity, oty)
  | ty -> None

let rec destruct_ntyarrow ty =
  match destruct_tyarrow ty with
    | None -> ([], ty)
    | Some (i, o) -> let (is, o) = destruct_ntyarrow o in (i :: is, o)

type instantiation_kind =
  | TypeApplication of mltype list
  | LeftImplicit

module type TypingSyntax = sig
  type binding

  val binding
    : Positions.position -> name -> mltype option -> binding

  val destruct_binding
    : binding -> name * mltype option

  type instantiation

  val instantiation
    : Positions.position -> instantiation_kind -> instantiation

  val destruct_instantiation_as_type_applications
    : instantiation -> mltype list option

  val implicit : bool

end

module ImplicitTyping =
struct
  type binding = Name.name * mltype option

  let binding _ x ty : binding = (x, ty)

  let destruct_binding b = b

  type instantiation = mltype option

  let instantiation pos = function
    | TypeApplication _ ->
      Errors.fatal' pos "No type application while being implicit."
    | LeftImplicit ->
      None

  let destruct_instantiation_as_type_applications _ = None

  let implicit = true

end

module ExplicitTyping =
struct
  type binding = Name.name * mltype

  let binding pos x = function
    | None -> Errors.fatal' pos "An explicit type annotation is required."
    | Some ty -> (x, ty)

  let destruct_binding (x, ty) = (x, Some ty)

  type instantiation = mltype list

  let instantiation pos = function
    | LeftImplicit ->
      Errors.fatal' pos "An explicit type application is required."
    | TypeApplication i -> i

  let destruct_instantiation_as_type_applications i = Some i

  let implicit = false
end

let rec kind_of_arity = function
  | 0 -> KStar
  | n -> KArrow (KStar, kind_of_arity (pred n))


(* FIXME: not sure we'll use it...*)
let rec arity_of_kind = function
  | KStar -> 0
  | KArrow (KStar, k) -> 1 + arity_of_kind k
  | _ -> assert false (* FIXME: ? *)

let rec equivalent ty1 ty2 =
  match ty1, ty2 with
    | TyVar (_, t), TyVar (_, t') ->
      t = t'
    | TyApp (_, t, tys), TyApp (_, t', tys') ->
      t = t' && List.for_all2 equivalent tys tys'
    | _, _ ->
      false

let rec substitute (s : (type_var_name * mltype) list) = function
  | TyVar (p, v) ->
    (try List.assoc v s with Not_found -> TyVar (p, v))

  | TyApp (pos, t, tys) ->
    if not (Fts.on ())
    then TyApp (pos, t, List.map (substitute s) tys)
    else begin let t' =
                 try
                   match List.assoc t s with
                     | TyVar (_, v) -> v
                     | TyApp (_, t, []) -> t
                     | _ -> assert false (* TODO: think harder *)
                 with Not_found -> t
               in
               TyApp (pos, t', List.map (substitute s) tys)
    end
      

(*************************************)

module TSet = Set.Make(OrderedTName)
let tset_of_list l =
  List.fold_left (fun acc x -> TSet.add x acc) TSet.empty l

(* this implementation is slightly simpler (and more stupid) than
   a linear-time DFS would be *)
let rec type_variable_set = function
  | TyVar (_, tv) -> TSet.singleton tv
  | TyApp (_, _, args) ->
    let f acc t = TSet.union acc (type_variable_set t) in
    List.fold_left f TSet.empty args

let rec type_constructor_set = function
  | TyApp (_, constr, args) ->
    let f acc t = TSet.union acc (type_constructor_set t) in
    List.fold_left f (TSet.singleton constr) args
  | _ -> TSet.empty


module LSet = Set.Make(OrderedLName)
let lset_of_list l =
  List.fold_left (fun acc x -> LSet.add x acc) LSet.empty l

let lset_of_unique_list l =
  (* Local exception in ocaml #IAmAwesome *)
  let module M = struct exception NotUnique end in
  let open M in

  let build s x =
    if LSet.mem x s then raise M.NotUnique
    else LSet.add x s
  in

  try Some (List.fold_left build LSet.empty l) with
  | NotUnique -> None
