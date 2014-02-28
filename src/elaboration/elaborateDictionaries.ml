open String
open Name
open XAST
open Types
open Positions
open ElaborationErrors
open ElaborationExceptions
open ElaborationEnvironment
open Operators
open AdditionnalSetOperations
open Misc

(* reduces syntactic noise *)
let nowhere = undefined_position

(* Using global mutable state to handle namespace segregation
   between methods and variables *)

let names_hashtbl : (name, bool) Hashtbl.t = Hashtbl.create 277
(* true iff overloaded name *)

let register_as_normal_name name =
  try
    if Hashtbl.find names_hashtbl name
    then raise (OverloadedSymbolCannotBeBound (nowhere, name))
  with
    | Not_found -> Hashtbl.add names_hashtbl name false

let register_as_overloaded_name name =
  try
    if not (Hashtbl.find names_hashtbl name)
    then raise (OverloadedSymbolCannotBeBound (nowhere, name))
  with
    | Not_found -> Hashtbl.add names_hashtbl name true

(* Redefine bind_scheme and bind_simple to register their name.
   This should allow us to handle all bindings (pattern, lambda, let)
   without having to rewrite code.
   Since bind_scheme does not have a position argument,
   our errors will not provide location info (hence the "nowhere" above).
*)

(* TODO: add position argument to bind_scheme and modify all
   occurrences so that error messages are nicer *)

let bind_method_scheme x ts ps ty env = 
  register_as_overloaded_name x;
  bind_scheme x ts ps ty env (* the old bind_scheme *)

let bind_scheme x ts ps ty env = 
  register_as_normal_name x;
  bind_scheme x ts ps ty env

let bind_simple x ty env =
  register_as_normal_name x;
  bind_simple x ty env




(* Type for indicating if a dictionary is needed for an overloaded expression
   or a superclass field *)
(* CHECK: name? *)
type dict_request_source =
| OverloadedExpr of unit (* TODO *)
| SuperclassField of
    (* TODO: the following is the instance "name"; maybe have a dedicated type for it?
       (as it can also be used in instance_definition) *)
    type_class_name * type_constr_name * (type_var_name list)


(* Type that represents *any* class predicate.
   More exactly, it covers the following cases: 
   the type can be:
   - a simple type variable
   - built from a single {0,1}-ary type constructor and (possibly) a type variable) 
*)
type general_class_predicate =  type_class_name * predicate_target
and predicate_target = 
| PredicateTypeVar of type_var_name
| PredicateTypeConstr of type_constr_name * type_var_name option


let make_gen_cl_pred (ClassPredicate (cl, var)) =
  cl, PredicateTypeVar var


type instance_tree = 
| InstLeafFromEnv of instance_definition
| InstLeafFromCtx of class_predicate
| InstBranch of instance_definition * instance_tree list


(* Entry point of the module *)
let rec program p = handle_error List.(fun () ->
  flatten (fst (Misc.list_foldmap block ElaborationEnvironment.initial p))
)



and block env = function
  | BTypeDefinitions ts ->
    let env = type_definitions env ts in
    ([BTypeDefinitions ts], env)

  | BDefinition d ->
    let d, env = value_binding env d in
    ([BDefinition d], env)

  | BClassDefinition c ->
    let (dict_t, accessors, env) = class_definition env c in
    let dict_type_def = TypeDef (nowhere, KStar, 
                                 (class_to_type_name c.class_name),
                                 dict_t) in
    ([BTypeDefinitions (TypeDefs (nowhere, [dict_type_def]));
      BDefinition (BindValue (nowhere, accessors))],
     env)

  | BInstanceDefinitions is ->
    let (dict_defs, env) = instance_definitions env is in
    ([BDefinition (BindRecValue (nowhere, dict_defs))], env)

and type_definitions env (TypeDefs (_, tdefs)) =
  let env = List.fold_left env_of_type_definition env tdefs in
  List.fold_left type_definition env tdefs

and env_of_type_definition env = function
  | (TypeDef (pos, kind, t, _)) as tdef ->
    bind_type t kind tdef env

  | (ExternalType (p, ts, t, os)) as tdef ->
    bind_type t (kind_of_arity (List.length ts)) tdef env

and type_definition env = function
  | TypeDef (pos, _, t, dt) -> datatype_definition t env dt
  | ExternalType (p, ts, t, os) -> env

and datatype_definition t env = function
  | DAlgebraic ds ->
    List.fold_left algebraic_dataconstructor env ds
  | DRecordType (ts, ltys) ->
    List.fold_left (label_type ts t) env ltys

and label_type ts rtcon env (pos, l, ty) =
  let env' = List.fold_left (fun env x -> bind_type_variable x env) env ts in
  check_wf_type env' KStar ty;
  bind_label pos l ts ty rtcon env

and algebraic_dataconstructor env (_, DName k, ts, kty) =
  check_wf_scheme env ts kty;
  bind_scheme (Name k) ts [] kty env

and introduce_type_parameters env ts =
  List.fold_left (fun env t -> bind_type_variable t env) env ts

and check_wf_scheme env ts ty =
  check_wf_type (introduce_type_parameters env ts) KStar ty

and check_wf_type env xkind = function
  | TyVar (pos, t) ->
    let tkind = lookup_type_kind pos t env in
    check_equivalent_kind pos xkind tkind

  | TyApp (pos, t, tys) ->
    let kt = lookup_type_kind pos t env in
    check_type_constructor_application pos env kt tys

and check_type_constructor_application pos env k tys =
  match tys, k with
  | [], KStar -> ()
  | ty :: tys, KArrow (k, ks) ->
    check_wf_type env k ty;
    check_type_constructor_application pos env ks tys
  | _ ->
    raise (IllKindedType pos)

and check_equivalent_kind pos k1 k2 =
  match k1, k2 with
    | KStar, KStar -> ()
    | KArrow (k1, k2), KArrow (k1', k2') ->
      check_equivalent_kind pos k1 k1';
      check_equivalent_kind pos k2 k2'
    | _ ->
      raise (IncompatibleKinds (pos, k1, k2))


(* Is this function useless?
   It doesn't seem to be called from anywhere else... *)
and env_of_bindings env cdefs = List.(
  (function
    | BindValue (_, vs)
    | BindRecValue (_, vs) ->
      fold_left (fun env (ValueDef (_, ts, _, (x, ty), _)) ->
        bind_scheme x ts [] ty env (* low priority TODO: examine further *)
      ) env vs
    | ExternalValue (_, ts, (x, ty), _) ->
      bind_scheme x ts [] ty env (* external value = not overloaded *)
  ) cdefs
)

and check_equal_types pos ty1 ty2 =
  if not (equivalent ty1 ty2) then
    raise (IncompatibleTypes (pos, ty1, ty2))

and type_application pos env x tys =
  (* TODO: handle overloading by turning a type application
     into dictionary passing *)
  List.iter (check_wf_type env KStar) tys;
  let (ts, (_, ty)) = lookup pos x env in
  try
    substitute (List.combine ts tys) ty
  with _ ->
    raise (InvalidTypeApplication pos)

and expression env = function
  | EVar (pos, ((Name s) as x), tys) ->
    (EVar (pos, x, tys), type_application pos env x tys)

  | ELambda (pos, ((x, aty) as b), e') ->
    check_wf_type env KStar aty;
    let env = bind_simple x aty env in
    let (e, ty) = expression env e' in
    (ELambda (pos, b, e), ntyarrow pos [aty] ty)

  | EApp (pos, a, b) ->
    let a, a_ty = expression env a in
    let b, b_ty = expression env b in
    begin match destruct_tyarrow a_ty with
      | None ->
        raise (ApplicationToNonFunctional pos)
      | Some (ity, oty) ->
        check_equal_types pos b_ty ity;
        (EApp (pos, a, b), oty)
    end

  | EBinding (pos, vb, e) ->
    let vb, env = value_binding env vb in
    let e, ty = expression env e in
    (EBinding (pos, vb, e), ty)

  | EForall (pos, tvs, e) ->
    (** Because type abstractions are removed by [value_binding]. *)
    raise (OnlyLetsCanIntroduceTypeAbstraction pos)

  | ETypeConstraint (pos, e, xty) ->
    let e, ty = expression env e in
    check_equal_types pos ty xty;
    (e, ty)

  | EExists (_, _, e) ->
    (** Because we are explicitly typed, flexible type variables
        are useless. *)
    expression env e

  | EDCon (pos, DName x, tys, es) ->
    let ty = type_application pos env (Name x) tys in
    let (itys, oty) = destruct_ntyarrow ty in
    if List.(length itys <> length es) then
      raise (InvalidDataConstructorApplication pos)
    else
      let es =
        List.map2 (fun e xty ->
          let (e, ty) = expression env e in
          check_equal_types pos ty xty;
          e
        ) es itys
      in
      (EDCon (pos, DName x, tys, es), oty)

  | EMatch (pos, s, bs) ->
    let (s, sty) = expression env s in
    let bstys = List.map (branch env sty) bs in
    let bs = fst (List.split bstys) in
    let tys = snd (List.split bstys) in
    let ty = List.hd tys in
    List.iter (check_equal_types pos ty) (List.tl tys);
    (EMatch (pos, s, bs), ty)

  | ERecordAccess (pos, e, l) ->
    let e, ty = expression env e in
    let (ts, lty, rtcon) = lookup_label pos l env in
    let ty =
      match ty with
        | TyApp (_, r, args) ->
          if rtcon <> r then
            raise (LabelDoesNotBelong (pos, l, r, rtcon))
          else
            begin try
              let s = List.combine ts args in
              Types.substitute s lty
            with _ ->
              (** Because we only well-kinded types and only store
                  well-kinded types in the environment. *)
              assert false
            end
        | _ ->
          raise (RecordExpected (pos, ty))
    in
    (ERecordAccess (pos, e, l), ty)

  | ERecordCon (pos, n, i, []) ->
    (** We syntactically forbids empty records. *)
    assert false

  | ERecordCon (pos, n, i, rbs) ->
    let rbstys = List.map (record_binding env) rbs in
    let rec check others rty = function
      | [] ->
        begin match rty with
          | Some (_, TyApp (_, rtcon, _)) ->
            let labels = labels_of rtcon env in
            if (List.length labels <> List.length others) then
              raise (InvalidRecordConstruction pos)
          | _ -> assert false (** Because we forbid empty record. *)
        end;
        List.rev others, rty
      | (RecordBinding (l, e), ty) :: ls ->
        if List.exists (fun (RecordBinding (l', _)) -> l = l') others then
          raise (MultipleLabels (pos, l));

        let (ts, lty, rtcon) = lookup_label pos l env in
        let (s, rty) =
          match rty with
            | None ->
              let rty = TyApp (pos, rtcon, i) in
              let s =
                try
                  List.combine ts i
                with _ -> raise (InvalidRecordInstantiation pos)
              in
              (s, rty)
            | Some (s, rty) ->
              (s, rty)
        in
        check_equal_types pos ty (Types.substitute s lty);
        check (RecordBinding (l, e) :: others) (Some (s, rty)) ls
    in
    let (ls, rty) = check [] None rbstys in
    let rty = match rty with
      | None -> assert false
      | Some (_, rty) -> rty
    in
    (ERecordCon (pos, n, i, ls), rty)

  | ((EPrimitive (pos, p)) as e) ->
    (e, primitive pos p)

and primitive pos = function
  | PIntegerConstant _ ->
    TyApp (pos, TName "int", [])

  | PUnit ->
    TyApp (pos, TName "unit", [])

  | PCharConstant _ ->
    TyApp (pos, TName "char", [])

and branch env sty (Branch (pos, p, e)) =
  let denv = pattern env sty p in
  let env = concat pos env denv in
  let (e, ty) = expression env e in
  (Branch (pos, p, e), ty)

and concat pos env1 env2 =
  List.fold_left
    (fun env (_, (x, ty)) -> bind_simple x ty env)
    env1 (values env2)

and linear_bind pos env (ts, (x, ty)) =
  assert (ts = []); (** Because patterns only bind monomorphic values. *)
  try
    ignore (lookup pos x env);
    raise (NonLinearPattern pos)
  with UnboundIdentifier _ ->
    bind_simple x ty env

and join pos denv1 denv2 =
  List.fold_left (linear_bind pos) denv2 (values denv1)

and check_same_denv pos denv1 denv2 =
  List.iter (fun (ts, (x, ty)) ->
    assert (ts = []); (** Because patterns only bind monomorphic values. *)
    try
      let (_, (_, ty')) = lookup pos x denv2 in
      check_equal_types pos ty ty'
    with _ ->
      raise (PatternsMustBindSameVariables pos)
  ) (values denv1)

and pattern env xty = function
  | PVar (_, name) ->
    bind_simple name xty ElaborationEnvironment.empty

  | PWildcard _ ->
    ElaborationEnvironment.empty

  | PAlias (pos, name, p) ->
    linear_bind pos (pattern env xty p) ([], (name, xty))

  | PTypeConstraint (pos, p, pty) ->
    check_equal_types pos pty xty;
    pattern env xty p

  | PPrimitive (pos, p) ->
    check_equal_types pos (primitive pos p) xty;
    ElaborationEnvironment.empty

  | PData (pos, (DName x), tys, ps) ->
    let kty = type_application pos env (Name x) tys in
    let itys, oty = destruct_ntyarrow kty in
    if List.(length itys <> length ps) then
      raise (InvalidDataConstructorApplication pos)
    else
      let denvs = List.map2 (pattern env) itys ps in (
        check_equal_types pos oty xty;
        List.fold_left (join pos) ElaborationEnvironment.empty denvs
      )

  | PAnd (pos, ps) ->
    List.fold_left
      (join pos)
      ElaborationEnvironment.empty
      (List.map (pattern env xty) ps)

  | POr (pos, ps) ->
    let denvs = List.map (pattern env xty) ps in
    let denv = List.hd denvs in
    List.(iter (check_same_denv pos denv) (tl denvs));
    denv

and record_binding env (RecordBinding (l, e)) =
  let e, ty = expression env e in
  (RecordBinding (l, e), ty)

and value_binding env = function
  | BindValue (pos, vs) ->
    let (vs, env) = Misc.list_foldmap value_definition env vs in
    (BindValue (pos, vs), env)

  | BindRecValue (pos, vs) ->
    let env = List.fold_left value_declaration env vs in
    let (vs, _) = Misc.list_foldmap value_definition env vs in
    (BindRecValue (pos, vs), env)

  | ExternalValue (pos, ts, ((x, ty) as b), os) ->
    let env = bind_scheme x ts [] ty env in
    (ExternalValue (pos, ts, b, os), env)

and eforall pos ts e =
  match ts, e with
    | ts, EForall (pos, [], ((EForall _) as e)) ->
      eforall pos ts e
    | [], EForall (pos, [], e) ->
      e
    | [], EForall (pos, _, _) ->
      raise (InvalidNumberOfTypeAbstraction pos)
    | [], e ->
      e
    | x :: xs, EForall (pos, t :: ts, e) ->
      if x <> t then
        raise (SameNameInTypeAbstractionAndScheme pos);
      eforall pos xs (EForall (pos, ts, e))
    | _, _ ->
      raise (InvalidNumberOfTypeAbstraction pos)


(***** Modifying this for the project *****)

and value_definition env (ValueDef (pos, ts, ps, (x, xty), e)) =
  let env' = introduce_type_parameters env ts in
  check_wf_scheme env ts xty;

  if is_value_form e then begin
    let e = eforall pos ts e in
    (* TODO: add class constraints to local typing environment *)
    let e, ty = expression env' e in
    let b = (x, ty) in
    check_equal_types pos xty ty;

    (* Checks wrt typeclasses *)

    let ty_vars = type_variable_set ty in
    List.iter begin fun (ClassPredicate (_, a)) ->
      if not (TSet.mem a ty_vars)
      (* unreachable constraint!
         TODO: think about adding a specific exception for that *)
      then raise (InvalidOverloading pos)
    end ps;
    
    check_correct_context pos env (tset_of_list ts) ps;

    
    (* is this correct? *)

    (* TODO: do something sensible when ps <> []
       (expression elaboration) *)
    (ValueDef (pos, ts, [], b, EForall (pos, ts, e)),
     bind_scheme x ts ps ty env)
  end else begin
    if ts <> [] then
      raise (ValueRestriction pos)
    else
      let e = eforall pos [] e in
      let e, ty = expression env' e in
      let b = (x, ty) in
      check_equal_types pos xty ty;
      (ValueDef (pos, [], [], b, e), bind_simple x ty env)
  end

and value_declaration env (ValueDef (pos, ts, ps, (x, ty), e)) =
  bind_scheme x ts ps ty env


and is_value_form = function
  | EVar _
  | ELambda _
  | EPrimitive _ ->
    true
  | EDCon (_, _, _, es) ->
    List.for_all is_value_form es
  | ERecordCon (_, _, _, rbs) ->
    List.for_all (fun (RecordBinding (_, e)) -> is_value_form e) rbs
  | EExists (_, _, t)
  | ETypeConstraint (_, t, _)
  | EForall (_, _, t) ->
    is_value_form t
  | _ ->
    false

(* End of initial code *)

(************************************************************)


(***** Elaborate classes *****)

(* note: type name of new record = type name of class *)

(* strategy for now: do *not* add the elaborated record type
   to the typing environment, only generate the code for
   the declaration and add the original *class* decl to the env *)

(* Issues to think about:
   - do we generate accessors for superclass fields in the dictionary,
     even though the user won't ever call them?
     this is what is prescribed in the course notes
     alternatively, we could just use dict.field_name...
     current choice: only generate accessors for class members
   => No.
*)

(* 
   TODO: put the naming convention here

   => Cf discussion in todo

*)


(***** Naming functions / conventions *****)
(* TODO: evil clash avoidance *)
(* TODO: unify their way of operation (parameter form, etc) *)
(* TODO: find better names (bitch!) *)
and sconcat = String.concat ""
and uconcat = String.concat "_"
and superclass_accessor_type_name (TName supcl) (TName cl) =
  LName (uconcat ["superclass_field"; supcl; cl])
and class_to_type_name (TName cl_name) = 
  TName (uconcat ["class_type"; cl_name]) 
(* k = class, g = instance *)
and instance_to_dict_name (TName cl_name) (TName inst_name) =
  Name (uconcat ["inst_dict"; cl_name; inst_name])


(* TODO: find better names for these functions *)
and class_to_dict_type k a = tyappvar (class_to_type_name k) a
and class_predicate_to_type (ClassPredicate (k, a)) = class_to_dict_type k a
(* and class_to_dict_var_name (TName str) = Name ("_" ^ str) *)


and tyappvar constructor variable =
  TyApp (nowhere, constructor, [TyVar (nowhere, variable)])


and class_definition env cdef = 
  let tvar = cdef.class_parameter
  and cname = cdef.class_name
  and pos = cdef.class_position in

  let env = bind_class cname cdef env in

  (* Handle superclasses *)
  let super = cdef.superclasses in
  Misc.iter_unordered_pairs (check_unrelated_superclasses pos env) super;
  let dict_super_fields = List.map (superclass_dictionary_field tvar cname)
                                   super in

  (* Handle class members *)
  (* TODO: prevent 2 members from having the same name
     also, should we allow shadowing of an overloaded name
     by another one?
  *)
  let members = cdef.class_members in
  let (accessors, env) = Misc.list_foldmap (class_member cname tvar)
                                           env members in
  let dict_record = DRecordType ([tvar], dict_super_fields @ members) in

  (dict_record, accessors, env)

and check_unrelated_superclasses pos env k1 k2 =
  if is_superclass pos k1 k2 env || is_superclass pos k2 k1 env then
    raise (TheseTwoClassesMustNotBeInTheSameContext (pos, k1, k2))

and superclass_dictionary_field tvar cname sc_name =
  let field_name = superclass_accessor_type_name cname sc_name in
  (* for instance, _Eq_Ord *)
  (nowhere, field_name, class_to_dict_type sc_name tvar)

and class_member cname tvar env (pos, l, ty) =
  check_wf_type (bind_type_variable tvar env) KStar ty;
  begin
    if not (TSet.mem tvar (type_variable_set ty))
      (* unreachable constraint / ambiguous type variable *)
    then raise (InvalidOverloading pos)
  end;

  (* generate code for accessor *)
  let nw = nowhere in
  let dict_type = class_to_dict_type cname tvar in
  let accessor_elaborated_type = ntyarrow nw [dict_type] ty
  and accessor_expr = ELambda (nw, (Name "z", dict_type),
                               ERecordAccess (nw, EVar (nw, Name "z", []), l))
  and accessor_name = let (LName str) = l in Name str in
  (* Note: in the elaborated code, => was converted into ->, 
     but the binding added to the environment has the type scheme
     with => *)
  (ValueDef (nowhere, [tvar], [(* no class predicate *)],
             (accessor_name, accessor_elaborated_type),
             accessor_expr),
   bind_method_scheme accessor_name [tvar] [ClassPredicate (cname, tvar)] ty env)
    

(***** Elaborate instances *****)

and instance_definitions env deflist =
  let big_env = List.fold_left env_of_instance_definition env deflist in
  Misc.list_foldmap (instance_definition big_env) env deflist

and env_of_instance_definition env inst_def =
  bind_instance inst_def env

(* big_env: environment where all instances are visible
   (since consecutive instances are recursively defined)
   small_env: environment where only previous instances are visible
   (to avoid ill-founded recursion)
   see the project spec for more details
*)
and instance_definition big_env small_env inst_def =
  let index = inst_def.instance_index
  and tvars = inst_def.instance_parameters
  and cname = inst_def.instance_class_name
  and pos = inst_def.instance_position
  and members = inst_def.instance_members
  in

  (* contains parameters as free variables *)
  let instance_type =
    TyApp (nowhere, index,
           List.map (fun tv -> TyVar (nowhere, tv)) tvars) in


  let class_def = lookup_class pos cname small_env
  in

  (* TODO: maybe check that the same type variable does not occur twice??
     Is this enforced in the rest of the code? *)
  let tvar_set = tset_of_list tvars in

  let ctx = inst_def.instance_typing_context in
  check_correct_context pos small_env tvar_set ctx;
  let constructor_argument_types = List.map class_predicate_to_type ctx in 

  (* new_small_env = h' + h (in subject) *)
  let new_small_env = bind_instance inst_def small_env in
  let dict_constructor_type =
    let dict_type =
      TyApp (nowhere, class_to_type_name cname, [instance_type]) in
    ntyarrow nowhere constructor_argument_types dict_type
  in
  (* TODO: actually create dictionary
     also, what the hell is the name field in ERecordCon supposed to be???
     => The name field is filled with a non significant name by the parser...
  *)
  (* FIXME: we should add the superinstances to the envs *)
  (* TODO: description *)
  let env_with_free_tvars t =
    TSet.fold bind_type_variable tvar_set 
      $
      match destruct_ntyarrow t with
      | ([], _) ->  small_env
      | _ -> big_env
  in


  (* Record bindings set type *)
  (* Note: the following code would be far less heavy if we had a real "set" type
     in ocaml (Ord a => 'a set, and not some functors...)
     (here, we have to implement with functors what a typeclass would automagically
     do for us)
  *)
  (* CHECK: Is it useful to define it somewhere else, in order to reuse it? *)
  let module OrderedMember =
        struct
          type t = record_binding
          (* CHECK: this function only checks names; is it ok? *)
          let compare (RecordBinding (n1, _)) (RecordBinding (n2, _)) =
            OrderedLName.compare n1 n2
        end
  in
  let module OrderedPair =
      struct
          type t = record_binding * mltype
          let compare (m1, t1) (m2, t2) = OrderedMember.compare m1 m2
      end
  in


  let module MSet = Set.Make (OrderedMember) in
  let module PSet = Set.Make(OrderedPair) in

  let module MP = BiSetOp(MSet)(PSet) in
  let module PM = BiSetOp(PSet)(MSet) in


  (* Build the members set and check uniqueness *)
  let members_set =
    List.fold_left
      (
        fun s x ->
          if MSet.mem x s then failwith "TODO: multiple definition of instance member"
            (* TODO: real error handling *)
          else MSet.add x s
      )
      MSet.empty members
  in


  (* Members set augmented with the type of the corresponding class member
     Therefore, it is a set of (record_binding, mltype) pairs *)
  (* TODO: what if an instance member matches no class member? *)
  let augmented_members_set =
    (* Find corresponding class member and add its type,
       instanciated at the instance type, to form the pair (member, type) *)
    let rec subst = function
      | TyVar (pos, x) when x = class_def.class_parameter ->
        instance_type
      (* TODO: change this once we support classes with
         polymorphic methods, e.g. fmap *)
      | TyVar _ -> failwith "unbound type var"
      | TyApp (pos, constr, args) -> TyApp (pos, constr, List.map subst args)
    in
    let f (RecordBinding (name, _)) =
      (* TODO: if we got no matching class member, this List.find will raise Not_found *)
      subst =< proj3_3 =< List.find (((=) name) =< proj2_3)
      $ class_def.class_members
    in
    let id x = x in (* why isn't this part of the standard library? *)
    MP.map (id &&& f)  members_set
  in

  (* TODO: check that every class member is defined
     (uniqueness is already enforced) *)

  (* CHECK: can we provide more position information below? (lots of nowhere, dummy_pos, etc) *)
  let dict_record = ERecordCon
    (
      nowhere,
      Name "WTFITS??",
      (* Instantiation of the record type *)
      instantiation Lexing.dummy_pos (* FIXME: wtf?!  *)
        $ TypeApplication (List.map (fun x -> TyVar (nowhere, x)) tvars)
        (* CHECK: If G is nullary, then this is the empty list,
           so morally it should work (bitch) *)
        ,

      List.append
        (* Members defined by the current class *)
        (
          MSet.elements $
            PM.map
              (fun (rec_binding, cl_member_type) ->
                let compiled_rb_code, computed_type = (* CHECK: names? *)
                  record_binding (env_with_free_tvars cl_member_type) rec_binding
                in
                (* Check instance member type against corresponding class
                   member type, correctly instantiated *)
                check_equal_types pos computed_type cl_member_type;
                compiled_rb_code
              )
              augmented_members_set
        )

       (* Superclass accessors *)
        (List.map 
           (fun spcl ->
             RecordBinding
               (superclass_accessor_type_name spcl cname, 
                (* TODO: remove this (debug) and return a proper expression *)
                begin
                  (fun (TName n) ->  print_string ("Computed derivation for superclass " ^ n ^ ":\n")) spcl;
                  let rec p = function
                    | InstLeafFromEnv instdef ->
                      "From env"
                    | InstLeafFromCtx _ ->
                      "From ctx"
                    | InstBranch (inst, dep) ->
                      "Inst" ^ (* TODO *) "[" ^ (String.concat "; "(List.map p dep)) ^ "]"
                  in
                  let deriv =
                    find_parent_dict_proof ctx small_env 
                      (spcl, PredicateTypeConstr (index, 
                                             match tvars with
                                             | [] -> None
                                             | [v] -> Some v
                                             | _ -> assert false)
                      )
                  in
                  match deriv with
                  | Some deriv ->
                    p deriv;
                    elaborate_parent_proof_into_expr ctx small_env deriv
                  | None -> assert false
                end
               )
           )
           class_def.superclasses
        )

    )
  in


  (* TODO: dict_constructor should be lambda super1 ... supern . dict_record *)
  (* => ELambda + add a naming convention for dictionary variables *)
  (* Superinstances arguments *)
  let dict_constructor =
    List.fold_right
      (fun (ClassPredicate (name1, name2)) next ->
        ELambda
          ( (* TODO *)
            nowhere,
            assert false (* (name * t) *),
            next
          ))
      ctx
      dict_record
  in
  let dict_def = ValueDef
    (nowhere, tvars, [(* no class predicate *)],
     (instance_to_dict_name cname index,
      dict_constructor_type),
     dict_constructor)
  in
  (dict_def, new_small_env)

and check_correct_context pos env tvar_set ctx =
  (* check that the classes are defined and the type variables
     are quantified over *)
  List.iter
    (
      fun (ClassPredicate (k, a)) ->
        if not (TSet.mem a tvar_set)
        then raise (UnboundTypeVariable (pos, a))
        else ignore (lookup_class pos k env)
    )
    ctx;
  (* canonicity *)
  Misc.iter_unordered_pairs
    (
      fun cp1 cp2 ->
        let (ClassPredicate (k1, a1)) = cp1
        and (ClassPredicate (k2, a2)) = cp2 in
        if a1 = a2 then check_unrelated_superclasses pos env k1 k2
    )
    ctx


(** Finding parent dictionaries **)
(* TODO: following commentaries should go to the .mli *)
(* Following function finds a proof derivation for the target class in the given context *)
(* The following functions have simple forms thanks to the simplification of the class system *)
(* CHECK: is it better to merge ctx in env for the search? *)
(* TODO: try to add more position information (remove as much nowheres as possible) *)
and find_parent_dict_proof ctx env target =
  let unwrap = Misc.unwrap_res_or_die
  and unwrap_list = Misc.unwrap_res_or_die_list
  in

  let rec loop (cl, target) =
    match target with
    | PredicateTypeVar v ->
      begin
        match List.filter ((=) (ClassPredicate (cl, v))) ctx with
        | [] ->
          None
        | [inst] ->
          Some (InstLeafFromCtx inst)
        | _ ->
          assert false (* Duplicate entry in context *)
      end

    | PredicateTypeConstr (constr, opt_var) ->
      let arity = arity_of_kind (lookup_type_kind nowhere constr env)
      in

      begin
        match arity with
        (* Nullary constructor target (i.e., base type) *)
        | 0 ->
          (* Simple check to ensure coherence *)
          (match opt_var with 
          | Some _ -> raise (InvalidTypeApplication nowhere)
          | None -> ());
          (* CHECK: should it be IllKindedType? *)
          

          unwrap (fun x -> Some (InstLeafFromEnv x)) (lookup_instance (cl, constr) env)
            
        (* Unary constructor target *)
        | 1 ->
          (* Simple check to ensure coherence *)
          if opt_var = None then
            raise (IllKindedType nowhere);
          
          (* TODO : this match is an unwrap *)
          (
            match lookup_instance (cl, constr) env with
            | None -> None
            | Some inst ->
              let dependencies = inst.instance_typing_context 
              in

              let new_targets = List.map (fun x -> Some (make_gen_cl_pred x)) dependencies
              in
                
              unwrap (fun e -> Some (InstBranch (inst, e))) 
                $ unwrap_list loop new_targets
          )

        | n -> assert false (* TODO *)

      end

  in

  loop target
	  

(* This function uses a proof derivation found by <find_parent_dict_proof> to elaborate
   an expression to access target dictionary *)
and elaborate_parent_proof_into_expr ctx env target =
  (* TODO *)
  EPrimitive (nowhere, PUnit)
