open String
open Name
open XAST
open Types
open Positions
open ElaborationErrors
open ElaborationExceptions
open ElaborationEnvironment

(* reduces syntactic noise *)
let nowhere = undefined_position


let string_of_type ty      = ASTio.(XAST.(to_string pprint_ml_type ty))


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
    (** Instance definitions are ignored. Student! This is your job! *)
    ([], env)

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

and env_of_bindings env cdefs = List.(
  (function
    | BindValue (_, vs)
    | BindRecValue (_, vs) ->
      fold_left (fun env (ValueDef (_, ts, _, (x, ty), _)) ->
        bind_scheme x ts [] ty env (* TODO: examine further *)
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


and value_definition env (ValueDef (pos, ts, ps, (x, xty), e)) =
  let env = introduce_type_parameters env ts in
  check_wf_scheme env ts xty;

  if is_value_form e then begin
    let e = eforall pos ts e in
    let e, ty = expression env e in
    let b = (x, ty) in
    check_equal_types pos xty ty;
    (* TODO: do something sensible when ps <> []
       (expression elaboration) *)
    (ValueDef (pos, ts, [], b, EForall (pos, ts, e)),
     bind_scheme x ts ps ty env)
  end else begin
    if ts <> [] then
      raise (ValueRestriction pos)
    else
      let e = eforall pos [] e in
      let e, ty = expression env e in
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


(* Elaborate class *)

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
   - naming! how to avoid name conflicts? do names starting with a
     capital letter work after compilation to OCaml? actually, no...
*)

(* Convention: names of dictionary types, and superclass fields, should start
   with underscores. Main reason: unlike capital letters, _ is a valid leading
   character for a type name or a label name. Also, we hope to avoid name
   clashes as long as the user doesn't define types with strange names...
   Question: should we add leading underscores to dictionary record labels
   for even more name clash avoidance?
*)

and class_to_type_name (TName str) = TName ("_" ^ str)

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
  let members = cdef.class_members in
  let (accessors, env) = Misc.list_foldmap (class_member cname tvar)
                                           env members in
  let dict_record = DRecordType ([tvar], dict_super_fields @ members) in

  (dict_record, accessors, env)

and check_unrelated_superclasses pos env k1 k2 =
  if is_superclass pos k1 k2 env || is_superclass pos k2 k1 env then
    raise (TheseTwoClassesMustNotBeInTheSameContext (pos, k1, k2))

and superclass_dictionary_field tvar cname sc_name =
  let (TName inf_str) = cname and (TName sup_str) = sc_name in
  let field_name = LName ("_" ^ sup_str ^ "_" ^ inf_str) in
  (* for instance, _Eq_Ord *)
  (nowhere, field_name, tyappvar (class_to_type_name sc_name) tvar)

and tyappvar constructor variable =
  TyApp (nowhere, constructor, [TyVar (nowhere, variable)])

and class_member cname tvar env (pos, l, ty) =
  (* TODO: should we reject members with a type where tvar is not free,
     or let that fail during elaboration of expressions? *)
  check_wf_type (bind_type_variable tvar env) KStar ty;

  (* generate code for accessor *)
  let nw = nowhere in
  let dict_type = tyappvar (class_to_type_name cname) tvar in
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
   bind_scheme accessor_name [tvar] [ClassPredicate (cname, tvar)] ty env)
    

