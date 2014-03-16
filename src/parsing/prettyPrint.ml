(** Pretty-printing of AST for ML variants. *)

module Make (GAST : AST.GenericS) = struct

  open Positions
  open Name
  open GAST
  open PPrint
  open TypeAlgebra
  open Types

  let parens_if b d =
    if b then parens d else d

  let rec ml_kind = function
    | KStar ->
      !^ "*"

    | KArrow (KStar, k) ->
      group (!^ "* =>" ^^ ml_kind k)

    | KArrow (k1, k2) ->
      group (parens (ml_kind k1) ^/^ !^ "=>" ^/^ ml_kind k2)

  let separate_map2 first sep f = function
    | [] -> empty
    | [x] -> group (first ^^ f x)
    | x :: xs -> group (first ^^ f x) ^^ sep ^^ separate_map sep f xs

  let bad_assoc s side =
    match associativity s, side with
      | AssocRight, `R
      | AssocLeft, `L -> false
      | _, _ -> true

  let tname (TName s) = !^ s

  let rname (TName s) =
    !^ (if s.[0] = '\'' then String.sub s 1 (String.length s - 1) else s)

  let rec ml_type ?generics = function
    | TyVar (_, s) ->
      begin match generics with
        | None ->
          tname s
        | Some generics ->
          if List.mem s generics then rname s
          else !^ "_" (* Work around bugs PR6150/PR6264 of ocaml <= 4.01. *)
      end
    | TyApp (_, t, ts) ->
      type_application ?generics t ts

  and ml_type' ?generics side parent = function
    | TyApp (_, n, _) as ty ->
      parens_if (begin match as_symbol n with
        | Some s ->
          (priority s < priority parent || (parent = s && bad_assoc s side))
        | None ->
          false
      end) (ml_type ?generics ty)
    | ty -> ml_type ?generics ty

  and ml_type'' ?generics ty =
    parens_if (match ty with
      | TyVar _ | TyApp (_, _, []) -> false
      | _ -> true
    ) (ml_type ?generics ty)

  and type_application ?generics ((TName sn) as n) ts =
    group (
      match as_symbol n with
      | None ->
        begin match ts with
          | [] ->
            !^ sn
          | [x] ->
            ml_type'' ?generics x ^/^ !^ sn
          | xs ->
            group (parens (separate_map comma (ml_type ?generics) xs)) ^/^ !^ sn
        end
      | Some s ->
        if infix s then
          match ts with
            | [ a; b ] ->
              group (ml_type' ?generics `L s a)
              ^//^ !^ sn ^/^ group (ml_type' ?generics `R s b)
            | _ -> assert false (* We only handle infix binary operators. *)
        else
          match ts with
            | [] -> !^ sn
            | [x] -> begin match associativity s with
                | EnclosedBy (l, r) ->
                  group (!^ l ^//^ ml_type ?generics x ^//^ !^ r)
                | _ ->
                  group (parens (ml_type ?generics x) ^/^ !^ sn)
            end
            | xs ->
              group (parens (separate_map comma (ml_type ?generics) xs))
              ^/^ !^ sn
    )

  let printer produce_ocaml =

  let annotate e a =
    if produce_ocaml then
      e
    else group (parens (group e ^/^ group (!^ ":" ^/^ a)))

  in
  let rec expression = function
    | EVar (_, Name x, i) ->
      group (!^ x ^^ instantiation i)

    | ELambda (_, b, e) ->
      group (!^ "fun" ^/^ binding b ^/^ !^ "->") ^//^ group (expression e)

    | EApp (_, a, b) ->
      group (expression' `LApp a) ^//^ group (expression' `RApp b)

    | EPrimitive (_, p) ->
      primitive p

    | EDCon (_, DName k, i, []) ->
      group (!^ k ^^ instantiation i)

    | EDCon (_, DName k, i, ts) ->
      group (!^ k
             ^^ instantiation i
             ^//^ parens (separate_map (comma ^^ break 1) expression ts))

    | ETypeConstraint (_, e, ty) ->
      annotate (expression e) (ml_type ty)

    | EBinding (_, bvs, e) ->
      group (bind_values false bvs ^/^ !^ "in")
      ^//^ group (expression e)

    | ERecordAccess (_, e, LName l) ->
      parens_if (match e with
        | EVar _ -> false
        | _ -> true
      ) (expression e) ^/^ !^ ("." ^ l)

    | ERecordCon (_, _, i, rbs) ->
      if produce_ocaml && rbs = [] then
        !^ "()"
      else
        braces (record_bindings rbs) ^^ instantiation i

    | EMatch (_, s, bs) ->
      group (group (!^ "match" ^/^ expression s ^/^ !^ "with") ^/^ branches bs)

    | EForall (_, ts, e) ->
      group (type_parameters ts ^^ expression e)

    | EExists (_, ts, e) ->
      let ts =
        match ts with
          | [] -> empty
          | ts ->
            if produce_ocaml then
              empty
            else
              group (braces (separate_map (break 1) tname ts)) ^^ break 1
      in
      group (ts ^^ expression e)

    | EModuleAccess (mod_expr, name) ->
      group (module_access mod_expr name)

  and instantiation i =
    does_not_exist_in_ocaml (fun () ->
      match destruct_instantiation_as_type_applications i with
        | None ->
          empty
        | Some tys ->
          group (brackets (
            separate_map (comma ^^ break 1) ml_type tys
          ))
    )

  and primitive = function
    | PIntegerConstant x ->
      !^ (string_of_int x)

    | PCharConstant '\'' ->
      !^ "'\''"

    | PCharConstant c ->
      !^ (Printf.sprintf "'%c'" c)

    | PUnit ->
      !^ "()"

  and record_bindings rbs =
    separate_map (!^ ";" ^^ break 1) record_binding rbs

  and record_binding (RecordBinding (LName l, e)) =
    group (!^ l ^/^ !^ "="  ^/^ expression' `RecordField e)

  and branches bs =
    group (separate_map (break 1 ^^ group (!^ "|" ^^ break 1)) branch bs)

  and branch (Branch (_, p, e)) =
    group (group (pattern p)
           ^/^ !^ "->" ^/^ group (expression' `InBranchBody e)
    )

  and pattern = function
    | PVar (_, Name x) ->
      !^ x

    | PWildcard _ ->
      !^ "_"

    | PAlias (_, Name x, p) ->
      pattern p ^/^ !^ "as" ^/^ !^ x

    | PTypeConstraint (_, p, ty) ->
      annotate (pattern p) (ml_type ty)

    | PPrimitive (_, p) ->
      primitive p

    | PData (_, DName k, i, ps) ->
      !^ k ^^ instantiation i ^^ (match ps with
        | [] -> empty
        | xs -> parens (separate_map (comma ^^ break 1) pattern ps)
      )

    | POr (_, ps) ->
      parens (separate_map (!^ "|" ^^ break 1) pattern ps)

    | PAnd _ ->
      (* The source language does not handle this form of pattern matching
         to match O'Caml's pattern language. *)
      assert false

  and all_local_modules = List.for_all
    (function VLocalModule _ -> true | _ -> false)

  and bind_values is_toplevel = function
    | BindValue (_, []) | BindRecValue (_, []) ->
      empty

    (* Handle special cases of interest for modules *)
    | BindValue (_, [VLocalModule mod_def]) ->
      (if is_toplevel then empty else !^ "let")
      ^/^ module_struct mod_def

    | BindValue (_, vs) when all_local_modules vs ->
      assert is_toplevel;
      failwith "TODO"

    | BindRecValue (_, vs) when all_local_modules vs ->
      assert is_toplevel;
      failwith "TODO"

    (* End of modules *)

    | BindValue (_, vs) ->
      group (
        !^ "let"
        ^/^ separate_map (break 1 ^^ !^ "and" ^^ break 1) value_definition vs
      )

    | BindRecValue (_, vs) ->
      group (
        !^ "let rec"
        ^/^ separate_map (break 1 ^^ !^ "and" ^^ break 1) value_definition vs
      )

    | ExternalValue (_, ts, b, s) ->
      group (
        !^ (if produce_ocaml then "let" else "let external")
        ^/^ type_parameters ts ^^ binding b
        ^/^ !^ "="
        ^/^ (if produce_ocaml then !^ s else string_literal s)
      )

  and string_literal s =
    dquotes (!^ s)

  and value_definition (ValueDef (_, ts, c, b, e)) =
    if produce_ocaml then
      let b =
        match destruct_binding b with
          | (Name x, None) -> !^ x
          | (Name x, Some ty) -> !^ x ^/^ !^ ":" ^/^ ocaml_type_scheme ts ty
      in
      group (b ^/^ !^ "=") ^//^ group (expression e)
    else
      group (type_parameters ts
             ^^ class_predicates c ^^ binding b ^/^ !^ "=")
      ^//^ group (expression e)

  and ocaml_type_scheme ts ty =
    let dty = ml_type ~generics:ts ty in
    match ts with
      | [] -> dty
      | ts -> !^ "type" ^/^ separate_map (break 1) rname ts ^^ !^ "." ^/^ dty

  and class_predicates = function
    | [] -> empty
    | cs ->
      does_not_exist_in_ocaml (fun () ->
        brackets (
          break 1
          ^^ separate_map (comma ^^ break 1) class_predicate cs
          ^^ break 1
        ) ^^ break 1
      )

  and class_predicate (ClassPredicate (k, t)) =
    group (tname k ^/^ tname t)

  and type_parameters = function
    | [] -> empty
    | ts ->
      does_not_exist_in_ocaml (fun () ->
        brackets (separate_map (break 1) tname ts) ^^ break 1
      )

  and expression' ctx e =
    parens_if (match ctx, e with
      | `RecordField, (ELambda _ | EBinding _ | EMatch _)
      | `LApp, (ELambda _ | EDCon _ | EMatch _)
      | `RApp, (ELambda _ | EApp _ | EDCon _ | EMatch _) -> true
      | `InBranchBody, EMatch _ -> true
      | _, _ -> false
    ) (expression e)

  and binding b =
    match destruct_binding b with
      | (Name x, None) ->
        !^ x
      | (Name x, Some ty) ->
        annotate (!^ x) (ml_type ty)

  and type_mutual_definitions (TypeDefs (_, tdefs)) =
    group (
      separate_map2
        (!^ "type" ^^ space)
        (break 1 ^^ !^ "and" ^^ break 1)
        type_definition
        tdefs
    )

  and type_definition = function
    | TypeDef (_, _, TName t, dt) ->
      let ts = extract_type_parameters dt in
      group (group (adt_type_parameters ts ^^ !^ t ^/^ !^ "=")
             ^/^ datatype_definition dt)

    | ExternalType (_, ts, TName t, s) ->
      if produce_ocaml then
        group (adt_type_parameters ts ^^ !^ t ^/^ !^ "="
               ^/^ !^ s)
      else
        group (!^ "external"
               ^/^ group (adt_type_parameters ts ^^ !^ t ^/^ !^ "=")
               ^/^ string_literal s)

  and datatype_definition = function
    | DAlgebraic datacons ->
      group (separate_map2 empty (break 1 ^^ !^ "| ") datacon datacons)

    | DRecordType (_, []) when produce_ocaml ->
      !^ "unit"

    | DRecordType (_, r) ->
      braces (row r)

  and row r =
    separate_map (!^ ";" ^^ break 1) label_type r

  and label_type (_, LName l, ty) =
    !^ l ^/^ !^ ":" ^/^ ml_type ty

  and adt_type_parameters = function
    | [] -> empty
    | [t] -> tname t ^^ break 1
    | ts -> parens (separate_map (comma ^^ break 1) tname ts) ^^ break 1

  and extract_type_parameters = function
      | DAlgebraic ds ->
        let (_, _, ts, _) = List.hd ds in
        ts
      | DRecordType (ts, _) ->
        ts

  and datacon (_, DName k, _, ty) =
    let of_ d = space ^^ !^ "of" ^/^ d in
    group (!^ k ^^ match fst (destruct_ntyarrow ty) with
      | [] ->
        empty
      | [x] ->
        of_ (ml_type x)
      | xs ->
        of_ (separate_map (break 1 ^^ star ^^ break 1) ml_type xs)
    )

  and does_not_exist_in_ocaml f =
    if produce_ocaml then empty else f ()

  and class_definition cd =
    does_not_exist_in_ocaml (fun () ->
      let cs =
        List.map
          (fun k -> ClassPredicate (k, cd.class_parameter))
          cd.superclasses
      in
      group (
        group (
          !^ "class"
          ^/^ superclasses cs
          ^^ class_predicate (ClassPredicate (cd.class_name,cd.class_parameter))
        ) ^/^ nest 2 (!^ "{"
                      ^/^ group (row cd.class_members)) ^/^ !^ "}"
      )
    )

  and superclasses = function
    | [] ->
      empty
    | xs ->
      separate_map (comma ^^ break 1) class_predicate xs
      ^/^ !^ "=>" ^^ break 1

  and instance_definition id =
    does_not_exist_in_ocaml (fun () ->
      let instance_index =
        TyApp (undefined_position, id.instance_index,
               List.map
                 (fun t -> TyApp (undefined_position, t, []))
                 id.instance_parameters
        )
      in
      group (
        group (
          group (
            !^ "instance"
            ^/^ type_parameters id.instance_parameters
            ^^ superclasses id.instance_typing_context
            ^^ group (tname id.instance_class_name ^/^ ml_type instance_index)
          ) ^/^ nest 2 (
            !^ "{"
            ^/^ group (record_bindings id.instance_members)
          ) ^/^ !^ "}"
        )
      )
    )

  and module_sig name (TName tycon) members =
    if not produce_ocaml then empty else begin
      group (
        group (
          !^ "module type"
          ^/^ !^ name
          ^/^ !^ "="
        ) ^/^ nest 2 (!^ "sig"
                      ^/^ group (!^ "type 'a" ^/^ !^ tycon)
                      ^/^ group (separate_map (break 1) sig_val members)) ^/^ !^ "end"
      )

    end

  and sig_val (Name l, ty) =
    !^ "val" ^/^ !^ l ^/^ !^ ":" ^/^ ml_type ty

  and module_struct ?(recursive = false) mod_def =
    if not produce_ocaml then empty else begin
      group (
        group (
          !^ (if recursive then "module rec" else "module")
          ^/^ !^ (mod_def.module_name)
          ^/^ module_type_annot mod_def.module_signature
          ^/^ !^ "="
        )
        ^/^ functor_args mod_def.module_functor_args
        ^/^ !^ "struct"
        ^/^ nest 2 (program mod_def.module_members)
        ^/^ !^ "end"
      )
    end
      
  and module_type_annot = function
    | None -> empty
    | Some (name, with_type) -> !^ ":" ^/^ !^ name ^/^ begin match with_type with
        | None -> empty
        | Some (TName t, str) ->
          !^ "with type" ^/^ !^ "'a" ^/^ !^ t ^/^ !^ "=" ^/^ !^ "'a" ^/^ !^ str
    end

  and functor_args args = separate_map (break 1) functor_arg args
  and functor_arg (name, ty) = group (
    !^ "functor"
    ^/^ parens (!^ name ^/^ module_type_annot (Some ty))
    ^/^ !^ "->"
  )

  and module_access mod_expr (Name name) =
    parens (
      !^ "let module X = "
      ^/^ group (module_expr mod_expr)
      ^/^ !^ "in"
      ^/^ !^ ("X." ^ name)
    )

  and module_expr = function
    | FunctorApp (fctr, args) ->
      !^ fctr ^^ separate_map empty (fun x -> parens (module_expr x)) args
    | ModulePath mod_names ->
      separate_map (!^ ".") (!^) mod_names
    | InlineStruct mltype ->
      !^ "struct" ^/^ !^ "type 'a t =" ^/^ !^ "'a" ^/^ ml_type mltype ^/^ !^ "end"
      
  and block = function
    | BClassDefinition c ->
      [class_definition c]
    | BInstanceDefinitions ts ->
      List.map instance_definition ts
    | BTypeDefinitions ts ->
      [type_mutual_definitions ts]
    | BDefinition d ->
      [bind_values true d]
    | BModuleSig (name, tycon, members) ->
      [module_sig name tycon members]
    | BModule mod_def ->
      [module_struct mod_def]

  and program p =
    separate (break 1) (List.(flatten (map block p)))

  in
  program, expression

  let program b = fst (printer b)
 
  let expression = snd (printer false)

  let ml_type b = ml_type b

end
