open Name

module E = InferenceExceptions

let handle_error print_variable p =
  let fatal pos msg =
    let pos = Positions.([start_of_position pos; end_of_position pos]) in
    Errors.fatal pos msg
  and s = Printf.sprintf in
  try
    p ()
  with
    | AlphaRename.UnboundVariable (pos, Name x) ->
      fatal pos (s "  Identifier `%s' is unbound." x)

    | AlphaRename.OverloadedSymbolCannotBeBound (pos, Name x) ->
      fatal pos (s "  Identifier `%s' cannot be both overloaded and let-bound."
                   x)

    | E.UnboundTypeVariable (pos, TName x) ->
      fatal pos (s "  Type variable `%s' is unbound." x)

    | E.UnboundTypeConstructor (pos, TName x) ->
      fatal pos (s "  Type constructor `%s' is unbound." x)

    | E.UnboundTypeIdentifier (pos, TName x) ->
      fatal pos (s "  Type identifier `%s' is unbound." x)

    | E.UnboundLabel (pos, LName x) ->
      fatal pos (s "  Label `%s' is unbound." x)

    | E.InvalidTypeVariableIdentifier (pos, TName x) ->
      fatal pos (s "  `%s' is already a type constructor." x)

    | E.UnboundDataConstructor (pos, DName x) ->
      fatal pos (s "  Data constructor `%s' is unbound." x)

    | E.InvalidDataConstructorDefinition (pos, DName x) ->
      fatal pos (s "  Invalid definition of data constructor `%s'." x)

    | E.MultipleLabels (pos, LName x) ->
      fatal pos (s "  Label `%s' appears multiple times in a record." x)

    | E.NonLinearPattern (pos, Name x) ->
      fatal pos (s "  Identifier `%s' appears multiple times in a pattern."
                   x)

    | E.InvalidDisjunctionPattern pos ->
      fatal pos "  Disjunctive patterns must bind the same variables."

    | E.NotEnoughPatternArgts pos ->
      fatal pos "  Invalid application of a data constructor in a pattern."

    | E.CannotGeneralize (pos, v) ->
      fatal pos
        (s "  Cannot generalize the type of this term as requested.
         \n   Inferred type: %s"
           (print_variable pos v))

    | E.NonDistinctVariables (pos, vl) ->
      fatal pos
        (s "  The following type variables are inferred to be equal: `%s'."
           (String.concat " " (List.map (print_variable pos) vl)))

    | E.KindError pos ->
      fatal pos "  Ill-kinded type."

    | E.PartialDataConstructorApplication (pos, xa, ga) ->
      fatal pos (s "  Partial data constructor application
                      (given %d, expecting %d)."
                   ga xa)

    | E.TypingError pos ->
      fatal pos "  Type error."

    | E.IncompatibleTypes (pos, v1, v2) ->
      fatal pos (s "  The following two types are incompatible:\n    %s\n    %s"
                   (print_variable pos v1)
                   (print_variable pos v2))

    | E.UnboundIdentifier (pos, Name x) ->
      fatal pos (s "  `%s' is unbound." x)

    | E.UnboundClass (pos, TName x) ->
      fatal pos (s "  Class `%s' is unbound." x)

    | E.MultipleClassDefinitions (pos, TName x) ->
      fatal pos (s "  Class `%s' is defined several times." x)

    | E.OverlappingInstances (pos, TName x, TName y) ->
      fatal pos (s "  Class `%s' has two instances on head symbol `%s'."
                   x y)

    | E.InvalidClassPredicateInContext (pos, TName x) ->
      fatal pos (s "  Class predicate `%s' is applied to something\
                      \ other than a variable."
                   x)

    | E.IncompatibleLabel (pos, LName x) ->
      fatal pos (s "  The label '%s' is not part of this record type." x)

    | E.UnreachableConstraint (pos, Types.ClassPredicate (TName c, TName x)) ->
      fatal pos (s "  Unreachable constraint `%s %s'." c x)

    | ExternalizeTypes.RecursiveType pos ->
      fatal pos (s "  Type error.")
