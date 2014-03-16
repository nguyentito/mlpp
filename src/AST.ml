open Name
open Positions
open Types

module Make (P : Types.TypingSyntax) = struct

  include P

  type program = block list

  and block =
    | BClassDefinition of class_definition
    | BInstanceDefinitions of instance_definition list
    | BTypeDefinitions of type_mutual_definitions
    | BDefinition of value_binding

    (* special-purpose hack: module signature with a single type constructor *)
    | BModuleSig of string * tname * (name * mltype) list (* TODO: should be a set, like all those below... *)
    (* Functor arguments + members *)
    | BModule of module_definition

  (* Foo with type 'a <tname> = 'a <string> *)
  and module_type = string * (tname * string) option

  and module_definition = { module_name : string;
                            module_functor_args : (string * module_type) list;
                            module_signature : module_type option;
                            module_members : block list;
                            module_is_recursive : bool
                          }

  (* No support for functor inside modules... *)
  and module_expr =
    | FunctorApp of string * module_expr list
    | ModulePath of string list 
    (* support a special case: struct type 'a t = 'a foobar end *)
    | InlineStruct of mltype


  and class_definition = {
    is_constructor_class : bool;
    class_position  : position;
    class_parameter : tname;
    superclasses    : tname list;
    class_name      : tname;
    class_members   : (position * lname * mltype) list; (* TODO: this should be a set, or a map 
                                                           Problem : declaration order (should be preserved) *)
  }

  and instance_definition = {
    instance_position       : position;
    instance_parameters     : type_var_name list; (* FIXME: use a polymorphic variant, bitch! *)
    instance_typing_context : class_predicate list; (* TODO: rename *)
    instance_class_name     : type_class_name;
    instance_index          : type_constr_name;
    instance_members        : record_binding list; (* TODO: this should be a set, or even a map *)
  }

  and value_binding =
    | BindValue of position * value_definition list
    | BindRecValue of position * value_definition list
    | ExternalValue of position * tnames * binding * string

  and type_mutual_definitions =
    | TypeDefs of position * type_definition list

  and expression =

    (** Core ML. *)
    | EVar of position * name * instantiation
    | ELambda of position * binding * expression
    | EApp of position * expression * expression
    | EBinding of position * value_binding * expression
    | EPrimitive of position * primitive

    (** Type abstraction. *)
    | EForall of position * tname list * expression

    (** Type annotations. *)
    | EExists of position * tname list * expression
    | ETypeConstraint of position * expression * mltype

    (** Algebraic datatypes. *)
    | EDCon of position * dname * instantiation * expression list
    | EMatch of position * expression * branch list

    (** Records. *)
    | ERecordAccess of position * expression * lname
    | ERecordCon of position * name * instantiation * record_binding list  (* TODO: this should be a set *)

    (* Access a member of a module;
       this actually means (let module X = (...) in X.(...),
       since Set.Make(String).empty is a syntax error *)
    | EModuleAccess of module_expr * name

  (** Constant. *)
  and primitive =
    | PIntegerConstant of int     (** Integer constant. *)
    | PCharConstant of char       (** Character constant. *)
    | PUnit                       (** Unit constant. *)

  (** Pattern matching branch. *)
  and branch =
    | Branch of position * pattern * expression

  and record_binding =
    | RecordBinding of lname * expression

  and type_definition =
    | TypeDef of position * mltypekind * tname * datatype_definition
    | ExternalType of position * tnames * tname * string

  and datatype_definition =
    | DAlgebraic of (position * dname * tnames * mltype) list
    | DRecordType of tnames * (position * lname * mltype) list

  (** A value definition consists of a list of explicit universal
      quantifiers, a binding, and an expression. *)
  and value_definition =
    | ValueDef of position * tnames * class_predicates * binding * expression
    (* let module Foobar = ... in ... *)
    (* should fail miserably in a value_binding with multiple elements... *)
    | VLocalModule of module_definition

  and pattern =
    | PVar of position * name
    | PWildcard of position
    | PAlias of position * name * pattern
    | PTypeConstraint of position * pattern * mltype
    | PPrimitive of position * primitive
    | PData of position * dname * instantiation * pattern list
    | PAnd of position * pattern list
    | POr of position * pattern list

  and tnames = tname list

  and mltype = Types.mltype

  and mltypescheme = Types.scheme

  and mltypekind = Types.kind

end

module Generic = Make (struct
  type binding
  let binding _ _ = assert false
  let destruct_binding _ = assert false
  type instantiation
  let instantiation _ _ = assert false
  let destruct_instantiation_as_type_applications _ = assert false
  let destruct_instantiation_as_type_constraint _ = assert false
  let implicit = false
end)

module type GenericS = module type of Generic
