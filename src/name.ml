type name = Name of string

type dname = DName of string

type lname = LName of string

type tname = TName of string

module type OrderedType = Map.OrderedType

module OrderedName = struct
  type t = name
  let compare (Name x) (Name y) = compare x y
end

module OrderedLName = struct
  type t = lname
  let compare (LName x) (LName y) = compare x y
end

module OrderedTName = struct
  type t = tname
  let compare (TName x) (TName y) = compare x y
end

