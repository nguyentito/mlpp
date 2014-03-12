(* Direct translation from the generated sample.ml *)

module type ClassDictEq = sig
  type t
  val equal : t -> t -> bool
end

let equal : type a. (module ClassDictEq with type t = a) -> a -> a -> bool = fun x ->
  let (module X) = x in X.equal

let rec inst_dict_Eq_int = (module struct
  type t = int
  let equal x y = x = y
end : ClassDictEq with type t = int)

and inst_dict_Eq_char = (module struct
  type t = char
  let equal x y = x = y
end : ClassDictEq with type t = char)

and inst_dict_Eq_list
    : type a. (module ClassDictEq with type t = a) -> (module ClassDictEq with type t = a list)
    = fun dictionary_var_Eq_a -> (module struct
        type t = a list
        let equal l1 l2 = match l1 with
          | [] -> (match l2 with [] -> true | _ -> false)
          | x :: xs -> begin match l2 with
              | [] -> false
              | y :: ys ->
                equal dictionary_var_Eq_a x y
                && equal (inst_dict_Eq_list dictionary_var_Eq_a) xs ys
          end
    end : ClassDictEq with type t = a list)

module type ClassDictOrd = sig
  type t
  val superclass_field_Eq_Ord : (module ClassDictEq with type t = t)
  val lt : t -> t -> bool
end

let lt : type a. (module ClassDictOrd with type t = a) -> a -> a -> bool = fun x ->
  let (module X) = x in X.lt

let rec inst_dict_Ord_int = (module struct
  type t = int
  let superclass_field_Eq_Ord = inst_dict_Eq_int
  let lt = (<)
end : ClassDictOrd with type t = int)

and inst_dict_Ord_list
    : type a. (module ClassDictOrd with type t = a) -> (module ClassDictOrd with type t = a list)
    = fun dictionary_var_Ord_a -> (module struct
        type t = a list
        let superclass_field_Eq_Ord = let (module X) = dictionary_var_Ord_a in
                                      inst_dict_Eq_list X.superclass_field_Eq_Ord
        let lt l1 l2 = match l1 with
          | [] -> true
          | x :: xs -> begin match l2 with
              | [] -> false
              | y :: ys ->
                lt dictionary_var_Ord_a x y
                || (equal 
                      (let (module X) = dictionary_var_Ord_a in
                       X.superclass_field_Eq_Ord)
                      x y
                    && lt (inst_dict_Ord_list dictionary_var_Ord_a) xs ys)
          end
    end : ClassDictOrd with type t = a list)

