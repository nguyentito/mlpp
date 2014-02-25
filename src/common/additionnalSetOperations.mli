(** Provides additionnal operations to Ocaml basic Set type **)
(** They might have to be defined via functors, due to the 
    functor type of Set.Make **)


module BiSetOp :
  functor (Set1 : Set.S) ->
    functor (Set2 : Set.S) ->
sig
  
  (** Map on a set **)
  val map : (Set1.elt -> Set2.elt) -> Set1.t -> Set2.t 
    
end
