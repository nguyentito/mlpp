open Operators


module BiSetOp = 
  functor (Set1 : Set.S) ->
    functor (Set2 : Set.S) ->
struct
  
  (** Map on a set **)
  let map f s =
    Set1.fold (Set2.add =< f) s Set2.empty
    
end
