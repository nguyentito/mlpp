(* Trying to fit into a mechanical generic pattern, instead of
   doing the most clever thing possible on a case-by-case basis *)

(* Notes:
   - polymorphic recursion + higher kinds probably works
     (but it's not really important)
   - abandoned first-class modules, use local functors instead
     (should work better)
*)

module type TypeCon = sig
  type 'a t
end

module TList : TypeCon with type 'a t = 'a list = struct
  type 'a t = 'a list
end

module type Functor = sig
  type 'a f
  val fmap : ('a -> 'b) -> 'a f -> 'b f
end

module Wrapper_fmap
  = functor (T : TypeCon) -> 
    functor (F : Functor with type 'a f = 'a T.t) -> struct
      let fmap = F.fmap
    end

(* rec or not rec? That is the question...
   use the same criteria as for big_env vs small_env *)
(* In case of rec, the signature is mandatory *)
module rec FunctorList : Functor with type 'a f = 'a list = struct
  type 'a f = 'a list
  let fmap f = function 
    | [] -> []
    (* polymorphic recursion! yay! *)
    | x :: xs -> f x :: (let module X = Wrapper_fmap(TList)(FunctorList) in X.fmap) f xs
end


(* Lax monoidal functors, defined by I -> F I
   and F A \otimes F B -> F (A \otimes B) *)
module type Applicative = sig
  type 'a f
  module SuperclassFunctor : Functor with type 'a f = 'a f
  val appli_unit : unit f
  val appli_pair : 'a f -> 'b f -> ('a * 'b) f
end

module Wrapper_appli_unit
  = functor (T : TypeCon) -> 
    functor (F : Applicative with type 'a f = 'a T.t) -> struct
      let appli_unit = F.appli_unit
    end

module Wrapper_appli_pair 
  = functor (T : TypeCon) -> 
    functor (F : Applicative with type 'a f = 'a T.t) -> struct
      let appli_pair = F.appli_pair
    end

module ApplicativeList : Applicative with type 'a f = 'a list = struct
  type 'a f = 'a list
  module SuperclassFunctor = FunctorList
  let appli_unit = [()]
  let appli_pair xs ys = List.concat (List.map (fun x -> List.map (fun y -> (x, y)) ys) xs)
end

module Wrapper_pure
  = functor (T : TypeCon) -> 
    functor (F : Applicative with type 'a f = 'a T.t) -> struct
      let pure : type a. a -> a T.t = fun x ->
        (let module X = Wrapper_fmap(T)(F.SuperclassFunctor) in X.fmap)
          (fun () -> x)
          (let module X = Wrapper_appli_unit(T)(F) in X.appli_unit)
    end

module Wrapper_ap 
  = functor (T : TypeCon) -> 
    functor (F : Applicative with type 'a f = 'a T.t) -> struct
      let ap : type a b. (a -> b) T.t -> a T.t -> b T.t = fun f x ->
        (let module X = Wrapper_fmap(T)(F.SuperclassFunctor) in X.fmap)
          (fun (f', x') -> f' x')
          ((let module X = Wrapper_appli_pair(T)(F) in X.appli_pair) f x)
    end

module TOpt = struct
  type 'a t = 'a option
end

module rec FunctorOpt : Functor with type 'a f = 'a option = struct
  type 'a f = 'a option
  let fmap f = function 
    | None -> None
    | Some x -> Some (f x)
end

module ApplicativeOpt : Applicative with type 'a f = 'a option = struct
  type 'a f = 'a option
  module SuperclassFunctor = FunctorOpt
  let appli_unit = Some ()
  let appli_pair x_opt y_opt = match x_opt, y_opt with
    | None, _ -> None
    | _, None -> None
    | Some x, Some y -> Some (x, y)
end

(* Local overloaded function *)
let foobar =
  let module Wrapper_liftA2
      = functor (T : TypeCon) -> 
        functor (F : Applicative with type 'a f = 'a T.t) -> struct
          let liftA2 : type a b c. (a -> b -> c) -> a T.t -> b T.t -> c T.t = fun f x y ->
            (let module X = Wrapper_ap(T)(F) in X.ap)
              ((let module X = Wrapper_fmap(T)(F.SuperclassFunctor) in X.fmap)
                  f x)
              y
        end
  in
  ((let module X = Wrapper_liftA2(TList)(ApplicativeList) in X.liftA2)
    (+) [1;2] [5;42],
   (let module X = Wrapper_liftA2(TOpt)(ApplicativeOpt) in X.liftA2)
    ( * ) (Some 6) (Some 7))
    

module type Monad = sig
  type 'a m
  module SuperclassApplicative : Applicative with type 'a f = 'a m
  val join : 'a m m -> 'a m
end

module MonadList : Monad with type 'a m = 'a list = struct
  type 'a m = 'a list
  module SuperclassApplicative = ApplicativeList
  let join = List.concat
end

module MonadOpt : Monad with type 'a m = 'a option = struct
  type 'a m = 'a option
  module SuperclassApplicative = ApplicativeOpt
  let join = function (Some x) -> x | _ -> None
end

module Wrapper_join
  = functor (T : TypeCon) -> 
    functor (M : Monad with type 'a m = 'a T.t) -> struct
      let join = M.join
    end

module Wrapper_bind
  = functor (T : TypeCon) -> 
    functor (M : Monad with type 'a m = 'a T.t) -> struct
      let bind : type a b. a T.t -> (a -> b T.t) -> b T.t = fun m k ->
        (let module X = Wrapper_join(T)(M) in X.join)
          ((let module X = 
                  Wrapper_fmap(T)(M.SuperclassApplicative.SuperclassFunctor)
            in X.fmap)
              k
              m)
    end


