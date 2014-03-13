(* kind * -> * *)
module type Type1 = sig
  type 'a t
end

module type FMap = sig
  type 'a f
  val fmap : ('a -> 'b) -> 'a f -> 'b f
end

module Type1List : Type1 with type 'a t = 'a list = struct
  type 'a t = 'a list
end

module FMapList : FMap with type 'a f = 'a list = struct
  type 'a f = 'a list
  let fmap = List.map
end

module type Pointed = sig
  type 'a f
  val point : unit f
end

module PointedList : Pointed with type 'a f = 'a list = struct
  type 'a f = 'a list
  let point = [()]
end

module FMapShow =
  functor (T1 : Type1) ->
    functor (F : FMap with type 'a f = 'a T1.t) ->
      functor (P : Pointed with type 'a f = 'a T1.t) ->
        struct
          let foobar = F.fmap (fun () -> 42) P.point
        end 

