module type Foo = sig
  type t
  val foo : t
end

module FooInt : Foo with type t = int = struct
  type t = int
  let foo = 42
end

let f : type a. (module Foo with type t = a) -> a = fun x ->
  let (module FooX) = x in FooX.foo

module type Bar = sig
  type 'a t
  val foo : 'a t -> 'a
end

module BarList : Bar with type 'a t = 'a list = struct
  type 'a t = 'a list
  let foo = List.hd
end


