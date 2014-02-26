let ($) f x = f x

(* FIXME: This one is defined in pervasives (with another name).
   But at the time, caml.inria.fr doesn't respond *)
let (=<) f g = fun x -> f (g x)
