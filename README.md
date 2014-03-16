ML++ (aka ML with classes)
==========================

This is a project for a course in functional programming and type
systems (MPRI 2-4). Its goal is to add type classes to a ML-like
language using a source-to-source translation, and to support
type inference for typeclasses.


Awesome feature: constructor classes!
-------------------------------------

The language for this project is supposed to be called _Mini-Haskell_;
but it doesn't even have Monads, Haskell's most famous feature!
In order to support them, we have added (limited) support for
_constructor classes_, which includes polymorphism with type variables
of kind * -> * and the ability for methods to be polymorphic in more 
type variables than just the parameter of their class.

To make this fit into the pre-existing framework, we had to resort
to piling up ugly hacks on top of the code base. For example, if you
try to put something complicated into an instance declaration for
a constructor class, it will break; you are advised to define your methods
separately and then just put an equality in the instance definition.

To enable this feature, use the `--fts` flag, which also lifts a
restriction on namespaces in the spec. A sample file is given in
`test/functor-applicative.mlt`. Test using

    ./src/front.native test/functor-applicative.mlt --fts --compile-with-ocaml

You will be able to witness the usage of ML modules and functors
to translate constructor classes.


Requirements
------------

* OCaml >= 4.00
* The Menhir parser generator

Compilation
-----------

    cd src/
    make

This results in an executable named `joujou` in `src/`.

To clean the build files, just do `make clean`.

The build system is now completely based on ocamlbuild, which makes
life a lot easier!

Testing
-------

To use the tests, first enter the directory `test`, which contains
the script `run-tests.hs`.

#### Negative tests

    runhaskell run-tests.hs elaboration bad

This runs the program on every `.mle` file in the directory and
displays a formatted log of the test results. 

When the compiler has returned with exit code 0, `[[Success]]` is
displayed, otherwise `[[Failure]]` is displayed. In the summary `[OK]`
means test successful i.e. the test file was rejected (since these are
negative tests). So `[[Failure]]` -> `[OK]` and `[[Success]]` ->
`[KO]`.

To test inference, run

    runhaskell run-tests.hs inference bad

Note that when testing inference, `--inference-only` is activated: the
program won't bother with elaboration.
     
#### Positive tests

    runhaskell run-tests.hs elaboration good
    runhaskell run-tests.hs inference good

Pretty much the same thing, except `[[Success]]` and `[OK]` match.

#### Cleaning up

    runhaskell run-tests.hs (inference|elaboration) (good|bad) clean

Deletes the generated `.mle`/`.mlr` files.


Source code documentation
-------------------------

    ocamldoc -html -d doc -I elaboration/ -I inference/ -I common/ **/*.mli


