ML++ (aka ML with classes)
==========================

This is a project for a course in functional programming and type
systems (MPRI 2-4).

TODO: fill README later.

Requirements
------------

* OCaml >= 4.00
* The Menhir parser generator
* For now, nothing else. Should we add a dependency on ExtLib?

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

    runhaskell run-tests.hs inference good
    runhaskell run-tests.hs inference bad

Pretty much the same thing, except `[[Success]]` and `[OK]` match.

#### Cleaning up

    runhaskell run-tests.hs (inference|elaboration) (good|bad) clean

Deletes the generated `.mle`/`.mlr` files.


Source code documentation
-------------------------

    ocamldoc -html -d doc -I elaboration/ -I inference/ -I common/ **/*.mli


