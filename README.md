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

To clean the build files, just do `make clean`. Note: you have to
separately clean the pretty-printer (in
`parsing/pprint-20130324/src/`).

Testing
-------

Negative tests:

    cd test/elaboration/bad/
    make

This runs the program on every `.mle` file in the directory and
displays a formatted log of the test results. OK means test successful
i.e. the test file was rejected (since these are negative tests).

Positive tests:

    cd test/elaboration/good/
    make

Pretty much the same thing.


Source code documentation
-------------------------

    ocamldoc -html -d doc -I elaboration/ -I inference/ -I common/ **/*.mli


