# tail2futhark


Installation
============

Use [Stack](http://haskellstack.org) to build the `tail2futhark` compiler.

To get all the prerequisites for building (including, if necessary,
the appropriate version of the Haskell compiler), run:

    stack setup

*Note that this will not install anything system-wide and will have no
effect outside the Tail2Futhark build directory*.  Now, run the
following command to build the `tail2futhark` compiler, including all
dependencies:

    stack build

You can install `tail2futhark` to your `$HOME/.local/bin`
by running:

    stack install

Make sure this directory is in your `$PATH`.  Alternatively, just copy
the binary to where you need it.

Testing
=======

To test the compiler, run the following command:

    (cd tests/basic_tests; make clean test)

This command will execute a number of tests and print a test report on
stdout.

The test source files are really APL-files and to (re)generate TAIL
programs from the APL-files, you need the `apltail` compiler,
available from https://github.com/melsman/apltail.
