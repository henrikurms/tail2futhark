With `tail2Futhark` it is possible to compile APL programs - through [apltail](https://github.com/melsman/apltail) - to [Futhark](https://github.com/HIPERFIT/futhark) and thereby target GPGPU architectures supported by the Futhark project.

## Status

[![Build Status](https://travis-ci.org/henrikurms/tail2futhark.svg?branch=master)](https://travis-ci.org/henrikurms/tail2futhark)

## Installation

Use [Stack](http://haskellstack.org) to build the `tail2futhark` compiler.

To get all the prerequisites for building (including, if necessary,
the appropriate version of the Haskell compiler), run:

    stack setup

*Notice that this command will not install anything system-wide and will have no
effect outside the tail2futhark build directory*.  Now, run the
following command to build the `tail2futhark` compiler, including all
dependencies:

    stack build

You can install `tail2futhark` to your `$HOME/.local/bin` directory
by running:

    stack install

Make sure this directory is in your `$PATH`.  Alternatively, just copy
the binary to where you need it.

## Usage

There are two ways to invoke `tail2futhark`, corresponding to whether
you want to compile a program with a `main` function, or a library
with multiple entry points.  In the former case, run

    tail2futhark prog.tail -o prog.fut

which results in a Futhark program defining a `main` function.

In the latter case, run

    tail2futhark --library prog1.tail ... progN.tail -o prog.fut

This command results in a Futhark program with one entry point for every
`.tail` file.  The name of each entry point will match the name of the
file, with `.tail` stripped off.


## APL to TAIL

While stricly not the domain of `tail2futhark`, this is a handy
command line for translating an APL file to a TAIL file suitable for
consumption by `tail2futhark`:

    aplt -p_types -s_tail -c -o foo.tail ${TAIL_ROOT}/lib/prelude.apl foo.apl

Here, `$TAIL_ROOT` must point to a checkout of the TAIL source
repository.

## Testing

To test the compiler, run the following command:

    stack test

This command will execute a number of tests and print a test report on
stdout. An alternative is to run

    (cd tests/basic_tests; make test)
    
Yet an alternative is to run

    (cd tests/basic_tests; make testopencl)

The test source files are really APL-files and to (re)generate TAIL
programs from the APL-files, you need the `apltail` compiler,
available from https://github.com/melsman/apltail. See the `Makefile` in the `tests/basic_tests` directory for details.
