`bs-gen`
----------
This is c-cube's Gen iterator library for OCaml, cross-compiled to
JavaScript via [BuckleScript][] (an OCaml-to-JavaScript compiler) for
[Reason][] (an alternative OCaml syntax targeting that compiler.)

You can safely ignore the installation instructions below when compiling
to JS. Instead:

1. Install this fork through npm:

        npm install --save @elliottcable/bs-gen

2. Manually add `bs-gen` to your `bsconfig.json`'s `bs-dependencies`:

        "bs-dependencies": [
          ...
          "@elliottcable/bs-gen"
        ],

3. Use `Gen.t`!

Of note, I ran into some type errors when trying to compile with
`GenLabels` API enabled; thus, the npm version of this library omits
that entirly. All of the other modules are included, though.

   [BuckleScript]: <https://bucklescript.github.io/>
   [Reason]: <https://reasonml.github.io/>

#### Original README follows:

# Gen

Iterators for OCaml, both restartable and consumable. The implementation
keeps a good balance between simplicity and performance.

The library is extensively tested using `qtest`. If you find a bug,
please report!

The documentation can be found [here](http://cedeela.fr/~simon/software/gen);
the main module is [Gen](http://cedeela.fr/~simon/software/gen/Gen.html)
and should suffice for 95% of use cases.

## Install

    $ opam install gen

or, manually, by building the library and running `make install`. Opam is
recommended, for it keeps the library up-to-date.

## Use

You can either build and install the library (see "Build"), or just copy
files to your own project. The last solution has the benefits that you
don't have additional dependencies nor build complications (and it may enable
more inlining).

If you have comments, requests, or bugfixes, please share them! :-)

## Build

There are no dependencies. This should work with OCaml>=3.12.

    $ make

To build and run tests (requires `oUnit` and `qtest`):

    $ opam install oUnit qtest
    $ ./configure --enable-tests
    $ make test

## License

This code is free, under the BSD license.
