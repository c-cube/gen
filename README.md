# Gen

Iterators for OCaml, both restartable and consumable. The implementation
keeps a good balance between simplicity and performance.

The library is extensively tested using `qtest`. If you find a bug,
please report!

The documentation can be found [here](http://c-cube.github.io/gen/)
the main module is [Gen](https://github.com/c-cube/gen/blob/master/src/gen.mli)
and should suffice for 95% of use cases.

[Changelog](https://github.com/c-cube/gen/blob/master/CHANGELOG.md)

## Native install

Installation in a native OCaml project, via [opam](https://opam.ocaml.org/):

```sh
$ opam install gen
```

or, manually, by building the library and running `make install`. Opam is
recommended, for it keeps the library up-to-date.

## BuckleScript install

Installation in JavaScript, via [BuckleScript](https://bucklescript.github.io/bucklescript/Manual.html),
in an [npm](https://npmjs.com/) project:

1. Install this package:

   ```sh
   $ npm install bs-gen
   ```

2. Manually add `bs-gen` to your `bsconfig.json`'s `bs-dependencies`:

   ```json
   "bs-dependencies": [
      ...
      "bs-gen"
   ],
   ```

## Use

You can either build and install the library (see "Build"), or just copy
files to your own project. The last solution has the benefits that you
don't have additional dependencies nor build complications (and it may enable
more inlining).

If you have comments, requests, or bugfixes, please share them! :-)

## Build

There are no dependencies except for `dune` for building. This should work with `OCaml>=4.02`

```sh
$ make
```

To build and run tests (requires `oUnit` and `qtest`):

```sh
$ opam install oUnit qtest
$ make test
```

## License

This code is free, under the BSD license.
