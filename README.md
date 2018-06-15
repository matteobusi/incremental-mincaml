# README - Incremental Type Checking of MinCaml

This project is a proof of concept in a very early stages (see below
for TODOs and known issues) implementing the incremental type checking for MinCaml
described in [1].

## Requirement to build the project #

The project requires:

- [OCaml >= 4.06.1](http://www.ocaml.org/) standard compilers and tools
- [Ounit/OUnit2](http://ounit.forge.ocamlcore.org/) OCaml Unit testing library. 

## Building the project #
Typing `make` will generate a `main.byte` executable, that you can run to try the incremental typechecker:
```
$ make
```

To clean-up the folder, run:
```
$ make clean
```

## Running #
To execute the incremental type checker simply run:
```
$ ./main.byte <path/to/original.ml> <path/to/modified.ml>
```
where the first argument is the path to a program to be type checked, and the second one is the path to the modified program, to be type checked incrementally.

## Running the test #
To run the tests, simply compile the `test.byte` file:
```
$ make test
```
and run it:
```
$ ./test.byte
```

## Project structure #

Here is a description of content of the repository

      src/           <-- source code lives here

      Makefile       <-- Driver for `make` (uses OCB)
      .merlin        <-- Merlin configuration
      _tags          <-- OCamlBuild configuration

## The source code

The `src/` directory defines:

    `annotast.ml` <-- the annotated abstrac syntax tree
    `cache.ml`    <-- the cache for incremental type checking
    `incrementaltc.ml` <-- the actual incremental type checker obtained according to the method in [1]
    `typing.ml` <-- the original type checking algorithm

## Known issues (to solve in future versions)
-

### TODOS #
1. Incremental type inference
2. Nameless implementation
3. More efficient representation of caches
4. Other analyses

[1] M. Busi, P. Degano, L. Galletta, ``Using Standard Algorithms Incrementally''