# README - Incremental Type Checking of MinCaml

This project is a proof of concept in a very early stages (see below
for TODOs and known issues) implementing the incremental type checking for MinCaml
described in ``Using Standard Typing Algorithms Incrementally''.

## Requirement to build the project #

The project requires:

- [OCaml >= 4.06.1](http://www.ocaml.org/) standard compilers and tools
- [Ounit/OUnit2](http://ounit.forge.ocamlcore.org/) OCaml Unit testing library.
- [Batteries](http://batteries.forge.ocamlcore.org/) OCaml alternative standard library.
- [Benchmark](http://ocaml-benchmark.forge.ocamlcore.org/) OCaml library for benchmarking.

## Building the project #
Typing `make` will generate a `main.byte` executable, that you can run to try the incremental typechecker:
```
$ make
```

To clean-up the folder, run:
```
$ make clean
```

To build the benchmarks, run:
```
$ make tbenchmark
```

## Running #
To execute the incremental type checker simply run:
```
$ ./main.byte <path/to/original.ml> <path/to/modified.ml>
```
where the first argument is the path to a program to be type checked, and the second one is the path to the modified program, to be type checked incrementally.

## Execution example

When running the ``main.byte`` as
```
$ ./main.byte examples/fact.ml examples/fact_opt.ml
```
the output is
```
Analyzing: Orig: examples/fact.ml Mod: examples/fact_opt.ml ...
Type: unit - IType: unit
[Visited: 8/20] H: 5 - M: 0 (I) + 3 (NF) = 3
```
meaning that ``fact_opt.ml`` was type checked incrementally as ``unit`` (``IType: unit``) starting from the original typing information of ``fact.ml``.
To do that the incremental algorithm 8 nodes of the syntax tree (on a total of 20), finding the needed typing information directly in the TypingCache for 5 times and not finding it for 3 times (0 because of incompatible typing environment and 3 because they were never TypingCached).

## Running the test #
To run the tests, simply compile the `test.ml` file:
```
$ make test
```
and run the executable:
```
$ ./test.byte
```

## Running the benchmarks #

To build the benchmarks, simply compile `tbenchmark.ml` file:
```
$ make tbenchmark
```
and run the executable:
```
$ ./tbenchmark.native
```

## Project structure #

Here is a description of content of the repository

     src/                 <-- source code lives here
     test/	              <-- unit test for code implementing the TypingCache and the incremental type checker
     examples/            <-- small MinCaml programs

     Makefile             <-- Driver for `make` (uses OCB)
     .merlin              <-- Merlin configuration
     _tags                <-- OCamlBuild configuration

## The source code

The `src/` directory defines:

     annotast.ml          <-- the annotated abstrac syntax tree
     cache.ml             <-- the TypingCache for incremental type checking
     incremental.ml <-- the actual incremental type checker obtained according to the method in [1]
     typing.ml            <-- the original type checking algorithm

## Known issues (to solve in future versions) #
- None, yet

## TODOS #
- Incremental type inference
- Nameless implementation
- More efficient representation of TypingCaches
- Other analyses
