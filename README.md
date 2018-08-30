# README - Incremental Type Checking of MinCaml

This project is a proof of concept in a very early stages (see below
for TODOs and known issues) implementing the incremental type checking for MinCaml
described in ``Using Standard Typing Algorithms Incrementally''.

## Requirement to build the project #

The project requires:

- [OCaml >= 4.06.1](http://www.ocaml.org/) standard compilers and tools
- [Ounit/OUnit2](http://ounit.forge.ocamlcore.org/) OCaml Unit testing library. 
- [Nearley](https://github.com/kach/nearley) Parser generator with unparsing function.
- [Batteries] OCaml alternative standard library.
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

     src/                 <-- source code lives here
     test/	              <-- unit test for code implementing the TypingCache and the incremental type checker
     examples/            <-- small MinCaml programs

     Makefile             <-- Driver for `make` (uses OCB)
     .merlin              <-- Merlin configuration
     _tags                <-- OCamlBuild configuration

## The source code

The `src/` directory defines:

     annotast.ml          <-- the annotated abstrac syntax tree
     TypingCache.ml             <-- the TypingCache for incremental type checking
     incrementaltyping.ml <-- the actual incremental type checker obtained according to the method in [1]
     typing.ml            <-- the original type checking algorithm

## Known issues (to solve in future versions) #
- None, yet

## TODOS #
- Incremental type inference
- Nameless implementation
- More efficient representation of TypingCaches
- Other analyses

## Useful benchmarks #

### To simulate code modifications 
- Same aAST (i.e. no modifications) - from depth d \in 2, 4, ..., 20
- To simulate code addition, simply invalidate the cache from certain depth of the tree 
- To simulate code motion simply swap two subtrees
- To simulate code deletion simply delete a subtree and substitute it with a smaller one or a leaf
- ??

### To test caches
- Start with an empty cache (e.g. first time you see the whole tree); This is the "first time" see
- Analyse with non-empty cache, but limited and see what happens
- ??