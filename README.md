# README - Incremental Type Checking of MinCaml

This project is a proof of concept in a very early stages (see below
for TODOs and known issues) implementing the incremental type checking for MinCaml
described in ``Using Standard Typing Algorithms Incrementally''.

## Requirement to build the project #

The project requires:

- [OCaml >= 4.06.1](http://www.ocaml.org/) standard compilers and tools
- [Ounit/OUnit2](http://ounit.forge.ocamlcore.org/) OCaml Unit testing library.

## Building the project #
Typing `make` will generate a `main.byte` executable, that you can run to try the incremental type-checker:
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
where the first argument is the path to a program to be type checked, and the second
one is the path to the modified program, to be type checked incrementally.

## Usage example

When you run
```
$ ./main.byte examples/fact.ml examples/fact_opt.ml
```
the output is
```
Analyzing: Orig: examples/fact.ml Mod: examples/fact_opt.ml ...
Type: unit - IType: unit
[Visited: 8/20] H: 5 - M: 0 (I) + 3 (NF) = 3
```
The first two lines mean that ``fact_opt.ml`` was type checked incrementally as ``unit`` (``IType: unit``) starting
from the original typing information of ``fact.ml`` (``Type : unit``).
The last line reports some statistics on the number of visited nodes and the usage of the cache.
In particular, the incremental algorithm visited 8 nodes of the syntax tree (on a total of 20).
It found the needed typing information directly in the cache for 5 times (``H: 5`` cache hits).
There were 3 cache misses, 0 because of incompatible typing environment ( ``0 (I)``) and 3 because of missing information
(``3 (NF)``).

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

     src/            <-- source code lives here
     test/	     <-- unit test for code implementing the cache and the incremental type checker
     examples/       <-- small MinCaml programs

     Makefile        <-- Driver for `make` (uses OCB)
     .merlin         <-- Merlin configuration
     _tags           <-- OCamlBuild configuration

## The source code

The `src/` directory defines:

     annotast.ml      <-- the annotated abstract syntax tree
     cache.ml         <-- the cache for incremental type checking
     incrementaltc.ml <-- the actual incremental type checker obtained according to the method in [1]
     typing.ml        <-- the original type checking algorithm

## Known issues (to solve in future versions) #
- None, yet

### TODOS #
- Incremental type inference
- Nameless implementation
- More efficient representation of caches
- Other analyses
