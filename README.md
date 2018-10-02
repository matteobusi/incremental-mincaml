# README - Incremental Type Checking of MinCaml

This project is a proof of concept in a very early stages (see below
for TODOs and known issues) implementing the incremental type checking for MinCaml
described in ``Using Standard Typing Algorithms Incrementally''.

## Requirement to build the project #

The project requires:

- [OCaml >= 4.06.1](http://www.ocaml.org/) standard compilers and tools
- [Ounit/OUnit2](http://ounit.forge.ocamlcore.org/) OCaml Unit testing library.
<<<<<<< HEAD

## Building the project #
Typing `make` will generate a `main.byte` executable, that you can run to try the incremental type-checker:
=======
- [Batteries](http://batteries.forge.ocamlcore.org/) OCaml alternative standard library.
- [Benchmark](http://ocaml-benchmark.forge.ocamlcore.org/) OCaml library for benchmarking.
- [Python 3](https://www.python.org/), [Pandas](https://pandas.pydata.org/), [Matplotlib](https://matplotlib.org/) and [NumPy](http://www.numpy.org/) for producing the plots and analyse the data.

## Building the project #

Typing `make` will generate a `main.byte` executable, that you can run to try the incremental typechecker:
>>>>>>> experiments
```
$ make
```

To clean-up the folder, run:
```
$ make clean
```

To build the test suite, run:
```
$ make test
```

To build the benchmarks, run:
```
$ make tbenchmark
```

To build everything, run:
```
$ make all
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
<<<<<<< HEAD
The first two lines mean that ``fact_opt.ml`` was type checked incrementally as ``unit`` (``IType: unit``) starting
from the original typing information of ``fact.ml`` (``Type : unit``).
The last line reports some statistics on the number of visited nodes and the usage of the cache.
In particular, the incremental algorithm visited 8 nodes of the syntax tree (on a total of 20).
It found the needed typing information directly in the cache for 5 times (``H: 5`` cache hits).
There were 3 cache misses, 0 because of incompatible typing environment ( ``0 (I)``) and 3 because of missing information
(``3 (NF)``).
=======
meaning that ``fact_opt.ml`` was type checked incrementally as ``unit`` (``IType: unit``) starting from the original typing information of ``fact.ml``.
To do that the incremental algorithm 8 nodes of the syntax tree (on a total of 20), finding the needed typing information directly in the TypingCache for 5 times and not finding it for 3 times (0 because of incompatible typing environment and 3 because they were never TypingCached).
>>>>>>> experiments

## Running the test suite #

To run the test suite, simply compile the executable as described above and then run the executable:
```
$ ./test.byte
```

## Running the benchmarks #

To run the benchmarks, simply compile the executable as described above and then run the executable:
```
$ ./tbenchmark.native repeat time min_depth max_depth csv
```
where `repeat` is the number of repeats of the same experiment, `time` is the running time of each experiment, `min_depth` and `max_depth` specify the size of sythetic programs and `csv` is a boolean that controls whether the output should be tabular or in csv format.

## Reproducing the experiments #

To reproduce the experiments it is enough to compile `tbenchmark` and then to run:
```
$ ./tbenchmark.native 5 2 8 16 true > results-5-2-8-16.csv; python analysis/analyze.py results-5-2-8-16-final.csv /plots
```
the create a new folder `plot` with all the generated plots and a csv file with the raw results.

## Project structure #

Here is a description of content of the repository

<<<<<<< HEAD
     src/            <-- source code lives here
     test/	     <-- unit test for code implementing the cache and the incremental type checker
     examples/       <-- small MinCaml programs
=======
     src/                 <-- source code lives here
     test/	              <-- unit test for code implementing the TypingCache and the incremental type checker
     examples/            <-- small MinCaml programs
>>>>>>> experiments

     Makefile             <-- Driver for `make` (uses OCB)
     .merlin              <-- Merlin configuration
     _tags                <-- OCamlBuild configuration

## The source code

The `src/` directory defines:

<<<<<<< HEAD
     annotast.ml      <-- the annotated abstract syntax tree
     cache.ml         <-- the cache for incremental type checking
     incrementaltc.ml <-- the actual incremental type checker obtained according to the method in [1]
     typing.ml        <-- the original type checking algorithm
=======
     annotast.ml          <-- the annotated abstrac syntax tree
     cache.ml             <-- the TypingCache for incremental type checking
     incremental.ml       <-- the actual incremental type checker, obtained following our methodology
     typing.ml            <-- the original type checking algorithm
>>>>>>> experiments

## Known issues (to solve in future versions) #
- None, yet

## TODOS #
- Incremental type inference
- Nameless implementation
<<<<<<< HEAD
- More efficient representation of caches
=======
>>>>>>> experiments
- Other analyses
