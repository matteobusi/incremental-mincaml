# README - Incremental Type Checking of MinCaml

This project is a proof of concept implementing the incrementalization framework described in
> [M. Busi, P. Degano and L. Galletta. Mechanical Incrementalization of Typing Algorithms](tba).

As an example we also provide the standard and incremental type checker for MinCaml.

## Requirement to build the project #

The project requires:

- [OCaml >= 4.07.0](http://www.ocaml.org/) standard compilers and tools
- [OUnit2](http://ounit.forge.ocamlcore.org/) OCaml Unit testing library.
- [Batteries](http://batteries.forge.ocamlcore.org/) OCaml alternative standard library.
- [Core_bench](https://github.com/janestreet/core_bench) OCaml library for micro-benchmarking.
- [Landmarks](https://github.com/LexiFi/landmarks) OCaml profiling library.
- [Python 3](https://www.python.org/), [Pandas](https://pandas.pydata.org/), [Matplotlib](https://matplotlib.org/) and [NumPy](http://www.numpy.org/) for producing the plots and analyse the data.

## Building the project #

Typing `make` will generate a `main.native` executable, that you can run to try the automatically incrementalized typechecker for MinCaml:
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

To build the executable running the time experiments, run:
```
$ make texperiments
```

To build the memory experiments, run:
```
$ make mexperiments
```

To build everything, run:
```
$ make all
```

## Running #
To execute the incrementalized type checker simply run:
```
$ ./main.native <path/to/original.ml> <path/to/modified.ml>
```
where the first argument is the path to a program to be type checked, and the second one is the path to the modified program, to be type checked incrementally.

## Execution example

When running the ``main.native`` as
```
$ ./main.native examples/fact.ml examples/fact_opt.ml
```
the output is
```
Analyzing: Orig: src/fun/examples/fact.ml ... Mod: src/fun/examples/fact_opt.ml ...
Type: unit - IType: unit
[Visited: 8/20] O: 3 - H: 2 - M: 0 (I) + 3 (NF) = 3
```
meaning that ``fact_opt.ml`` was type checked incrementally as ``unit`` (``IType: unit``) starting from the original typing information of ``fact.ml``.
To do that the incremental algorithm visited 8 nodes of the syntax tree (out of a total of 20), finding the needed typing information directly in the cache for 2 times, not finding it for 3 times (0 because of incompatible typing environment and 3 because they were not found) and resorting to the original type checker for 3 times.

## Running the test suite #

To run the test suite, simply compile the executable as described above and then run the executable:
```
$ ./test.byte
```

## Running the time experiments #

To run the time experiments, simply compile the executable as described above and then run the executable:
```
$ ./texperiments.native quota min_depth max_depth
```
where `quota` is the running time of each experiment, and `min_depth` and `max_depth` specify the size of synthetic programs.

## Running the memory experiments #

To run the memory experiments, simply compile the executable as described above and then run the executable:
```
$ OCAML_LANDMARKS=on ./mexperiments.native min_depth max_depth cache
```
where `min_depth` and `max_depth` specify the size of sythetic programs and `cache` is a boolean that controls whether the script should measure the memory overhead of the cache alone or that of the whole incremental run.

## Reproducing the experiments #

To reproduce the experiments presented of the paper, it is enough to compile `texperiments` and then to run:
```
$ ./texperiments.native 10 8 16 true > results-10-8-16.csv; python analysis/tanalyze.py results-10-8-16.csv /plots
```
It creates a new folder `plots` with all the generated plots and a csv file with the raw results.

To reproduce the memory experiments presented of the paper, it is enough to compile `mexperiments` and then to run:
```
$ OCAML_LANDMARKS=on ./mexperiments.native 10 16 true > mem_cache.json; OCAML_LANDMARKS=on ./mexperiments.native 10 16 false > mem_all.json; python analysis/manalyze.py mem_cache.json mem_all.json
```
It produces the latex code for the tables are reported in the paper.

## Project structure #

Here is a description of content of the repository

     analysis/                          <-- Python scripts to analyse the results of the experiments
     src/                               <-- The source code lives here

     Makefile                           <-- Driver for `make` (uses OCB)
     .merlin                            <-- Merlin configuration
     _tags                              <-- OCamlBuild configuration

## The source code

The `src/` directory defines:

     lib/                               <-- Library with the functors implementing the framework from the paper
          languageSpecification.mli     <-- Interface that must be implemented to be able to incrementalize a type system
          original.ml                   <-- Functor that given a language specification returns a non-incremental type system
          incrementalizer.ml            <-- Functor that given a language specification returns an incremental type system
     fun/                               <-- MinCaml folder
          examples                      <-- Simple MinCaml examples
          langspec                      <-- An example implementation of the MinCaml type checker in our framework
          tests                         <-- Unit tests and time/memory experiments
          main.ml                       <-- Example usage of the generated type systems

## Known issues (to solve in future versions) #
- None, yet

