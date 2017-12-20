# memo-AG

#### What is this?

This repository contains source code to benchmark the performance (speed and memory profiling) of various implementations of ZipperAG's.

#### What is ZipperAG?

ZipperAG is a  modular, zipper-based, memoized implementation of Attribute Grammars (AG). Implemented in Haskell, can be found in [Hackage](https://hackage.haskell.org/package/ZipperAG).

Reference publications:
* *"Zipper-Based Attribute Grammars and Their Extensions"*  published [here](https://link.springer.com/chapter/10.1007/978-3-642-40922-6_10).
* *"Embedding attribute grammars and their extensions using functional zippers"* published [here](https://dl.acm.org/citation.cfm?id=3044220.3064441).
* *"Memoized Zipper-Based Attribute Grammars"* published [here](https://www.researchgate.net/publication/308277211_Memoized_Zipper-Based_Attribute_Grammars).

#### What is inside this repository?

Inside this repository you can find 5 implementations of attribute grammars, in 5 separate folders:
* `Algol68`: Name resolution with inner scoping.
* `Desk`: A language processing task, with variable resolution and variable scope checking. Originally found [here](https://dl.acm.org/citation.cfm?id=197409).
* `HTMLTableFormatter`: Deep traversal algorithm for automatic sizing pf HTML tables.
* `LetIn`
* `Repmin`: The classic example of AGs: deep traversal and cloning of a binary tree with node re-writing.

Each AG folder contains:
* `Benchmnark.hs` with the main benchmarking code.
* `Original.hs` with traditional, non-memoized implementations of the AG.
* `Memo.hs` with memoized implementations of the AG.
* `Shared.hs` with shared datatypes for the memoized and non-memoized versions.
* Some folders contains a `Memo2.hs`, which is an implementation with selective memoization.

#### How do I run the benchmarks?

Start with the `Makefile`. It has 4 sets of self-descriptive options: `compile-speed` and `run-speed`; and `compile-memory` and `run-memory`. Running benchmarks implies **manually editing the Makefile**, to uncomment the desired target and comment anything else. Currently the `Makefile` is setup to `compile-*` and `run-*` the `Repming` AG.

You also need to **manually edit the `Benchmark.hs`** file for each AG. This file contains various `main` functions. Each `Benchmark.hs` file has labels indicating what should and should not be commented for each benchmark. Note that for speed benchmarking different versions of an AG can be analyzed at the same time, while for memory profilling each version has to be manually uncommented for analysis.

For memory profiling dependencies have to be installed with `-p`: `cabal install -p some_dependency`. It is advised to comment the `Criterion` dependency on each `Benchmark.hs` file for memory profiling, since it is large and only required for speed benchmarking.

#### I want to know more.

Let's have a chat then. Send an email to one (or even all) of the authors on the papers above. Alternatively add an `Issue` to this repository.

##### Happy benchmarking!
