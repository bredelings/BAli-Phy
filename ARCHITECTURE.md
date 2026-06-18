# BAli-Phy Code Overview

This directory is the BAli-Phy source tree: a C++23 phylogenetics and MCMC application with its own Haskell-like modeling language, runtime, built-in modules, command-line model bindings, tools, documentation, examples, and tests.

## High-Level Shape

- `meson.build` defines project `bali-phy` version 4.2, licensed under GPLv2, built with Meson and C++23.
- The main executable is `bali-phy`, with the entry point in `src/bali-phy/bali-phy.cc`.
- `src/` contains the C++ implementation.
- `haskell/` contains the Haskell-like standard library and domain modules installed with the program.
- `bindings/` contains JSON metadata that exposes functions, distributions, and models through the command-line interface.
- `tests/` contains fixtures and expected outputs for parsing, MCMC, IO, Haskell language behavior, probabilistic programs, and command-line tools.

## Main Components

- `src/bali-phy/`: application entry point, command-line parsing, path setup, help, run directory handling, and overall orchestration.
- `src/computation/`: custom Haskell-like compiler/interpreter pipeline, including parser, renamer, desugarer, typechecker, optimizer, runtime AST, lazy graph machine, effects, module loader, and evaluator.
- `src/builtins/`: C++ shared modules loaded by the runtime, such as `Prelude`, `Distribution`, `MCMC`, `Likelihood`, `SModel`, `Alignment`, `Matrix`, `File`, and `TreeDist`.
- `src/models/`: parser/compiler for model expressions and command-line model specifications, plus code generation for full BAli-Phy model programs.
- `src/mcmc/`: MCMC moves and samplers for branch lengths, topology, alignments, nodes, and dynamic-programming-backed alignment changes.
- `src/dp/`: dynamic programming engines for pairwise and multiway alignment likelihood/sampling.
- `src/substitution/`: substitution likelihood, parsimony, caches, and substitution operations.
- `src/alignment/`, `src/sequence/`, `src/tree/`, and `src/tree-align/`: biological sequence, alphabet, alignment, Newick tree, random tree, and indel/tree-alignment data structures and algorithms.
- `src/tools/`: standalone command-line utilities for alignments, trees, statistics, consensus, drawing, bootstrapping, and post-processing.

## Runtime Flow

The `bali-phy` executable parses CLI/config options, initializes settings and the random number generator, sets up the module loader, builds a `Program` from one of the requested modes (`--run`, `--print`, `--align`, or `--model`), initializes the graph-machine runtime, and runs `main`.

For alignment/model runs, it also creates the output directory, records run metadata, emits initial alignments when applicable, reports the log files that will be written, and then starts MCMC computation.

## Language and Model System

BAli-Phy does not use GHC. It ships its own Haskell-like standard library in `haskell/` and compiles/interprets it through `src/computation/`.

C++ functions are exposed to this language using declarations like:

```haskell
foreign import bpcall "module_name:cpp_func_name" haskell_name :: Type
```

The implementation lives in a corresponding C++ builtin module under `src/builtins/`, with exported C functions named with the `builtin_function_` prefix.

Command-line-visible models and functions are registered through JSON files under `bindings/`. These bindings describe the CLI name, Haskell call expression, imports, result type, constraints, arguments, defaults, citations, and descriptions.

## Other Directories

- `doc/` and `help/`: manual pages, user docs, generated help text, and developer docs.
- `examples/`: biological sequence, alignment, and tree example datasets.
- `scripts/`: helper scripts, R plotting/reporting scripts, benchmarking, and packaging helpers.
- `python/`: small Python helper package, mainly for printers.
- `external/` and `subprojects/`: vendored or fallback dependencies such as range-v3, immer, dlfcn-win32, fmt, and xxhash wraps.

