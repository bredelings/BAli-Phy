# Guidance for executable tests

## Problem

Adding a new executable for each feature or bug can leave test programs
scattered beside production library sources.  These executables often have no
clear subsystem owner, duplicate large link dependencies, behave differently
across platforms, and remain after the investigation that motivated them has
ended.

Useful checks should not automatically become permanent standalone programs.
The test's intended lifetime, ownership, and location must be decided before
it is retained.

## Default rule

Do not add a new test executable, Meson `executable()` target, or
feature-specific test runner unless explicitly requested.

Prefer, in order:

1. an existing language or integration test suite;
2. an existing test runner owned by the relevant subsystem; or
3. an existing program that naturally exposes the behavior under test.

Do not put unrelated tests into an existing executable merely to avoid
creating a new target.

If no suitable runner exists, stop and perform a design analysis of the
missing test infrastructure.  The analysis should consider:

- whether native executable coverage is actually required;
- which subsystem owns the tests;
- where test sources and Meson definitions should live;
- whether one runner can serve a meaningful collection of future tests;
- build and link cost;
- Windows and other supported-platform behavior; and
- whether higher-level tests or local assertions provide better coverage.

Adding a runner is justified only when its expected reuse and coverage
outweigh its maintenance and platform costs.

## Temporary diagnostic tests

A test written to locate a bug is not automatically a regression test.
Temporary harnesses should be made in a separate temporary `jj` change and
must not appear in the final commit series.  Abandon that change after the
investigation.

Before removing a temporary test, decide whether the knowledge it exposed
should be transferred to:

- a durable behavioral regression test;
- a local invariant assertion;
- a comment explaining a non-obvious scope or representation rule;
- a design note; or
- nowhere, when the check only helped navigate the implementation.

Do not retain diagnostic logging, unstable declaration-order checks, or a
standalone executable solely because they were useful during development.

## Permanent-test criteria

Keep a test when it records a durable contract, such as:

- user-visible behavior or language semantics;
- a serialization, ABI, cache, or cross-module contract;
- an internal invariant required by later compiler or runtime phases;
- a regression likely to recur during ordinary maintenance; or
- optimizer behavior whose loss would materially affect correctness or
  performance.

A permanent test should also be deterministic, reasonably fast, supported on
the required platforms, and located in a runner with a clear owner.

Rewrite debugging tests to express the contract rather than the route by
which the bug was found.  Prefer a test such as "recursive-RHS floats retain
the recursive scope they require" over checks of temporary names, incidental
bind positions, or an exact helper result that is not itself a contract.

Remove a test when it:

- only probes intermediate values or control flow;
- depends on temporary names, logging, or incidental ordering;
- duplicates broader durable coverage;
- tests a discarded implementation rather than required behavior;
- is disproportionately expensive or flaky;
- cannot be supported on a required platform; or
- is the sole justification for maintaining a new executable.

## Organization

Native test sources should live in a test tree organized by subsystem, not
beside whichever production source file motivated the test.  A possible
future layout is:

```text
tests/
  cpp/
    computation/
      core/
      runtime/
      optimization/
    util/
```

Meson definitions for these tests should live under the test tree.  Runner
granularity should normally be one executable per substantial library or
subsystem, with multiple focused test files linked into it, rather than one
executable per feature.

This layout is a design direction, not permission to introduce parallel test
infrastructure incrementally.  Create it only as an explicitly reviewed
migration that begins using it immediately and addresses existing tests where
practical.

## Runtime test migration

`runtime-ast-serialization-test` mixes unit checks of Runtime APIs with
component checks involving loaders, heaps, and multiple compiler phases.  Its
name, production-source location, and requirement for package data are not a
good permanent organization.

Apply the criteria above to each check.  Move language-visible behavior to the
standard harness and universal validity rules to production invariants.  Keep
direct checks of C++ transformations, equality, aliasing, and serialization in
a native suite; distinguish self-contained unit tests from component tests in
their source and test names.

As part of a reviewed native-test migration, place these checks under
`tests/cpp/computation/runtime/`, split by contract, and link them with Core and
optimization test sources into one `computation-tests` executable.  This gives
`libcomputation` clear ownership without adding an executable per feature.  The
current runner may preserve unique coverage until then, but should not acquire
unrelated checks or be treated as the final structure.

## Short-term policy for bali-phy

Until a centralized native-test structure is designed:

- do not create new permanent C++ test executables;
- use `tests/haskell/` and existing integration suites where possible;
- use existing subsystem runners only for tests within their stated scope;
- keep temporary C++ harnesses in disposable `jj` changes; and
- when durable low-level C++ coverage has no suitable home, raise the need for
  a centralized runner instead of adding a feature-specific executable.

This is a moratorium on ad hoc executable targets, not a claim that native
unit tests are never useful.
