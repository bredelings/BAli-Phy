# Upstream Haskell packages

The work needed to use upstream Haskell packages in BAli-Phy falls into three
partly independent tracks:

1. adding generally useful Haskell library and runtime APIs;
2. implementing or porting `optparse-applicative`; and
3. fetching and configuring packages from Hackage.

The items below are ordered approximately from easier to harder.  Items marked
as independent or optional are branches rather than strict prerequisites.
The intended progression is:

```text
small base additions
    -> useful local Options.Applicative
    -> pinned upstream optparse-applicative
    -> curated single-version Hackage packages
```

General Cabal and GHC compatibility is a separate, explicitly scoped project.

## 1. Small compatibility modules

Add nearly mechanical modules and missing functions, following upstream APIs
where possible:

- `Control.Monad.Fail`
- `Data.Functor.Identity`
- `Data.Bifunctor`
- missing `Data.List`, `Data.Maybe`, and `Data.Text` operations as encountered

These modules are independently useful and require little or no new runtime
machinery.

## 2. Exact-version Hackage fetcher (independent)

Add a command such as:

```console
bali-phy-pkg fetch-hackage PACKAGE VERSION
```

For an explicitly selected version, the fetcher can use Hackage's predictable
tarball and revised-Cabal-file endpoints without first downloading an index or
resolving dependencies.  It should record:

- package name and version;
- source URL;
- source SHA-256;
- Cabal revision and its hash; and
- license files.

This command only retrieves source.  It does not yet make the package
importable.

## 3. Safe versioned source cache (independent)

Extend the fetcher with:

- path-traversal and unsafe-link protection;
- atomic installation;
- a source manifest;
- a versioned cache rather than a merged package directory; and
- offline reuse of an already verified archive.

The existing `scripts/bali-phy-pkg.py` already contains HTTPS downloading,
hashing, temporary-file handling, manifests, and defensive archive-path checks.
Its archive layout is BAli-Phy-specific, but much of this machinery can be
reused.

## 4. `System.Environment` and controlled `System.Exit`

Add:

- `getProgName`;
- `lookupEnv`;
- `ExitCode`; and
- `exitWith`, `exitSuccess`, and `exitFailure`.

`exitWith` should propagate a distinguished runtime result to the top-level
runner rather than invoking native `exit()` from arbitrary Haskell code.  This
allows normal cleanup and makes exit behavior testable.

## 5. Port the needed `transformers` modules

Start with:

- `Control.Monad.Trans.Class`;
- `Control.Monad.Trans.Except`;
- `Control.Monad.Trans.Reader`; and
- `Data.Functor.Identity`.

Then consider `State`, `Writer`, and `Maybe`.  The upstream `transformers`
package is attractive because it is portable Haskell with few dependencies,
although its `ghc-prim` uses and compatibility conditionals still need to be
audited.

## 6. Implement the pure option-parser core

Port the upstream structures and behavior for:

- `Parser`;
- `ReadM`;
- `ParserInfo`;
- `ParserResult`;
- options, flags, arguments, defaults, alternatives, and subcommands; and
- `execParserPure`.

The parser can be tested at this point without process termination or terminal
presentation.  Compatibility tests should cover permutation parsing,
alternative selection, repeated and missing options, short-option grouping,
and `--` handling.

## 7. Add basic help and `execParser`

Initially use a simple, local, unstyled document renderer with a fixed or
explicitly configured width.  Support:

- usage lines;
- option descriptions;
- parse errors;
- `helper`; and
- `execParser`.

This is the first practical stopping point: programs can replace partial
`getArgs` matches while using the common `Options.Applicative` API.

## 8. Establish a BAli-Phy package compatibility profile

Before configuring upstream packages automatically, define:

- which `base` APIs BAli-Phy provides;
- how that environment is versioned;
- which language extensions are supported;
- how Cabal `impl(ghc ...)` conditions are handled;
- which CPP macros are provided; and
- whether package-specific compatibility overrides are permitted.

There are three main alternatives:

1. Pretend to be a selected GHC and `base` version.  This superficially
   maximizes compatibility but can select code whose assumptions BAli-Phy does
   not satisfy.
2. Introduce a distinct `bali_phy` compiler identity.  This is accurate, but
   existing packages have no conditionals for that identity.
3. Use curated per-package overrides without claiming general Cabal
   compatibility.

This is a substantial design choice and should be decided explicitly before
general Cabal interpretation.

## 9. Port `prettyprinter` and its required `text` surface

To use the current upstream help modules, add:

- the required `Data.Text` API;
- `prettyprinter`; and
- plain-text rendering.

Plain rendering should remain separate from terminal styling.  A useful
argument parser does not require ANSI output.

## 10. Port `ansi-terminal` and `prettyprinter-ansi-terminal`

This provides upstream-compatible styled output and portable terminal
behavior.  It is not required for a useful local parser, but it is part of the
current upstream presentation dependency closure.

## 11. Add `System.Process` (optional)

An initial interface could provide:

- `callProcess`;
- `readProcess`; and
- `readProcessWithExitCode`.

The full API is substantially harder because it includes asynchronous process
handles, pipe ownership, waiting and polling, termination, signals or process
groups, environment and working-directory control, deadlock-safe simultaneous
output capture, and Windows argument quoting.

Upstream `optparse-applicative` should initially be configured with its
process-dependent shell completion disabled, so `System.Process` does not block
ordinary option parsing.

## 12. Compile a pinned upstream release using a curated recipe

Combine the preceding pieces with a package-specific recipe containing:

- source directories;
- exposed modules;
- dependency versions;
- default extensions;
- CPP definitions;
- disabled optional features; and
- documented compatibility patches.

This experiment should determine how much upstream source compiles unchanged
before a general Cabal importer is designed.

## 13. Create a single-version package environment

Teach the module loader about a selected collection of package source roots and
exposed modules while retaining this simplifying invariant:

> At most one version of each package may participate in one program.

This avoids immediate changes to Haskell module identity.  Existing
compiled-module source hashes protect correctness when search paths change,
although switching environments may reduce cache reuse.

## 14. Interpret a restricted Cabal library component

Initially support only ordinary library fields:

- `hs-source-dirs`;
- `exposed-modules` and `other-modules`;
- `build-depends`;
- `default-language` and `default-extensions`;
- simple flags and conditionals;
- Cabal version ranges; and
- generation of `VERSION_*` and `MIN_VERSION_*` CPP macros.

Unsupported components and fields must be rejected explicitly rather than
silently ignored.  Component metadata affects both module discovery and
compiler options.

## 15. Obtain a dependency plan

Evaluate two approaches:

1. Ask an installed `cabal` to resolve dependencies and consume its plan.  This
   provides mature resolution, revisions, preferred versions, and security,
   but introduces an external Cabal and GHC-toolchain dependency.
2. Implement a restricted single-version resolver.  This keeps BAli-Phy
   independent but requires version-range interpretation, package flags,
   conditional dependencies, conflict reporting, and a reproducible lock
   file.

This choice should be made before implementing a new dependency solver.

## 16. Authenticated Hackage indexes and reproducible lock files

If BAli-Phy resolves packages itself:

- consume Hackage's incremental `01-index`;
- authenticate metadata using existing Hackage-security machinery or a
  trusted helper;
- respect revisions, preferred versions, and deprecations; and
- record all selected versions, revisions, flags, and hashes in a lock file.

Implementing TUF directly should not be part of this work.

## 17. Package-qualified module identity and multiple versions

Supporting two versions of the same package in one dependency graph requires
package or unit identity to participate in:

- import resolution;
- type and symbol identity;
- compiled-module cache identity;
- error messages; and
- visibility rules.

This is a compiler architecture change, not merely a package-manager feature.

## 18. Broader Cabal and GHC ecosystem compatibility

The final and open-ended obstacles include:

- `.lhs`, `.hsc`, and generated modules;
- custom setup programs and build tools;
- C and C++ sources and foreign libraries;
- Template Haskell;
- GHC-specific modules and primitives;
- rewrite rules and specialized pragmas; and
- precise `base` behavioral compatibility.

A realistic long-term boundary should stop before this generality and
advertise support for a defined portable-Haskell package profile.

