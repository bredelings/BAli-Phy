# Conditional preprocessing of Haskell

BAli-Phy implements the `CPP` language extension internally. It does not invoke
the system C preprocessor and does not depend on the `cpphs` package.

Stage 1 implements conditional compilation. It recognizes `#if`, `#ifdef`,
`#ifndef`, `#elif`, `#else`, `#endif`, `#define`, `#undef`, `#error`, and
`#warning`. Object-like and non-variadic function-like macros expand inside
conditional expressions. Macro names in ordinary Haskell source are copied
without expansion, which preserves the source locations used by the parser.

Conditional arithmetic uses signed, arbitrary-precision integers. The default
limits are 200 nested conditionals, 200 nested macro expansions, 1,000,000
expanded tokens per expression, and 1,000,000 bits per integer. These fixed
budgets make resource failures deterministic across platforms. Logical
operators and the conditional operator short-circuit, so arithmetic errors in
unevaluated operands are ignored.

The first stage does not support includes, line directives, preprocessing
pragmas, variadic macros, stringification, token concatenation, character
constants in conditional expressions, or macro expansion in Haskell bodies.
These forms produce explicit diagnostics when encountered in active directives.

`--cpp` enables preprocessing for every source module. Otherwise, a module must
have a leading `{-# LANGUAGE CPP #-}` pragma. `--cpp-define` and
`--cpp-undefine` configure the macro environment but do not enable CPP. All
command-line definitions are applied in their supplied order, followed by all
command-line undefinitions, before source directives are processed.

`--dump-cpp` displays the exact generated source passed to the Haskell parser.
Removed directives and inactive source retain their newlines, so surviving
Haskell tokens keep their original line and column positions.
