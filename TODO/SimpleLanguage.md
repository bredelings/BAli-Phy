# Simple Model Language

This document summarizes a proposed simpler model-description language for
BAli-Phy.  The goal is to explore a language that is easier to explain, easier
to typecheck, and less dependent on special command-line conveniences than the
current model language.

The current command-line language remains useful and should continue to work.
The simple language is a separate direction, not an immediate replacement.

## Motivation

The closest comparison points are probabilistic programming languages and
model-description languages, including RevBayes, PhyloSpec, TreePPL, Stan,
BUGS, and related systems.  BAli-Phy's existing command-line language has some
features in common with these systems, but also has BAli-Phy-specific
ergonomic features that can obscure the model structure.

The current command-line language has several features that make compact model
specifications ergonomic:

- defaults, including random default values;
- `get_state(...)` for contextual values such as alphabets, trees, and branch
  categories;
- argument-level `alphabet` expressions in JSON bindings;
- bare rule names as nullary calls;
- `+>` stacking and placeholder rewrites;
- rule metadata for logging, extraction, and computed values;
- implicit type conversions;
- expression-level sampling such as `~expr`;
- rule calls that look pure but perform monadic actions.

These features are useful for compact command-line models, but they make the
language harder to reason about as a generic model-description language.  The
simple language should start with explicit syntax and ordinary lexical scope.

## First-Version Scope

The simple language should:

- use familiar parenthesized function-call syntax;
- require explicit declarations for random variables;
- translate to Haskell source or Haskell AST;
- use the Haskell typechecker as the source of truth for types and constraints;
- avoid hidden contextual state, hidden defaults, and automatic command-line
  binding behavior in the core language;
- make model structure clear enough that users can read errors and generated
  code without understanding command-line-specific rewrites.

The language should be expressive enough to describe ordinary substitution
models, mixture models, and priors, but it does not need to reproduce every
ergonomic feature of the current command-line language.

The first version should not try to implement:

- implicit default arguments;
- implicit alphabet, tree, topology, or branch-category state;
- expression-level sampling such as `~dist`;
- automatic extraction metadata;
- automatic computed loggers from binding metadata;
- declaration-side function definitions such as `f(x) = ...`;
- impure lambda functions;
- user-defined operators and fixities;
- implicit currying;
- expression-level type application.

Some of these may be added later, but they should not be part of the initial
core unless there is a concrete need.

## Example

One possible simple-language model:

```text
codonA = codons(dna(), standard_code())
nucA = getNucleotides(codonA)

kappa ~ logNormal(log(2), 0.25)
omega ~ uniform(0, 1)
er ~ symmetric_dirichlet_on(letter_pairs(nucA), 1)
pi ~ symmetric_dirichlet_on(letters(nucA), 1)

nucModel = gtr(nucA, er, pi)
model = x3(codonA, nucModel) |> dNdS(omega)
```

This should lower to Haskell that is explicit about alphabets and random
values, without relying on command-line defaults or hidden alphabet state.

## User Language

### Calls And Names

Function calls use parentheses:

```text
gtr(nucA, er, pi)
x3(codonA, nucModel)
dNdS(model, omega)
```

Zero-argument calls require parentheses:

```text
dna()
```

Bare names are values, not implicit calls:

```text
gtr      # function value
gtr()    # zero-argument call, if gtr has arity zero
```

This avoids ambiguity between a variable and a nullary function call.

### Declarations

Ordinary declarations use `=`:

```text
codonA = codons(dna(), standard_code())
nucA = getNucleotides(codonA)
model = x3(codonA, nucModel)
```

Random declarations use `~`:

```text
kappa ~ logNormal(log(2), 0.25)
omega ~ uniform(0, 1)
pi ~ symmetric_dirichlet_on(letters(nucA), 1)
```

The intent is that sampled quantities are named.  This gives natural logger
names and avoids anonymous random subexpressions hidden inside larger terms.

Expression-level sampling should not be included initially:

```text
model = hky85(nucA, ~logNormal(log(2), 0.25), pi)  # not first-version syntax
```

Users should instead write:

```text
kappa ~ logNormal(log(2), 0.25)
model = hky85(nucA, kappa, pi)
```

### Patterns

The left-hand side of `=` and `~` is a pattern.  The first version may only
need variable patterns and tuple patterns:

```text
x = expr
(x, y) = pair_expr
theta ~ distribution
```

Tuple patterns are useful when a function returns multiple related values.
The same small pattern language can be used for lambda arguments:

```text
|(x, y)| x + y
```

More complex patterns can be added later if concrete examples need them.

### Random Variables And Logging

The simple language should use named random declarations as the natural logging
unit:

```text
omega ~ uniform(0, 1)
```

The first automatic logging rule is simple: log every loggable non-function
name bound by a top-level `=` or `~` declaration.  Tuple patterns introduce
multiple logged names:

```text
x ~ Normal(0, 1)
y ~ Normal(x, 1)
(z, q) = someFunction(y)
```

This logs `x`, `y`, `z`, and `q`.

Top-level declarations that bind function values, such as lambda-based helper
functions, should not produce function-valued log entries.

This avoids needing binding metadata to decide which anonymous subterms should
be logged, but it is intentionally limited.  Names introduced inside functions
or local scopes are not automatically logged.  For example, if repeated model
fragments are factored into a pure function that computes local values `u` and
`v`, those local names do not become log entries.

If the same fragment were expanded at the top level several times, each
expanded copy would need distinct top-level names such as `u1`, `v1`, `u2`,
and `v2`, and those names would be logged.  The simple language does not try to
generalize automatic logging so that function-local names behave like expanded
top-level declarations.  This is one of the ways it stays simpler than the
current command-line language.

The current command-line binding system can also specify extra computed values
to log and can mark some values as too uninteresting to log.  The simple
language should not reproduce that automatic `computed` / `no_log` machinery in
the first version.

Later, explicit logger declarations or attributes may be useful.  The first
version should not reproduce the current automatic logging and extraction
machinery.

### Pipes

The first pipe design is object-first:

```text
E |> f(a, b)
```

means:

```text
f(E, a, b)
```

For example:

```text
model = x3(codonA, nucModel) |> dNdS(omega)
```

means:

```text
model = dNdS(x3(codonA, nucModel), omega)
```

This follows conventions such as R-style pipes more than Haskell's
argument-last function application style.  The advantage is that the syntax is
easy to explain with parenthesized function calls.

Placeholder pipes may be considered later, but they add extra design questions
about placeholder scope, ordering, and nested calls.

### Lambdas And Function Values

The simple language should not use implicit currying in the first version.

If a function has arity three, then supplying two arguments is an arity error:

```text
gtr(er, pi)  # error if gtr requires an alphabet, er, and pi
```

This is intentionally different from Haskell.  It prevents missing arguments
from silently becoming function values.

Function values are still available by using bare names:

```text
gtr
```

Partial application should be explicit, using lambdas:

```text
|a| gtr(a, er, pi)
```

The exact lambda syntax is still open.  Rust-style syntax such as `|a| expr`
is attractive for users familiar with C-like languages.  Haskell-style
`\a -> expr` would be closer to the compilation target.  Either way, explicit
lambdas are clearer than implicit currying for this language.

Lambda binders should use the same first-version pattern syntax as declaration
left-hand sides: probably variable patterns and tuple patterns only.

### Effects

The simple language must distinguish pure expressions from expressions that
perform random sampling.

In the first version, random sampling is introduced only by top-level random
declarations:

```text
omega ~ uniform(0, 1)
```

Lambda values are supported only as pure functions.  A lambda body should not
contain `~`, and evaluating a lambda should not perform sampling.  A lambda may
be used directly or bound by an ordinary declaration; this is not a separate
function-declaration syntax.

For example, this is a pure function:

```text
scale = |x| x * 2
```

The type distinction should be visible.  A pure function might have a type like:

```text
Double -> Int -> Double
```

A function that samples should have a type more like:

```text
Double -> Int -> Random<Double>
```

The exact spelling of `Random<Double>` is not fixed.  The important point is
that a sampling function should not be confused with a pure function returning
the same result type.

Impure lambda functions may be reconsidered later, but they should not be part
of the initial language.

### Explicit Context Values

The simple language should initially require alphabets to be explicit:

```text
codonA = codons(dna(), standard_code())
nucA = getNucleotides(codonA)

er ~ symmetric_dirichlet_on(letter_pairs(nucA), 1)
pi ~ symmetric_dirichlet_on(letters(nucA), 1)

nucModel = gtr(nucA, er, pi)
model = x3(codonA, nucModel)
```

This avoids the current command-line language's hidden alphabet state.

The same principle applies to other contextual values in the current language,
such as trees, topologies, and branch categories.  The first version should pass
these values explicitly rather than accessing them through hidden state.

Builder functions that take an alphabet may be useful later.  For example, a
model builder could be written explicitly as:

```text
nucBuilder = |a| gtr(a, er, pi)
```

However, the first version should not make omitted alphabet arguments create
builder functions implicitly.

## Types

### Haskell As The Type System

The simple language should use Haskell as the semantic type system.  Binding
types may use a friendlier surface syntax, but they should map to Haskell
types before checking.

Constraints should be handled by the Haskell typechecker, not by reimplementing
a Haskell-like constraint solver in `src/models`.

### Type Syntax

Type-constructor application may use angle brackets:

```text
Distribution<Double>
CTMC<a>
DiscreteDist<CTMC<a>>
List<Double>
```

These map to Haskell types such as:

```haskell
Distribution Double
CTMC a
DiscreteDist (CTMC a)
[Double]
```

This is type syntax only.  The first version does not need expression-level
type application such as `f<T>(x)`, especially because the current Haskell
compilation pipeline does not yet support explicit type application.

### Constraint Behavior

For example, if `+` has type:

```haskell
Num a => a -> a -> a
```

then:

```text
hky85(nucA, kappa1, pi1) + hky85(nucA, kappa2, pi2)
```

should produce a wanted constraint resembling:

```haskell
Num (CTMC a)
```

Since there is no such instance, typechecking should fail.

This is the desired behavior.  The simple language should rely on the same
instance and constraint machinery that checks generated Haskell code.

## Bindings

### Binding Files

The simple language should use new binding files, or a clearly separate
section of binding files, rather than reusing the current command-line JSON
bindings wholesale.

The current bindings encode command-line-language policy:

- default values;
- alphabet propagation;
- automatic logging and extraction;
- computed values;
- compatibility with `+>` and `@submodel`;
- implicit conversions.

The simple binding file should start as a smaller catalog of explicit
functions.

A simple binding should describe:

- the model-language name;
- the ordered argument names;
- the call expression;
- any Haskell modules needed by generated code;
- optional explicit types.

For example:

```json
{
  "name": "hky85",
  "args": ["alphabet", "kappa", "pi"],
  "call": "SModel.hky85'(alphabet, kappa, pi)",
  "imports": ["SModel", "Bio.Alphabet"]
}
```

The `call` expression is written in the simple model language, not raw
Haskell.  It is translated to Haskell before typechecking.

### Binding Call Expressions

Binding `call` expressions should be parsed by the simple-language parser.
They are then translated to Haskell expressions.

This keeps binding syntax aligned with user syntax:

```json
{
  "name": "dNdS",
  "args": ["model", "omega"],
  "call": "model |> SModel.dNdS(omega)"
}
```

Qualified names such as `SModel.dNdS` and `Bio.Alphabet.letters` are allowed
as external Haskell references.  The first version should require qualified
external names in binding calls instead of supporting a full import mechanism
for unqualified Haskell values.

An `imports` field may still be useful for generated Haskell code, but it
should not initially create unqualified names in the simple-language binding
expression.

This avoids ambiguity between binding arguments and imported Haskell names.

### Signature Modes

Every simple binding should be in exactly one signature mode.

#### Infer Mode

If result type, argument types, and constraints are all absent, infer the whole
type from the translated Haskell call expression.

Example:

```json
{
  "name": "plus",
  "args": ["x", "y"],
  "call": "x + y"
}
```

This should infer a constrained type such as:

```haskell
Num a => a -> a -> a
```

#### Check Mode

If the result type and every argument type are present, check the translated
Haskell call expression against the declared type.

Constraints may be present or absent in check mode.

Example:

```json
{
  "name": "normal",
  "args": [
    {"name": "mean", "type": "Double"},
    {"name": "sd", "type": "Double"}
  ],
  "result_type": "Distribution<Double>",
  "call": "Probability.normal(mean, sd)"
}
```

With constraints:

```json
{
  "name": "map",
  "args": [
    {"name": "f", "type": "a -> b"},
    {"name": "xs", "type": "List<a>"}
  ],
  "result_type": "List<b>",
  "constraints": [],
  "call": "Data.List.map(f, xs)"
}
```

#### Mixed Mode Is An Error

Any partial signature is an error:

- some argument types present but not all;
- result type present without all argument types;
- argument types present without result type;
- constraints present without a complete explicit signature.

This avoids partial type signatures in the binding schema.

### Typechecking Bindings

Bindings should be checked by generating synthetic Haskell definitions and
using the Haskell typechecker.

For example, this binding:

```json
{
  "name": "hky85",
  "args": ["alphabet", "kappa", "pi"],
  "call": "SModel.hky85'(alphabet, kappa, pi)"
}
```

can be checked or inferred as if it produced:

```haskell
__binding_hky85 alphabet kappa pi =
  SModel.hky85' alphabet kappa pi
```

If an explicit signature is present, the generated declaration can include it:

```haskell
__binding_hky85 :: Nucleotides a => a -> Double -> [(String, Double)] -> CTMC a
__binding_hky85 alphabet kappa pi =
  SModel.hky85' alphabet kappa pi
```

The result should be a generalized Haskell type, including residual class
constraints.

## Deferred Features

### Named Arguments

The first version should not need named arguments in ordinary user calls.
Binding files define a fixed ordered argument list, and calls supply arguments
positionally:

```text
hky85(nucA, kappa, pi)
```

Named arguments may be useful later for readability or long argument lists:

```text
hky85(alphabet = nucA, kappa = kappa, pi = pi)
```

If named arguments are added, they should be ordinary call syntax rather than a
mechanism for defaults or hidden dependency tracking.  The first version should
avoid the current command-line parser's positional-to-named rewrite machinery.

### Local Declarations And Where Clauses

The current command-line language supports local `where { ... }` declarations.
The simple language does not need local declarations in the first version.

If local declarations are added later, they should use the same declaration
forms as the top level:

```text
expr where {
  x = pure_expr
  y ~ distribution
}
```

or an equivalent block syntax.  The design should be careful about whether
random declarations are allowed in local scopes, especially inside future
effectful functions.  For now, top-level declarations are enough.

### Explicit Conversions

The current command-line language inserts several implicit conversions when a
term is close to the required type.  The simple language should not initially
perform these command-line-specific rewrites.

Users should write explicit conversion functions where needed:

```text
unit_mixture(gtr(nucA, er, pi))
```

rather than relying on the compiler to insert `unit_mixture`, `discrete`,
`convertDiscrete`, or related conversion functions.

This does not rule out ordinary Haskell overloading or typeclass resolution.
The point is to avoid model-language-specific AST rewrites that hide which
model object is being constructed.

A future version could support explicit conversion through a Haskell typeclass
such as `Convert` or `Cast`.  The simple-language expression could be:

```text
convert(x)
```

or:

```text
cast(x)
```

The destination type should usually be inferred from local context, as in
Haskell, rather than written explicitly.  A language such as PhyloSpec can
write an explicit destination type like:

```text
cast<Double>(x)
```

but the current BAli-Phy Haskell compilation pipeline does not yet support
expression-level type arguments, and Haskell-style inference should often make
the destination type unnecessary.

If this feature is added, the available conversions should come from visible
Haskell instances, not from a hard-coded model-language rewrite table.  The
typechecker must therefore know which `Convert` or `Cast` instances are in
scope, and failed conversions should report missing instances rather than
silently trying ad hoc rewrites.

### Function Declarations

Separate function declaration syntax, such as `f(x) = ...`, is not part of the
first version.  Pure helper functions can still be represented as lambda values.

If function declarations are added later, functions that contain `~` will need
an explicit effect annotation or other syntax that marks them as random
procedures.

Pure functions and sampling functions should have visibly different types.
For example:

```text
Double -> Int -> Double
Double -> Int -> Random<Double>
```

### Operators And Fixities

The first version may include a fixed set of built-in operators:

- arithmetic operators;
- comparison operators;
- boolean operators;
- the pipe operator `|>`.

Longer term, it would be useful to use Haskell-like operator definitions and
fixity declarations.  This should be designed later.

To keep that option open, expression parsing should ideally avoid baking all
operator precedence decisions too deeply into the AST.  A future design might
parse infix expressions as operator spines and resolve fixities with an
operator table, similar in spirit to Haskell.

## Parser And Translation Strategy

The simple language should have a separate parser entry point from the current
command-line language.

It may share lexer machinery, AST nodes, or utility code where this is clean,
but the two languages have different semantics:

- the simple language uses `|>` instead of command-line `+>`;
- the simple language does not have expression-level `~`;
- the simple language does not treat bare rule names as nullary calls;
- the simple language requires complete calls;
- the simple language does not use hidden state for alphabets, trees, or other
  contextual values;
- the simple language does not perform command-line-specific implicit
  conversions.

A parser flag inside the existing grammar could work as a prototype, but it
risks mode-specific conditionals and accidental feature leakage.  A separate
entry point makes the boundary clearer.

The simple language should translate to Haskell source or Haskell AST before
typechecking.

The translator should handle at least:

- variables;
- literals;
- function calls;
- qualified external names;
- declarations;
- random declarations;
- object-first pipes;
- pure lambdas, once lambda syntax is chosen;
- variable and tuple patterns in declarations and pure lambdas;
- lists and tuples, if included in the first grammar.

The translation should preserve source locations where possible so that type
errors can point back to the simple-language input rather than only to
generated Haskell.

## Initial Implementation Sketch

An initial implementation could proceed in this order:

1. Define a small simple-language AST and parser for declarations, calls,
   literals, names, pipes, and random declarations.
2. Define a minimal simple binding schema.
3. Add a tiny binding set for a few examples: alphabets, distributions, `gtr`,
   `hky85`, `x3`, and `dNdS`.
4. Translate binding `call` expressions to Haskell expressions.
5. Generate synthetic Haskell declarations for bindings and infer or check
   their types.
6. Translate user simple-language declarations to Haskell.
7. Typecheck the generated Haskell module and report errors through the simple
   language locations where possible.
8. Add tests for successful models and deliberate type errors, including
   adding two substitution models with `+`.

## Open Questions

### Syntax

- exact lambda syntax;
- exact syntax for type annotations in user model files;
- whether patterns beyond variables and tuples are needed;
- whether simple-language names may contain Haskell-style primes;
- whether angle-bracket type application should remain only in type syntax;
- when, and whether, to support user-defined operators and fixities.

### Effects And Logging

- exact syntax for effect annotations if user-defined functions are added;
- whether local random declarations should ever be allowed;
- how explicit logger declarations or attributes should look if automatic
  top-level logging is not enough.

### Bindings And Types

- whether simple binding files are separate files or separate sections inside
  existing JSON files;
- how to represent module imports for generated Haskell code;
- how much of the current generated-code logger machinery should be reused;
- whether and when to support named arguments.

### Implementation And Diagnostics

- exact syntax and allowed scope for local declarations or `where` clauses;
- how to preserve source locations through generated Haskell typechecking;
- how much parser, lexer, and AST infrastructure should be shared with the
  existing command-line language.
