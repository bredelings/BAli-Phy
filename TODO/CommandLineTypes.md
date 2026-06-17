# Command-Line Model Types

Goal: replace command-line model type terms represented as `ptree` with a native
`CM::Type` AST, while preserving current behavior.  Haskell type inference is
out of scope for this plan; see `CommandLineHaskellTypes.md` for that later
work.

## Migration Rules

- Every implementation batch after introducing `CM::Type` must either convert an
  existing production API from `ptree` to `CM::Type` or delete obsolete `ptree`
  type code.
- Do not add converter-only tests.  Tests should exercise production APIs that
  already use `CM::Type`.
- Temporary `ptree` conversion helpers are allowed only at parser/JSON
  boundaries.  Mark them with brief comments explaining why they exist and when
  they can be removed.
- Do not keep parallel helper APIs such as `get_type_apps_ptree()` beside
  native helpers.  Convert the real API in place.
- Remove old infrastructure as soon as the production path no longer needs it.

## Phase 1: Native Type AST

Add a native type representation, probably in `src/models/model-type.H`.

Core representation:

- `TypeCon{name}` for constructors like `Int`, `Double`, `Distribution`,
  `Tuple`, and `Function`.
- `TypeVar{name, id}` or equivalent for variables like `a` and `a#0`.
- `TypeApp{head, arg}` for type application.

Do not add Haskell-specific features yet.

Required helpers:

- `type_con("Int")`
- fresh type-variable construction
- `type_app(head, arg)`
- `type_apps(head, args)`
- `get_type_apps(type)`
- `get_type_head(type)`
- `list_type(t)`
- `tuple_type({a,b,...})`
- `function_type(a,b)`

Add stable equality and ordering because current code stores types in `set`,
`map`, and unification records.

Initial tests should cover construction/decomposition, equality/ordering, type
variable recognition, nested list/tuple types, and function types.

## Phase 2: Temporary Boundary Conversion

Add temporary compatibility helpers:

```c++
CM::Type type_from_ptree(const ptree&);
ptree ptree_from_type(const CM::Type&);
```

These should be used only where external syntax still enters as `ptree`, such
as the existing parser type rules and JSON binding loading.  They should not
become the main production path.

Also add:

```c++
std::string unparse_type(const CM::Type&);
```

Tests should validate existing spellings such as `Int`, `a`, `a#0`,
`List<Int>`, `(Int,Bool)`, `Distribution<Double>`, and
`Function<Int,Double>`.

## Phase 3: Port Unification

Port `src/models/unification.H/cc` from `ptree` to `CM::Type` before changing
`Rule` or `Ann` fields.  This avoids a long period where production code stores
native types but repeatedly converts back to `ptree` for unification.

Replace:

- `typedef ptree term_t`
- `is_type_variable(const ptree&)`
- `find_variables_in_type(const ptree&)`
- `make_type_app(...)`
- `make_type_apps(...)`
- `get_type_apps(...)`
- `get_type_head(...)`

with native type operations using the same public API names where possible.

Preserve current behavior:

- occurs check
- alpha renaming
- fresh variable naming in diagnostics where visible
- constraint storage
- substitution behavior

Initially, constraints may remain type-shaped terms:

```c++
using Constraint = CM::Type;
```

Do not design Haskell class constraint semantics in this phase.

## Phase 4: Switch Type Aliases

After unification is native, switch the public aliases:

- `type_t` in `rules.H`
- `term_t` in `unification.H`

to use `CM::Type`.

This should deliberately expose compile errors in production code that still
expects `ptree` type terms.  Fix those call sites by moving them to native type
helpers, not by adding more bridges.

## Phase 5: Convert Rules and Binding Loading

Convert these fields to native types:

- `Rule::result_type`
- `RuleArg::type`
- `Rule::constraints`

JSON syntax remains unchanged.  `rules.cc` should parse JSON type strings into
`CM::Type` at load time.  Once loaded, rules should not carry `ptree` types
internally.

Tests should load representative binding JSON and verify rule result types,
argument types, and constraints structurally.

## Phase 6: Convert Expression Annotations

Change:

```c++
CM::Ann::type
```

from `ptree` to `CM::Type`.

Update:

- `model-expr.H`
- `model-expr-test.cc`
- annotated unparsing in `parse.cc`
- `compile.cc`
- `code-generation.cc`
- `typecheck.cc`

Use native helper constructors in tests instead of `ptree("Int")` and related
ad hoc type construction.

## Phase 7: Port Typechecking

Update `TypecheckingState` and model typechecking APIs to use native types:

- `typecheck_model_expr`
- `typecheck_model_decls`
- `parse_pattern`
- `unify_or_convert_model_expr`
- `convertible_to`
- `type_for_var`
- `type_for_arg`
- state variable types

Replace direct `ptree("Int")`, `ptree("Tuple")`, etc. with native type
constructors.

Preserve existing behavior first:

- int-to-double conversion
- discrete-to-distribution conversion
- `CTMC`, `ExchangeModel`, and `MultiMixtureModel` conversions
- tuple/list/function unification
- current diagnostics where practical

## Phase 8: Port Code Generation and Compile Helpers

Update type inspections and diagnostics in:

- `compile.cc`
- `compile.H`
- `code-generation.cc`
- `A-T-prog.cc` if needed

Examples:

- `is_loggable_type`
- `generated_code_t::log_value`
- pretty/extraction code
- `unparse_type(...)` diagnostics
- any code decomposing type applications

After this phase, production model code should no longer need `ptree` for model
types except at parser/JSON boundaries.

## Phase 9: Convert Parser Type Rules

Once native type APIs are stable, update `parser.y` so type grammar rules return
`CM::Type` directly instead of `ptree`.

Regenerate:

- `parser.cc`
- `parser.hh`

Update:

- `driver.cc`
- `parse.H`
- `parse_type(...)`

After this, remove parser-side `ptree -> Type` conversion if no other boundary
needs it.

## Phase 10: Remove Compatibility Code

Delete temporary type conversion helpers once production code no longer needs
them.

Audit with searches such as:

```sh
rg 'ptree\("Int"|ptree\("Double"|ptree\("Tuple"' src/models
rg 'make_type_app|get_type_apps|get_type_head' src/models
rg 'unparse_type\(const ptree|parse_type.*ptree' src/models
rg 'type_from_ptree|ptree_from_type' src/models
```

Remaining `ptree` uses should be unrelated to model types.

## Validation

Run focused checks after each implementation batch:

```sh
meson test -C ../build/gcc-16-debug-O "bali-phy:model expression AST" --print-errorlogs
meson test -C ../build/gcc-16-debug-O "bali-phy:runtime AST serialization" --print-errorlogs
meson test -C ../build/gcc-16-debug-O "bali-phy:bali-phy 5d +A 50" --print-errorlogs
```

When parser or rule loading changes, also run relevant parse tests through
Meson if available and at least one command-line model smoke test that loads
the binding database.
