# Use Haskell Types For Command-Line Bindings

Goal: use Haskell type inference to audit command-line binding signatures first,
then allow redundant JSON type annotations to be omitted for bindings where
inference is stable.  Keep explicit JSON signatures as a permanent override.
Do not replace `models/typecheck.cc` or `models/unification.*` until inferred
binding signatures are proven useful.

## Current Status

Initial infrastructure and one end-to-end inferred binding are in place:

- Binding JSON signature modes are validated before type conversion.
- Mixed signatures are rejected.
- Haskell binding import contexts are compiled as synthetic import-only modules:
  `module_loader -> synthetic import modules -> one Program -> CompiledModule contexts`.
- Rule-template lowering is shared with code generation and records referenced
  `@arg` names.
- A semantic value-signature lookup API returns compiled `Type`, `TypeVar`, and
  constraint data.
- Focused tests check name-resolution parity for representative template heads.
- Rule-call inference lowers a binding call template into a synthetic Haskell
  function declaration, runs the existing `TypeChecker` declaration inference
  path in a compiled import context, and reads generalized semantic `Type`
  values from `poly_env()`.
- A narrow semantic Haskell `Type` to `CM::Type` bridge supports variables,
  `Int`, `Double`, `Bool`, lists, tuples, and function arrows.
- Loader-aware `Rules(package_paths, module_loader)` can resolve inferred
  signatures before the existing model typechecker sees a rule.
- `bindings/functions/take.json` now omits redundant signature fields and gets
  `Int -> List<a> -> List<a>` from Haskell inference.

The compatibility constructor `Rules(package_paths)` remains explicit-only for
tests and simple callers that do not have a Haskell module loader.  Class
constraints, defaults, alphabets, and broad annotation removal still require
follow-up work.

## Signature Modes

Every binding JSON file has exactly one signature mode:

- Explicit mode: `result_type` is present and every arg has `type`.
  `constraints` may be present or absent.
- Inferred mode: `result_type` is absent, every arg omits `type`, and
  `constraints` is absent.
- Mixed mode: any partial combination is an error.

In inferred mode, `args` still defines command-line argument names, order,
defaults, alphabets, docs, and other metadata.  Only the signature is inferred.

## Milestone 1: Validate Signature Modes

Add rule-loader validation before converting rule types:

- Accept explicit mode.
- Accept inferred mode only behind a developer feature flag at first.
- Reject mixed mode with a rule-qualified error message.
- Treat `constraints` as optional only in explicit mode.

This prevents accidental partial inference and keeps the migration boundary
clear.

## Milestone 2: Compiled Value Signature Lookup

Expose a read-only API for looking up value signatures from compiled Haskell
modules.

Deliverables:

- Query value types by resolved or imported name.
- Support `Prelude` as the default context when no binding imports are
  specified.
- Support modules listed in binding `"import"`.
- Return zonked/generalized Haskell `Type` values and quantified constraints
  from compiled signatures.

Keep this separate from expression inference and instance lookup.

## Milestone 3: Shared Rule Template Lowering

Extract rule-template lowering from code generation into a shared helper.

Current code path: `src/models/code-generation.cc`,
`make_rule_template_expr(...)`.

The shared helper must preserve:

- `@arg` references.
- Qualified names like `SModel.mixture`.
- Operators like `@x + @y`.
- List and tuple constructors.
- Rule-template lambda support.
- `sample(...)` compatibility behavior.
- Current final `@submodel` / `+>` compatibility rewrite.
- Useful rule-name and arg-name error context.

Code generation and inference should use the same lowering semantics so they
cannot silently disagree.

## Milestone 4: Rule Call Inference

Add inference for one binding `call` template in a compiled-module context.

For each rule:

1. Read binding imports, defaulting to `Prelude`.
2. Create fresh local Haskell variables for each JSON arg name.
3. Lower the `call` expression using the shared rule-template lowering helper.
4. Create a synthetic Haskell function declaration such as
   `infer_rule arg_x = length arg_x`.
5. Use `TypeChecker::infer_type_for_decls_group(...)` to infer and generalize
   that declaration in the compiled import context.
6. Read the generalized semantic `Type` from `poly_env()`.
7. Peel `forall` binders and constraints, then split function arrows into
   result and arg types.

Do not store live `MetaTypeVar`s or solver evidence in `Rule` objects.

Initial inferred mode should require every declared arg to occur in `call`.
If an arg is absent, fail inference with a clear message requiring explicit
JSON annotations.  Do not infer from `default_value` or `alphabet` expressions
in this milestone.

Prototype bindings should be simple:

- `take`: `Int -> [a] -> [a]`
- `replicate`: `Int -> a -> [a]`
- `zip`: `[a] -> [b] -> [(a,b)]`
- later, after constraint bridging: `_add`, `_eq`, `length`

Keep bindings with defaults, alphabets, or domain-specific latent constraints
explicit until the audit data says they are safe.

`length` currently remains explicit because its Haskell type is inferred as
`Foldable t => t a -> Int`, while the command-line rule intentionally exposes
the narrower model type `List<a> -> Int`.

## Milestone 5: Name Resolution Parity Tests

Before comparing annotations, verify inference and generated Haskell resolve
names the same way.

Add tests for:

- Unqualified Prelude names.
- Qualified imported names.
- Unqualified names from binding imports.
- Symbolic operators.
- Constructors.
- Shadowing or ambiguity cases, if supported.

Inferred types are only trustworthy if inference and code generation refer to
the same Haskell symbol.

## Milestone 6: Haskell Type To `CM::Type` Bridge

Add a compatibility bridge from inferred Haskell `Type` to the existing
`CM::Type` model type representation.

Keep it intentionally narrow:

- Type variables.
- `Int`, `Double`, `Bool`, `String` / `Text` as currently modeled.
- Lists and tuples.
- Function arrows.
- Common model constructors: `Distribution`, `DiscreteDist`, `CTMC`,
  `ExchangeModel`, `MultiMixtureModel`, `Tree`, `Topology`, alphabet/model
  types used in bindings.
- Simple class constraints represented as current model constraints.

If an inferred type cannot round-trip through this bridge, require explicit
JSON annotations for that binding.

Do not try to bridge arbitrary type families, higher-rank types, complex
qualified constraints, or kind-polymorphic signatures in this phase.

## Milestone 7: Annotation Comparison Audit Mode

Keep all existing JSON annotations required.

Add a developer/test-only audit mode:

1. Infer each binding signature when possible.
2. Compare inferred result type to JSON `result_type`.
3. Compare inferred arg types to JSON arg `type`.
4. Compare inferred constraints to JSON `constraints`.
5. Report mismatches with rule name, arg name, declared type, inferred type,
   inferred constraints, declared constraints, and imports.

Comparison must normalize:

- Alpha-renamed type variables.
- Qualified vs. unqualified known type constructors.
- Haskell list/tuple spelling vs. model spelling.
- Constraint ordering.
- Solver-simplified residual predicates.

No normal user-facing behavior should change in this milestone.

## Milestone 8: Whole-Tree Signature Report

Run audit mode across all `bindings/*.json`.

Produce a report grouping bindings into:

- Exact match.
- Match after normalization.
- Inferred extra constraints.
- Declared extra constraints.
- Ambiguous inference.
- Argument absent from `call`.
- Unsupported inferred type shape.
- Haskell-to-`CM::Type` bridge failure.
- Name-resolution problem.
- Call-lowering unsupported syntax.

This report is the migration map.  Do not remove annotations before it exists.

## Milestone 9: Optional Annotation Support

Enable inferred mode for bindings that pass all checks:

- The binding is in inferred mode, not mixed mode.
- Every arg occurs in `call`.
- Inference succeeds.
- Inferred result and arg types bridge to `CM::Type`.
- Inferred constraints bridge to current model constraints.
- No ambiguous Haskell type remains after simplification/generalization.
- The template uses only supported lowering syntax.

Populate `Rule::result_type`, `RuleArg::type`, and `Rule::constraints` from
the inferred and bridged signature before `models/typecheck.cc` sees the rule.

Defaults and alphabets are still parsed and typechecked after the rule
signature is resolved, as they are today.

## Milestone 10: Remove Simple Annotations

Remove annotations only for bindings classified as exact match or
match-after-normalization.

Suggested order:

1. Simple Prelude-like functions: `_add`, `_sub`, `_mul`, `_eq`, `_lt`, `min`,
   `max`.
2. Constructors: `Nil`, `Cons`.
3. Simple utility functions: `length`, `replicate`, `take`, `zip`, `zipWith`.
4. Simple distributions: `normal`, `gamma`, `uniform`, `iid`.
5. More complex model bindings only after defaults and alphabet expressions are
   confirmed stable.

Keep explicit annotations for ambiguous bindings, bindings with args absent
from `call`, and bindings whose command-line model interface intentionally
differs from the raw Haskell function type.

## Milestone 11: Instance Head Export

After optional annotations work, expose instance heads from compiled Haskell
modules for diagnostics.

Deliverables:

- Query available instance signatures/heads by class.
- Include imported/transitive module instances as Haskell typechecking would.
- Do not expose instance bodies.
- Add tests for `Num`, `Eq`, `Ord`, and domain classes such as `Nucleotides`,
  `Triplets`, and `Doublets`.

This enables better diagnostics but is not required for initial annotation
removal.

## Milestone 12: Limited Instance-Aware Diagnostics

Add early model-layer failures for concrete constraints only.

Start with:

- Accepted: `Num Double`, `Num Int`, `Eq Int`, `Ord Double`.
- Rejected: `Num String`, `Ord (Distribution a)` when concrete enough.
- Domain checks where instances are concrete enough: `Nucleotides DNA`,
  `Triplets ...`, `Doublets ...`.

Leave polymorphic constraints unresolved and allow later Haskell compilation or
the existing generated-code path to handle remaining cases.

## Milestone 13: Preserve Model-Language Coercions

Keep model-language conversion insertion separate from Haskell signature
inference.

The current typechecker mutates the model AST by inserting calls such as:

- `intToDouble`
- `discrete`
- `convertDiscrete`
- `unit_mixture`
- `multiMixtureModel`
- `f`

These are command-line language conveniences, not ordinary Haskell unification.
Do not remove `convertible_to()` until an equivalent model coercion layer exists
for Haskell-backed types.

## Milestone 14: Revisit Type Representation

Only after audit mode and optional annotations are working, decide whether to
replace `CM::Type` with Haskell `Type` in the model typechecker.

Use evidence from the audit:

- How many inferred signatures cannot bridge to `CM::Type`?
- How often do constraints require real Haskell solving?
- How much duplicate type logic remains?
- Are model conversions still clearer in the current model type system?

Possible outcomes:

- Keep `CM::Type` and bridge inferred signatures.
- Use Haskell `Type` only inside `Rule` signatures and constraints.
- Gradually migrate `models/typecheck.cc` annotations to Haskell `Type`.
- Replace `models/unification.*` with a narrow facade over the Haskell
  typechecker.
- Fully retire `models/unification.*` later.

## Later: Infer From Defaults And Alphabets

After call-only inference is stable, consider richer inference:

- Use `default_value` expressions to constrain otherwise absent args.
- Use `alphabet` expressions to constrain alphabet-dependent args.
- Infer with partial signatures where the call leaves some variables
  underconstrained.
- Detect when explicit signatures are still required.

This is optional.  Explicit JSON signatures remain the escape hatch.

## Testing Strategy

Add tests incrementally:

- Signature mode validation tests.
- Value signature lookup tests.
- Rule-template lowering equivalence tests.
- Binding inference tests.
- Name resolution parity tests.
- Haskell-to-`CM::Type` bridge tests.
- Annotation comparison tests.
- Whole-tree audit test or script.
- Optional annotation regression tests.
- Later instance-head lookup tests.
- Later limited instance diagnostics tests.

## Implementation Principle

Haskell inference should first be an internal checker for the binding database.
Only after it agrees with current annotations should it become the source of
missing annotations.  Constraint solving, model typechecker replacement, and
full Haskell `Type` migration are follow-up migrations, not prerequisites.
