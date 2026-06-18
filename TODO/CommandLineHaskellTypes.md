# Command-Line Binding Types

Goal: make Haskell inference an audit oracle for command-line bindings first,
then use it to remove redundant binding annotations incrementally.  Do not
rewrite `models/typecheck.cc` or solve constraints broadly until the audit path
is proven.

## Milestone 1: Compiled Value Signature Lookup

Expose a small read-only API for looking up value signatures from compiled
Haskell modules.

Deliverables:

- Query value types by resolved or imported name.
- Support `Prelude` as the default context when no binding imports are
  specified.
- Support modules listed in binding `"import"`.
- Return Haskell `Type` and any quantified constraints already present in the
  compiled signature.

Keep this separate from expression inference and instance lookup.

## Milestone 2: Shared Binding Call Lowering

Extract binding `call` lowering from code generation into a shared helper.

Current code path: `src/models/code-generation.cc`, `make_call(...)`.

The shared helper must preserve:

- `@arg` placeholder handling.
- Qualified names like `SModel.mixture`.
- Operators like `@x + @y`.
- List and tuple constructors.
- Current `@submodel` / `+>` special case.
- Useful rule-name and arg-name error context.

Both code generation and inference should use this same lowering path so they
cannot silently disagree.

## Milestone 3: Binding Call Inference

Add inference for a binding call in a compiled-module context.

For each rule:

1. Read binding imports, defaulting to `Prelude`.
2. Create fresh local Haskell variables for each JSON arg name.
3. Lower the `call` expression using the shared lowering helper.
4. Infer the expression's result type.
5. Extract inferred types for each `@arg`.
6. Preserve inferred constraints as constraints, not solved facts.

Prototype bindings:

- `_add`: `Num a => a -> a -> a`
- `_eq`: `Eq a => a -> a -> Bool`
- `pdf`: `Distribution a -> a -> Double`
- `iid`: `Int -> Distribution a -> Distribution [a]`
- `hky85`: imported symbol lookup plus model-specific constraints

## Milestone 4: Name Resolution Tests

Before comparing annotations, verify inference and generated Haskell resolve
names the same way.

Add tests for:

- Unqualified Prelude names.
- Qualified imported names.
- Unqualified names from binding imports.
- Symbolic operators.
- Constructors.
- Shadowing or ambiguity cases, if supported.

This is critical: inferred types are only trustworthy if the inferred symbol is
the same symbol code generation will emit.

## Milestone 5: Annotation Comparison Audit Mode

Keep all existing JSON annotations required.

Add a developer/test-only audit mode:

1. Infer each binding signature when possible.
2. Compare inferred result type to JSON `result_type`.
3. Compare inferred arg types to JSON arg `type`.
4. Compare inferred constraints to JSON `constraints`.
5. Report mismatches with rule name, arg name, declared type, inferred type,
   and imports.

Comparison must normalize:

- Alpha-renamed type variables.
- Qualified vs. unqualified known type constructors.
- Syntactic list and tuple spelling differences.
- Constraint ordering.

No normal user-facing behavior should change in this milestone.

## Milestone 6: Whole-Tree Signature Report

Run audit mode across all `bindings/*.json`.

Produce a report grouping bindings into:

- Exact match.
- Match after normalization.
- Inferred extra constraints.
- Declared extra constraints.
- Ambiguous inference.
- Unsupported inferred type shape.
- Name-resolution problem.
- Call-lowering unsupported syntax.

This report becomes the migration map.  Do not remove annotations before this
exists.

## Milestone 7: Narrow Haskell-Type To Model-Type Bridge

Add a compatibility bridge from inferred Haskell `Type` to the existing model
`ptree` type language.

Keep it intentionally narrow:

- Type variables.
- `Int`, `Double`, `Bool`, `String`.
- `List`.
- Tuples.
- Common model constructors: `Distribution`, `DiscreteDist`, `CTMC`,
  `ExchangeModel`, `MultiMixtureModel`, and alphabet/model types used in
  bindings.
- Simple class constraints represented as current model constraints.

If an inferred type cannot round-trip through this bridge, require explicit JSON
annotations for that binding.

Do not attempt to support arbitrary Haskell type families, higher-rank types, or
complex qualified constraints here.

## Milestone 8: Optional Annotation Support

Allow `result_type` and arg `type` to be omitted only when inference succeeds
and the bridge can produce the existing model type representation.

Rules:

- If a JSON annotation is present, continue using it and audit against
  inference.
- If a JSON annotation is absent, fill `Rule::result_type` and `RuleArg::type`
  from inferred types.
- If inference fails or the bridge fails, emit a binding-definition error
  requiring explicit annotation.
- Constraints follow the same pattern: explicit if present, inferred if absent,
  audit if both exist.

This keeps `src/models/typecheck.cc` mostly unchanged.

## Milestone 9: Remove Simple Annotations

Remove annotations only for bindings classified as exact match or
match-after-normalization.

Suggested order:

1. Simple Prelude-like functions: `_add`, `_sub`, `_mul`, `_eq`, `_lt`, `min`,
   `max`.
2. Constructors: `Nil`, `Cons`.
3. Simple utility functions: `length`, `replicate`, `take`, `zip`.
4. Simple distributions: `normal`, `gamma`, `uniform`, `iid`.
5. More complex model bindings only after defaults and alphabet expressions are
   confirmed stable.

Keep explicit annotations for ambiguous or intentionally model-specific
bindings.

## Milestone 10: Instance Head Export

After optional annotations work, expose instance heads from compiled Haskell
modules.

Deliverables:

- Query available instance signatures/heads by class.
- Include imported/transitive module instances as Haskell typechecking would.
- Do not expose instance bodies.
- Add tests for `Num`, `Eq`, `Ord`, and domain classes such as `Nucleotides`,
  `Triplets`, `Doublets`.

This enables earlier diagnostics but is not required for initial annotation
removal.

## Milestone 11: Limited Instance-Aware Diagnostics

Add early model-layer failures for concrete constraints only.

Start with:

- Accepted: `Num Double`, `Num Int`, `Eq Int`, `Ord Double`.
- Rejected: `Num String`, `Ord (Distribution a)` when concrete enough.
- Domain checks: `Nucleotides DNA`, `Triplets ...`, `Doublets ...`.

Leave polymorphic constraints unresolved and allow Haskell compilation to handle
remaining cases.

## Milestone 12: Revisit Type Representation

Only after the audit and optional annotations are working, decide whether to
replace model `ptree` types with Haskell `Type`.

Use evidence from the audit:

- How many inferred signatures cannot bridge to `ptree`?
- How often do constraints require real Haskell solving?
- How much duplicate type logic remains?
- Are model conversions still clearer in the current model type system?

Possible outcomes:

- Keep `ptree` and bridge inferred signatures.
- Use Haskell `Type` only inside `Rule` signatures and constraints.
- Gradually migrate `models/typecheck.cc` to Haskell `Type`.
- Fully retire `models/unification.*` later.

## Testing Strategy

Add tests incrementally:

- Value signature lookup tests.
- Call lowering equivalence tests.
- Binding inference tests.
- Name resolution tests.
- Annotation comparison tests.
- Whole-tree audit test or script.
- Optional annotation regression tests.
- Later instance-head lookup tests.

## Implementation Principle

Haskell inference should first be an internal checker for the binding database.
Only after it agrees with current annotations should it become the source of
missing annotations.  Constraint solving and model typechecker replacement are
follow-up migrations, not prerequisites.
