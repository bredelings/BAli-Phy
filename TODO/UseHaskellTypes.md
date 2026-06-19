# Use Haskell Types For Command-Line Bindings

Goal: use Haskell type inference to audit command-line binding signatures first,
then allow redundant JSON type annotations to be omitted for bindings where
inference is stable.  Keep explicit JSON signatures as a permanent override.
Do not replace `models/typecheck.cc` or `models/unification.*` until inferred
binding signatures are proven useful.

## Current Status

Initial infrastructure and end-to-end inferred bindings are in place:

- Binding JSON signature modes are validated before type conversion.
- Mixed signatures are rejected.
- Haskell binding import contexts are compiled as synthetic import-only modules:
  `module_loader -> synthetic import modules -> one Program -> CompiledModule contexts`.
- Per-rule synthetic inference modules are now constructed through
  `HaskellBindingContexts`, so import normalization and `perform_imports(...)`
  setup have one owner.
- Rule-template lowering is shared with code generation and records referenced
  `@arg` names.  It now builds located Haskell expressions and `Hs::ApplyExp`
  nodes directly; a marked compatibility wrapper still unwraps the result for
  current code-generation call sites.
- A semantic value-signature lookup API returns compiled `Type`, `TypeVar`, and
  constraint data, and serves as the read-only compiled-symbol oracle for
  parity tests.
- Focused tests compare rule-inference name resolution with direct
  value-signature lookup for representative Prelude names, symbolic operators,
  explicit imports, qualified names, constructors, and missing globals.
- Rule-call analysis uses a dedicated `RuleCallAnalysisInput` instead of
  partially initialized `Rule` objects.
- `RuleCallAnalysis` is the shared boundary for explicit and inferred rules:
  it lowers a binding call template in inference mode, records the resolved
  Haskell expression, referenced `@arg` names, resolved Haskell symbols, an
  optional semantic signature, and separate resolution/inference errors.
- Strict inferred-mode loading is now a wrapper around rule-call analysis: it
  requires call-only inference inputs, checks that every declared arg was
  referenced, and then derives the legacy compatibility signature.
- Explicit-rule-style tests can now resolve and infer calls such as `length`
  and `Data.List.sort` without requiring the narrow `CM::Type` bridge to
  accept the inferred constraints.
- Rule-call analysis lowers a binding call template in inference mode, resolving
  globals and lambda locals during shared template lowering, then optionally
  builds a synthetic Haskell function declaration, runs the existing
  `TypeChecker` declaration inference path in a compiled import context, and
  reads generalized semantic `Type` values from `poly_env()`.
- Inferred semantic signatures retain their quantified `TypeVar`s as well as
  result, argument, and constraint `Type` values.
- Loader diagnostics now distinguish Haskell inference failures from
  Haskell-to-`CM::Type` compatibility bridge failures.
- A narrow semantic Haskell `Type` to `CM::Type` bridge supports variables,
  `Int`, `Double`, `Bool`, lists, tuples, and function arrows.
- A narrow semantic Haskell constraint bridge supports unary `Eq`, `Ord`, and
  `Num` class predicates as existing model constraints.
- Loader-aware `Rules(package_paths, module_loader)` can resolve inferred
  signatures before the existing model typechecker sees a rule.
- `bindings/functions/take.json` now omits redundant signature fields and gets
  `Int -> List<a> -> List<a>` from Haskell inference.
- `bindings/functions/_eq.json` now omits redundant signature and constraint
  fields and gets `Eq<a> => a -> a -> Bool` from Haskell inference.
- `bindings/functions/replicate.json` now omits redundant signature fields and
  gets `Int -> a -> List<a>` from Haskell inference.
- `bindings/functions/zip.json` now omits redundant signature fields and gets
  `List<a> -> List<b> -> List<(a,b)>` from Haskell inference.

The compatibility constructor `Rules(package_paths)` remains explicit-only for
tests and simple callers that do not have a Haskell module loader.  Broader
constraints, defaults, alphabets, and broad annotation removal still require
follow-up work.

## Architecture Cleanup Plan

The inferred `take` and `==` bindings prove that the inference path can work,
but they also expose several design problems that should be corrected before
removing more annotations.

### 1. Preserve Semantic Haskell Signatures

Problem: inferred semantic Haskell signatures are immediately collapsed into
legacy `CM::Type` fields.  This makes the current model typechecker happy, but
it throws away the qualified Haskell predicates that should become the long-term
source of truth.

Plan:

- Add a durable rule signature field, probably
  `std::optional<RuleHaskellSignature> Rule::haskell_signature`.
- Store semantic `Type` values for the result, each JSON arg, and each inferred
  constraint.
- Populate this field only for inferred rules at first; explicit JSON rules can
  remain legacy-only until audit mode records inferred counterparts for them.
- Continue filling `Rule::result_type`, `RuleArg::type`, and
  `Rule::constraints` from the legacy `CM::Type` bridge so existing model
  typechecking behavior does not change.
- Add tests that loader-aware `Rules` retains semantic signatures for `take` and
  `==`, including the original Haskell class predicate for `==`.

Done when: inferred rules keep their Haskell `Type` and constraint data after
`Rules` construction, and the legacy `CM::Type` view is explicitly a derived
compatibility view.

Status: implemented for inferred rules, including retained quantified
variables.  Explicit JSON rules can gain audited Haskell signatures later.

### 2. Make Constraint Bridging A Compatibility Boundary

Problem: the narrow constraint bridge currently looks like the inference design
itself, because it maps selected Haskell classes such as `Data.Eq.Eq` directly
to model constraints such as `Eq<a>`.

Plan:

- Treat Haskell predicates stored in `Rule::haskell_signature` as authoritative
  for inferred bindings.
- Use `bridge_haskell_constraint_to_model_constraint(...)` only while deriving
  the legacy `Rule::constraints` view.
- Keep the bridge intentionally narrow until the model typechecker can consume
  Haskell predicates or a generic class-reference representation.
- Add a brief code note at the bridge call site explaining that the bridge is a
  compatibility output for the current model typechecker.

Done when: code and tests make it clear that `Data.Eq.Eq a` is retained, and
`Eq<a>` is only the compatibility representation used by current model typing.

Status: implemented for inferred rules through `Rule::haskell_signature` and
the compatibility bridge call site.

### 3. Strengthen Name-Resolution Parity Tests

Problem: current parity tests lower a template and separately look up an
expected name, but they do not prove that the lowered expression used by
inference resolved the same symbol.

Plan:

- Expose a small inspection helper from the inference path, or a test-only
  wrapper around it, that lowers a rule template in an import context and
  returns the resolved global symbols it used.
- Compare those resolved names against `lookup_value_signature(...).resolved_name`.
- Cover unqualified Prelude names, symbolic operators, qualified imports,
  unqualified names from explicit binding imports, constructors, and an
  ambiguity/error case.
- Keep the helper tied to the same conversion path used by
  `infer_rule_call_signature(...)`; do not add a separate name-resolution
  implementation just for tests.

Done when: a test fails if inference would typecheck a different Haskell symbol
than the one the binding template appears to name.

Status: implemented for representative successful resolutions and missing
global failures through the same conversion path used by rule-call analysis.
`RuleCallAnalysis` also returns the resolved symbols beside the lowered Haskell
expression so explicit-rule audit code can reuse the same observation point.

### 4. Use Or Demote Value Signature Lookup

Problem: `lookup_value_signature(...)` is public model-layer infrastructure, but
it is currently only used by tests.  That risks becoming parallel infrastructure
beside the real inference path.

Plan:

- Keep `lookup_value_signature(...)` as the canonical read-only compiled-symbol
  lookup API.
- Use it in the strengthened parity tests described above.
- Later, use it in annotation audit diagnostics when reporting declared versus
  imported Haskell signatures.
- If audit work later chooses a different lookup API, move this helper into the
  test file instead of leaving it as unused production infrastructure.

Done when: the helper has a clear role in parity/audit code, or it is no longer
part of production sources.

Status: kept as the canonical read-only compiled-symbol lookup used by
name-resolution parity tests.  Audit diagnostics can reuse it later.

### 5. Fix The Second-Pass Inference Conversion

Problem: rule-template lowering is shared with code generation, but inference
then walks the lowered expression and resolves globals again.  That conversion
must not invent different scoping semantics.

Plan:

- Keep the current shared lowering for now; do not redesign it before the bug
  surface is pinned down.
- Teach the inference conversion to track local binders introduced by
  `Hs::LambdaExp` before resolving `var` nodes as globals.
- Add a regression test for an inferred rule template containing a lambda with a
  variable binder.
- If tuple-pattern or more complex lambda inference is not supported yet, add an
  xfail test for the unsupported shape before broadening support.
- After the narrow fix, consider returning a more typed lowering result for
  inference, such as a Haskell expression plus referenced args plus local binder
  metadata.

Done when: lambda-bound variables in inferred templates are not looked up as
imported globals, and tests cover that behavior.

Status: implemented by making rule-template lowering produce located Haskell
applications directly and by moving inference name resolution, including lambda
locals, into the shared lowering path.  The old second-pass inference conversion
has been removed.

### 6. Move Synthetic Inference Modules Into The Context Abstraction

Problem: `HaskellBindingContexts` compiles import-set context modules, but
`rule-call-inference.cc` still constructs a separate per-rule synthetic module
and only calls `context_for(...)` as a guard/cache check.

Plan:

- Add a narrow API to `HaskellBindingContexts`, for example
  `make_imported_module(imports, module_name, body_source)`.
- The API should normalize imports, require the compiled import context, build
  the synthetic `Module`, and call `perform_imports(program())`.
- Update `infer_rule_function_type(...)` to use this API instead of constructing
  and importing the module directly.
- Keep typechecking in `rule-call-inference.cc`; only module/import ownership
  moves into `HaskellBindingContexts`.
- Later interface generation for import sets or inferred rule modules should use
  this same abstraction.

Done when: there is one owner for import normalization and synthetic
module-import setup, and per-rule inference modules are no longer ad hoc
siblings of the compiled contexts.

Status: implemented with `HaskellBindingContexts::make_imported_module(...)`.

### 7. Update Audit And Documentation Around The New Boundary

Problem: the written milestone list still reads partly as if inference should
collapse to `CM::Type` and then proceed.  The more durable design is:

```text
compiled import-set context
  -> synthetic rule inference module
  -> semantic Haskell signature retained on Rule
  -> legacy CM::Type compatibility view for current model typechecking
```

Plan:

- Update audit mode to compare both semantic Haskell signatures and bridged
  legacy signatures.
- Report bridge failures as compatibility failures, not as Haskell inference
  failures.
- Report constraint differences using the retained Haskell predicates when
  possible.
- Keep explicit JSON signatures as the permanent override when the command-line
  interface intentionally differs from the raw Haskell type.

Done when: audit reports distinguish inference, semantic signature retention,
and legacy compatibility bridging.

Status: partially implemented for rule loading and analysis: inference failures
and Haskell-to-model compatibility bridge failures now report separate stages,
and explicit-rule-style analysis can keep a semantic signature even when the
compatibility bridge would reject a constraint.  Whole-tree audit comparison is
still future work.

### 8. Remove Partial Rule Inference Inputs

Problem: inference previously accepted skeletal `Rule` objects that were missing
the final types and metadata normally expected on `Rule`.

Status: implemented with `RuleCallAnalysisInput`, which carries only the rule
name, call template, import set, and argument metadata needed by call analysis.
The current strict inferred-mode wrapper still rejects defaults and alphabets
with a temporary limitation note, but best-effort call analysis itself is not
tied to those loader policy checks.

### 9. Keep Synthetic Inference Source Useful

Problem: synthetic inference module text used to say `infer_rule = <rule name>`,
while the actual declaration was built separately as AST.

Status: improved.  Synthetic module source now mirrors the generated inference
function shape and synthetic argument names.  The AST declaration remains the
source of behavior.

## Suggested Next Cleanup Batch

Name-resolution observability, load-time bridge diagnostics, and explicit-style
call analysis are now in place.  The next useful cleanup slice should either
retire the remaining codegen compatibility wrapper around located Haskell
template expressions, or start turning `RuleCallAnalysis` into the
explicit-annotation audit report:

1. Audit/reporting option: add an audit result shape that stores the explicit
   JSON signature, the `RuleCallAnalysis` semantic signature, bridge status, and
   comparison status for one rule.
2. Audit/reporting option: compare retained Haskell predicates separately from
   bridged model constraints for one or two explicit rules, starting with a
   bridge-failing case such as `length`.
3. Lowering/codegen option: teach codegen call sites to carry located Haskell
   expressions directly and remove `make_rule_template_expr(...)`.
4. Annotation-removal option: convert another small binding only after its call
   shape is covered by the stronger resolution tests and the compatibility
   bridge remains narrow.

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
- Accept inferred mode only through loader-aware `Rules` construction at first.
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

The reusable implementation boundary is `RuleCallAnalysis`: resolution and
optional Haskell signature inference are best-effort and return structured
results, while strict inferred-mode loading is a policy wrapper that turns
missing references, resolution failures, inference failures, and bridge failures
into loader errors.

Initial inferred mode should require every declared arg to occur in `call`.
If an arg is absent, fail inference with a clear message requiring explicit
JSON annotations.  Do not infer from `default_value` or `alphabet` expressions
in this milestone.

Prototype bindings should be simple:

- `take`: `Int -> [a] -> [a]`
- `_eq`: `Eq a => a -> a -> Bool`
- `replicate`: `Int -> a -> [a]`
- `zip`: `[a] -> [b] -> [(a,b)]`
- later, after more audit data: `_add`, `_lt`, `min`, `max`, `length`

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
- Simple unary `Eq`, `Ord`, and `Num` class constraints represented as current
  model constraints.

If an inferred type cannot round-trip through this bridge, require explicit
JSON annotations for that binding.

The retained semantic Haskell signature is the source of truth for inferred
rules.  The `CM::Type` bridge only derives the compatibility view consumed by
the current model typechecker.

Do not try to bridge arbitrary type families, higher-rank types, complex
qualified constraints, or kind-polymorphic signatures in this phase.

## Milestone 7: Annotation Comparison Audit Mode

For bindings that still carry explicit JSON annotations, keep those annotations
required while audit mode is introduced.

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

No normal user-facing behavior should change in this milestone, apart from
prototype bindings that have already been deliberately converted to inferred
mode.

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

This report is the broad migration map.  Do not remove large groups of
annotations before it exists; prototype inferred bindings may remain as focused
end-to-end checks.

## Milestone 9: Optional Annotation Support

Enable inferred mode for bindings that pass all checks:

- The binding is in inferred mode, not mixed mode.
- Every arg occurs in `call`.
- Inference succeeds.
- Inferred result and arg types bridge to `CM::Type`.
- Inferred constraints bridge to current model constraints.
- No ambiguous Haskell type remains after simplification/generalization.
- The template uses only supported lowering syntax.

Retain the semantic Haskell signature on the rule, then populate
`Rule::result_type`, `RuleArg::type`, and `Rule::constraints` from the bridged
compatibility signature before `models/typecheck.cc` sees the rule.

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
