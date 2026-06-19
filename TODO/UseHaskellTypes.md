# Use Haskell Types For Command-Line Bindings

Goal: use Haskell type inference to audit command-line binding signatures, then
allow redundant JSON type annotations to be omitted where inference is stable.
Explicit JSON signatures remain a permanent override.  Do not replace
`models/typecheck.cc` or `models/unification.*` until inferred signatures have
enough audit data.

## Current Architecture

- Binding JSON files have one signature mode: explicit, inferred, or invalid
  mixed mode.
- Haskell binding import contexts are compiled once through
  `HaskellBindingContexts`: `module_loader -> synthetic import modules -> one
  Program -> CompiledModule contexts`.
- Per-rule synthetic modules are created through `HaskellBindingContexts`, so
  import normalization and `perform_imports(...)` setup have one owner.
- Rule-template lowering is shared with code generation.  It builds located
  Haskell expressions directly, records referenced `@arg` names, and resolves
  globals/lambda locals in inference mode.
- `RuleCallAnalysisInput` is the non-`Rule` input for call analysis.  It carries
  only rule name, call template, imports, and argument metadata.
- `RuleCallAnalysis` is the boundary shared by explicit and inferred rules.  It
  records the resolved Haskell expression, referenced args, resolved symbols, an
  optional semantic signature, and separate resolution/inference errors.
- Strict inferred-mode loading is a policy wrapper around `RuleCallAnalysis`.
  It requires call-only inference input, requires every declared arg to appear
  in `call`, and then derives the legacy compatibility signature.
- `Rules` construction requires a Haskell module loader.  The old explicit-only
  constructor without a loader has been removed.
- Inferred rules retain semantic Haskell signatures on `Rule`, including
  quantified `TypeVar`s, result and arg `Type`s, and Haskell constraints.
- The current `CM::Type` and model-constraint fields are compatibility views
  derived from retained Haskell signatures for the existing model typechecker.
- Retained Haskell constraints are not yet used as a general model-time
  constraint solver; that is a later migration step.
- Loader diagnostics distinguish Haskell inference failures from
  Haskell-to-`CM::Type` bridge failures.
- Explicit rules retain `RuleHaskellCallAnalysis` during loading.  Broad
  package loading currently records Haskell resolution data only; raw signature
  inference for explicit rules remains available in focused analysis tests and
  should be replaced by expected-type checking before it is run over every
  binding.
- Explicit signatures currently act as overrides; a future experiment should
  typecheck call expressions against those declared types instead.

## Implemented End-To-End Checks

- `bindings/functions/take.json`: `Int -> List<a> -> List<a>`
- `bindings/functions/_eq.json`: `Eq<a> => a -> a -> Bool`
- `bindings/functions/replicate.json`: `Int -> a -> List<a>`
- `bindings/functions/zip.json`: `List<a> -> List<b> -> List<(a,b)>`

Name-resolution parity tests compare the symbols used by rule-call analysis
against compiled value-signature lookup for Prelude names, symbolic operators,
explicit imports, qualified names, constructors, and missing globals.

## Signature Modes

- Explicit mode: `result_type` is present and every arg has `type`.
  `constraints` may be present or absent.
- Inferred mode: `result_type` is absent, every arg omits `type`, and
  `constraints` is absent.  Args still define command-line names, order,
  defaults, alphabets, docs, and other metadata.
- Mixed mode: any partial combination is an error.

## Rule-Call Inference

For one binding `call` template:

1. Read binding imports, defaulting to `Prelude`.
2. Create fresh local Haskell variables for JSON args.
3. Lower `call` with the shared rule-template lowering helper.
4. Optionally create a synthetic declaration such as
   `infer_rule arg_x = length arg_x`.
5. Use `TypeChecker::infer_type_for_decls_group(...)` in the compiled import
   context.
6. Read the generalized semantic `Type` from `poly_env()`.
7. Peel `forall` binders and constraints, then split arrows into result and
   arg types.

Do not store live `MetaTypeVar`s or solver evidence in `Rule` objects.

Current strict inferred mode does not infer from `default_value` or `alphabet`
expressions.  If a declared arg is absent from `call`, the rule must stay
explicit for now.

## Compatibility Bridge

The Haskell-to-model bridge is intentionally narrow.  It currently covers type
variables, `Int`, `Double`, `Bool`, lists, tuples, function arrows, and simple
unary `Eq`, `Ord`, and `Num` constraints represented as current model
constraints.

The bridge should grow only as needed for audited bindings.  Do not bridge
arbitrary type families, higher-rank types, complex qualified constraints, or
kind-polymorphic signatures in this phase.

Likely next targets include model constructors such as `Distribution`,
`DiscreteDist`, `CTMC`, `ExchangeModel`, `MultiMixtureModel`, `Tree`,
`Topology`, and alphabet/model types used in bindings.

`length` remains explicit because Haskell infers
`Foldable t => t a -> Int`, while the command-line interface intentionally
exposes `List<a> -> Int`.

## Remaining Work

### 1. Annotation Audit

Add a developer/test-only audit result that stores, per explicit rule:

- The explicit JSON signature.
- The retained `RuleHaskellCallAnalysis`: context status, resolved symbols,
  referenced args, and semantic Haskell signature if expected-type checking or
  focused inference produced one.
- Bridge status and bridge diagnostics.
- Semantic comparison status.

Compare retained Haskell predicates separately from bridged model constraints.
Start with a bridge-failing rule such as `length`, so audit mode proves it can
report useful Haskell information even when compatibility conversion fails.
Normalize alpha-renamed variables, qualified/unqualified known constructors,
Haskell list/tuple spelling, constraint ordering, and solver-simplified
residual predicates.

### 2. Whole-Tree Report

Run audit mode across `bindings/*.json` and group rules into:

- Exact match or match after normalization.
- Inferred or declared extra constraints.
- Ambiguous inference.
- Argument absent from `call`.
- Unsupported inferred type shape.
- Haskell-to-`CM::Type` bridge failure.
- Name-resolution or call-lowering failure.

This report is the broad migration map.  Do not remove large groups of
annotations before it exists.

### 3. Optional Annotation Support

Allow inferred mode only when:

- The binding is not mixed mode.
- Every arg occurs in `call`.
- Haskell inference succeeds.
- Result, arg types, and constraints bridge to current model representations.
- No unresolved ambiguous Haskell type remains.
- The template uses supported lowering syntax.

After these checks, retain the semantic Haskell signature and populate the
legacy `Rule::result_type`, `RuleArg::type`, and `Rule::constraints` fields for
the current model typechecker.

### 4. Typecheck Calls Under Explicit Signatures

Explicit signatures should not have to mean "trust JSON and ignore the Haskell
call type."  A future experiment should translate an explicit JSON signature
into an expected Haskell function type, typecheck the lowered `call` expression
under that expected type, and report whether the Haskell call really supports
the declared command-line interface.

This differs from pure inference:

- Inference asks, "what type does this call have?"
- Expected-type checking asks, "can this call be used at this declared type?"

This matters for rules whose explicit command-line type is narrower than the
raw Haskell type, such as `length :: List<a> -> Int` versus the inferred
`Foldable t => t a -> Int`.  It also gives explicit rules a path to Haskell
name resolution, predicate retention, and audit diagnostics without making
their signatures optional.

The experiment should:

- Build the same synthetic imported module used by `RuleCallAnalysis`.
- Convert explicit result and arg types into an expected Haskell function type
  when possible.
- Typecheck the lowered call against that expected type rather than only
  reading the unconstrained inferred type.
- Keep the explicit JSON signature as the command-line interface while storing
  any Haskell predicates needed to justify that interface.
- Report cases where the JSON signature cannot be translated, the call does not
  fit the expected type, or the required predicates cannot yet be represented.

### 5. Preserve Arbitrary Haskell Constraints

Long term, rule signatures should be able to retain arbitrary Haskell
constraints instead of only the small set that can be bridged to current model
constraints.  The model bridge can remain narrow, but the semantic
`RuleHaskellSignature` should preserve predicates so generated Haskell,
diagnostics, and future constraint solving can see the same obligations the
Haskell typechecker derived.

This should allow predicates to pass through even when the model layer cannot
interpret them yet.  Concrete predicates that the model layer understands can be
checked early; opaque or polymorphic predicates can remain as residual Haskell
obligations until a later solver or the generated Haskell program handles them.

Type families and associated types are unresolved.  Until there is evidence
from audit data and the Haskell typechecker API, treat type-family applications
as opaque residual structure unless they can be normalized in the compiled
import context.  Do not design the first constraint pass around full type-family
reduction.

### 6. Constraint-Aware Model Typechecking

Eventually, the model typechecker should use retained Haskell constraints, not
just store or bridge them.  Rule application should instantiate a rule's
semantic Haskell predicates with the model expression's inferred types, collect
the resulting obligations, and check concrete obligations against compiled
Haskell instance information before generated Haskell is run.

For example, an expression such as `hky85 + hky85` should fail during
command-line model typechecking because `(+)` requires `Num Markov`, and there
is no such instance.  The error should report the operator/rule, the unsatisfied
class predicate, the concrete type, and the import context used for instance
lookup.

This stage should:

- Reuse retained `RuleHaskellSignature` constraints for both explicit-audited
  and inferred rules.
- Substitute model type variables into Haskell predicates at each rule
  application.
- Discharge concrete predicates using compiled instance heads from the relevant
  Haskell import context.
- Reject unsatisfied concrete constraints before code generation or Haskell
  execution.
- Preserve residual polymorphic constraints only where the surrounding
  expression can still generalize them.
- Keep model-language coercions separate from Haskell class solving.

### 7. Remove Simple Annotations

Remove annotations only for bindings classified as exact match or match after
normalization.  Prefer this order:

1. Simple Prelude-like functions: `_add`, `_sub`, `_mul`, `_eq`, `_lt`, `min`,
   `max`.
2. Constructors: `Nil`, `Cons`.
3. Simple utilities: `replicate`, `take`, `zip`, `zipWith`, and only later
   `length`.
4. Simple distributions: `normal`, `gamma`, `uniform`, `iid`.
5. More complex model bindings after defaults and alphabets are stable.

Keep explicit annotations for ambiguous bindings, absent args, and bindings
whose command-line interface intentionally differs from the raw Haskell type.

### 8. Retire Remaining Lowering Compatibility

Code generation still has a marked compatibility wrapper that unwraps located
Haskell template expressions for older call sites.  Teach codegen to carry
located expressions directly, then remove the wrapper.

### 9. Instance-Aware Diagnostics

After optional annotations work, expose imported instance heads from compiled
modules for diagnostics.  Start with `Num`, `Eq`, `Ord`, and domain classes such
as `Nucleotides`, `Triplets`, and `Doublets`.  Use instance heads first for
diagnostics and concrete constraint rejection; do not expose instance bodies.

### 10. Preserve Model-Language Coercions

Keep model-language conversion insertion separate from Haskell signature
inference.  Existing inserted calls such as `intToDouble`, `discrete`,
`convertDiscrete`, `unit_mixture`, `multiMixtureModel`, and `f` are
command-line language conveniences, not ordinary Haskell unification.

Do not remove `convertible_to()` until an equivalent model coercion layer exists
for Haskell-backed types.

### 11. Revisit Type Representation

Only after audit mode and optional annotations are working, decide whether to
replace `CM::Type` in the model typechecker.  Use audit data to decide whether
to keep the bridge, store Haskell `Type` only on `Rule`, gradually migrate
`models/typecheck.cc`, or eventually retire `models/unification.*`.

### 12. Infer From Defaults And Alphabets

After call-only inference is stable, consider using `default_value` and
`alphabet` expressions to constrain otherwise absent or underconstrained args.
This is optional; explicit JSON signatures remain the fallback.

## Testing Strategy

Add coverage incrementally for signature modes, compiled value lookup, shared
template lowering, binding inference, name-resolution parity, bridge behavior,
annotation audit/reporting, explicit-call expected-type checking, arbitrary
constraint retention, optional annotations, concrete unsatisfied constraints
such as `hky85 + hky85`, instance diagnostics, and code generation paths such
as `5d +A`.

## Implementation Principle

Haskell inference should first be an internal checker for the binding database.
Only after it agrees with current annotations should it become the source of
missing annotations.  Constraint solving, model typechecker replacement, and
full Haskell `Type` migration are follow-up migrations, not prerequisites.
