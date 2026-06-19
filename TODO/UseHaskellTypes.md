# Use Haskell Types For Command-Line Bindings

Goal: Make command-line binding signatures use Haskell types.  When types are
not specified, infer them -- otherwise check them.  

Strategy: For the moment, treat explicit signatures as an override, to avoid
breaking the program.

## Signature Modes

- Explicit mode: `result_type` is present and every arg has `type`.
  `constraints` may be present or absent.
- Inferred mode: `result_type` is absent, every arg omits `type`, and
  `constraints` is absent.  Args still define command-line names, order,
  defaults, alphabets, docs, and other metadata.
- Mixed mode: any partial combination is an error.

## Rule-Call Inference

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

### Typecheck Calls Under Explicit Signatures

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

### Preserve Arbitrary Haskell Constraints

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

### Constraint-Aware Model Typechecking

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

### Instance-Aware Diagnostics

After optional annotations work, expose imported instance heads from compiled
modules for diagnostics.  Start with `Num`, `Eq`, `Ord`, and domain classes such
as `Nucleotides`, `Triplets`, and `Doublets`.  Use instance heads first for
diagnostics and concrete constraint rejection; do not expose instance bodies.

### Preserve Model-Language Coercions

Keep model-language conversion insertion separate from Haskell signature
inference.  Existing inserted calls such as `intToDouble`, `discrete`,
`convertDiscrete`, `unit_mixture`, `multiMixtureModel`, and `f` are
command-line language conveniences, not ordinary Haskell unification.

Eventually we want to create a haskell typeclass `Convertible a b` with
a method `convert :: a -> b`, and implement these specific conversions as
instances.  This is going to require asking Haskell which instances are
available.

Do not remove `convertible_to()` until an equivalent model coercion layer exists
for Haskell-backed types.

### Revisit Type Representation

Consider replacing `CM::Type` in the model typechecker.
Should we start using haskell unification and typechecking machinery?

### Infer From Defaults And Alphabets

After call-only inference is stable, consider using `default_value` and
`alphabet` expressions to constrain otherwise absent or underconstrained args.
This is optional; explicit JSON signatures remain the fallback.

