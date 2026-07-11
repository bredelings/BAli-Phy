# Functional dependencies implementation plan

This is the reviewed, second-stage plan for implementing GHC-style functional
dependencies.  The parser and source AST already represent FunDeps, so the
work begins at class metadata and continues through instance validation,
constraint improvement, generalization, and module serialization.

The scope is ordinary class functional dependencies.  Type-family injectivity
and implicit-parameter improvement are related mechanisms in GHC, but are not
part of this work.  FunDeps remain compile-time information: they do not alter
dictionary layout, Core, code generation, or runtime evaluation.

## Review decisions

- Do not add FunDep improvement directly to `Solver::interact`.  This compiler
  performs inert interaction before instance lookup, while GHC gives ordinary
  instance resolution priority over FunDep improvement.  Add a dedicated
  post-instance improvement phase instead.
- Do not add a persistent `Derived` constraint flavor.  Run proposed FunDep
  equalities in a nested solver, retain only useful metavariable unifications,
  and discard evidence and residual equalities.
- Store normalized FunDeps in serialized `ClassInfo`.  Keeping them only in
  `Hs::ClassDecl` would silently lose their semantics across module boundaries.
- Treat coverage and consistency as independent instance checks.  Ordinary
  overlap checking does not imply either one.
- Implement dependency closure during generalization as part of the feature.
  Solver improvement alone would still infer incorrect or ambiguous types.
- Match current GHC behavior by accepting redundant and empty dependencies,
  including `a -> a`, `a ->`, and `-> a`.
- Preserve one grouped set of equations for a multi-result dependency.
  `a -> b c` is not internally interchangeable with the two independently
  instantiated dependencies `a -> b` and `a -> c`.
- Do not add GHC's FunDep-specific orphan anchors now.  Instance lookup already
  searches all transitively imported modules.  Cross-module metadata and
  behavior still require tests.
- Land tests with each implementation commit.  Use `NoImplicitPrelude` where
  possible; no tests need to be marked `xfail`.

## Commit 1: Retain functional dependencies in class metadata

Suggested description:

```text
Retain functional dependencies in class metadata
```

### Extension handling

Modify `src/computation/haskell/extensions.cc`:

- Make enabling `FunctionalDependencies` also enable
  `MultiParamTypeClasses`.
- Preserve the existing explicit extension bookkeeping.
- Do not enable `FunctionalDependencies` by default.
- Report an error when a class contains FunDeps without the extension enabled.

### Normalized representation

Modify `src/computation/typecheck/env.H` to add:

```cpp
struct FunctionalDependency
{
    std::vector<int> determining;
    std::vector<int> determined;

    template <class Archive>
    void serialize(Archive& ar);
};
```

Add this field to `ClassInfo` and to `ClassInfo::serialize`:

```cpp
std::vector<FunctionalDependency> functional_dependencies;
```

Parameter indexes are preferable to source `Hs::TypeVar`s because every
dependency variable must name a class parameter, instantiation becomes direct
indexing into class arguments, and indexes survive interface serialization
without source-name concerns.  Add a short comment explaining this choice.

### Declaration processing

Modify `src/computation/typecheck/class.cc`:

- Build a map from every `class_decl.type_vars` name to its parameter index.
- Convert every source FunDep to one `FunctionalDependency`.
- Preserve source order, repeated variables, and empty sides.
- Report an error at the variable's location when it is not a class parameter.
- Store only successfully translated dependencies in `ClassInfo`.

Modify `src/computation/rename/types.cc` only as needed to ensure an unbound
FunDep variable receives the usual scoped type-variable diagnostic.  Do not
qualify FunDep variables as module names.

### Tests

Create:

```text
tests/haskell/Types/FunctionalDependencies/Basic
tests/haskell/Types/FunctionalDependencies/ImpliesMultiParam
tests/haskell/Types/FunctionalDependencies/ExtensionDisabled
tests/haskell/Types/FunctionalDependencies/UnknownParameter
tests/haskell/Types/FunctionalDependencies/RedundantForms
```

Test that:

- `class C a b | a -> b` is accepted with the extension.
- `FunctionalDependencies` alone enables a multi-parameter class.
- The declaration fails without the extension.
- `class C a b | a -> c` reports that `c` is not a class parameter.
- Empty and redundant forms accepted by GHC remain accepted.

### Verification

Run:

```sh
ninja -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O

meson test -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O \
  --print-errorlogs \
  'bali-phy:runtime AST serialization'
```

Run each new FunctionalDependencies test by its full Meson test name.

## Commit 2: Enforce functional-dependency coverage

Suggested description:

```text
Check functional dependency coverage for instances
```

### Shared FunDep operations

Create:

```text
src/computation/typecheck/fundeps.H
src/computation/typecheck/fundeps.cc
```

Add `fundeps.cc` to `src/computation/meson.build`.

Introduce only the operations immediately required by coverage:

```cpp
using InstantiatedFunDep =
    std::pair<std::vector<Type>, std::vector<Type>>;

InstantiatedFunDep
instantiate_fun_dep(const FunctionalDependency&,
                    const std::vector<Type>& class_args);
```

Add rigid- and meta-variable dependency-closure overloads:

```cpp
std::set<TypeVar>
close_wrt_fun_deps(TypeChecker&,
                   const std::vector<Type>& predicates,
                   std::set<TypeVar> fixed);

std::set<MetaTypeVar>
close_wrt_fun_deps(TypeChecker&,
                   const std::vector<Type>& predicates,
                   std::set<MetaTypeVar> fixed);
```

Use one private templated fixed-point implementation so the overloads do not
diverge semantically.  For every predicate considered by the closure:

- For a nominal equality `t1 ~ t2`, add determination in both directions.
- For a class predicate, instantiate every dependency using its arguments.
- Recursively include substituted superclass predicates.
- Avoid superclass cycles with a visited-predicate set.
- Treat every free variable in a determining type as fixed when that type is
  known.
- Add only injective variables from determined types.  Reuse
  `injective_vars_for_type` for meta variables and add its rigid counterpart.
- Close over variables in kinds whenever variables are added.
- Iterate until no variable is added.

Document why this operation is narrower than collecting all variables that
occur in the same constraint.

### Coverage checking

Modify `src/computation/typecheck/instance.cc`.  After converting the instance
head and context to Core types, but before registering its dictionary, perform
the following for every class FunDep:

1. Instantiate its determining and determined sides with the instance args.
2. Compute free variables of the determining side, including kind variables.
3. Compute free variables of the determined side.  Since this compiler does
   not distinguish GHC's visible and invisible sets, conservatively use all
   free variables and document that behavior.
4. Without `UndecidableInstances`, require the determined variables to occur
   directly in the determining set.
5. With `UndecidableInstances`, close the determining set using the instance
   context and require the determined variables to occur in that closure.
6. On failure, name the instance, FunDep, uncovered variables, and whether the
   ordinary or liberal coverage condition was checked.

Do not weaken the existing Paterson and termination checks.

### Tests

Create:

```text
tests/haskell/Types/FunctionalDependencies/Coverage
tests/haskell/Types/FunctionalDependencies/CoverageFailure
tests/haskell/Types/FunctionalDependencies/LiberalCoverage
tests/haskell/Types/FunctionalDependencies/LiberalCoverageDisabled
tests/haskell/Types/FunctionalDependencies/EqualityCoverage
tests/haskell/Types/FunctionalDependencies/SuperclassCoverage
```

Use `instance C [a] a` as ordinary valid coverage and `instance C Int a`
as invalid coverage.  Exercise liberal coverage with:

```haskell
class D a b | a -> b
class C a b | a -> b
instance D a b => C [a] [b]
```

Require `UndecidableInstances` for that instance.  Add separate tests for
closure through an equality and through a superclass FunDep.

### Verification

Run the build, the complete FunctionalDependencies subtree, and:

```sh
meson test -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O \
  --print-errorlogs \
  'bali-phy:bali-phy testsuite haskell/Types'
```

## Commit 3: Reject inconsistent FunDep instances

Suggested description:

```text
Reject inconsistent functional dependency instances
```

### Consistency algorithm

Extend `fundeps.H/.cc` with:

```cpp
std::vector<InstanceInfo>
inconsistent_fun_dep_instances(TypeChecker&,
                               const InstanceInfo& candidate);
```

For every existing local or transitively imported instance of the same class:

1. Freshen the candidate and existing variables independently.
2. Instantiate the determining and determined portions of both heads.
3. Try to unify the determining portions using their fresh instance vars.
4. If the determining portions cannot unify, that FunDep does not conflict.
5. Apply the determinant substitution to both determined portions.
6. Match current GHC behavior: the determined portions are consistent when
   they can still unify, rather than only when syntactically equal.
7. Report each existing instance once even if several dependencies conflict.

### Instance registration

Modify `check_instance_can_be_added` in `instance.cc`:

- Run consistency independently of duplicate and overlap checking.
- Do not let overlap pragmas or `IncoherentInstances` suppress a FunDep
  inconsistency.
- Include the imported module name in cross-module diagnostics.
- Do not add a rejected instance to `local_instances`.

### Tests

Create:

```text
tests/haskell/Types/FunctionalDependencies/Consistency
tests/haskell/Types/FunctionalDependencies/ConsistencyFailure
tests/haskell/Types/FunctionalDependencies/ConsistencyNonOverlappingHeads
tests/haskell/Types/FunctionalDependencies/ConsistencyImported
tests/haskell/Types/FunctionalDependencies/MultipleDependencies
```

The non-overlap regression should use:

```haskell
class C a b marker | a -> b

data X
data Y

instance C Int Bool X
instance C Int Char Y
```

The heads are distinguished by `X` and `Y`, but conflict because `Int`
determines two different `b` arguments.  Put the class and first instance in a
support module for the imported test.  Verify that one instance conflicting
through two dependencies is reported only once.

### Verification

Run the FunctionalDependencies subtree, existing overlap and instance tests,
and runtime AST serialization.

## Commit 4: Improve unsolved class constraints using FunDeps

Suggested description:

```text
Improve class constraints using functional dependencies
```

Local and top-level improvement belong in one commit because they share the
nested solver and must obey one ordering policy.

### Improvement equation representation

Extend `fundeps.H`:

```cpp
struct FunDepEquations
{
    std::vector<TypeVar> quantified;
    std::vector<std::pair<Type,Type>> equalities;
};
```

The quantified variables are instance variables not fixed by matching the
determining side.  Keep all equalities from one multi-result FunDep together.

Add:

```cpp
std::vector<FunDepEquations>
improve_from_constraint(const ClassInfo&,
                        const std::vector<Type>& template_args,
                        const std::vector<Type>& work_args);

std::vector<FunDepEquations>
improve_from_instances(TypeChecker&,
                       const ClassInfo&,
                       const std::vector<Type>& work_args);
```

For two local constraints of the same class:

- Instantiate every dependency against both argument lists.
- Require determining types to be equal after solver rewriting.
- Pair determined types in template-to-work orientation.
- Omit pairs that are already equal and empty equation groups.
- Use no quantified variables.

For instance improvement:

- Visit every local and imported instance of the class.
- Match only the determining portion of the instance against the wanted.
- Do not require the complete instance head to match.
- Apply the determinant substitution to the instance's determined types.
- Put remaining instance variables used by those types in `quantified`, in an
  order that respects dependencies in their kinds.
- Pair instance determined types with wanted determined types.
- Preserve each multi-result dependency as one equation group.
- Omit groups that are already satisfied.

### Evidence-free nested solving

Add to `Solver`:

```cpp
enum class FunDepResult
{
    NoChange,
    Changed,
    Insoluble
};

FunDepResult
solve_fun_dep_equations(const Constraint& work,
                        const std::vector<FunDepEquations>&);
```

Implement it as follows:

1. Save the outer global `unification_level`.
2. Clear the marker for the nested solve.
3. Copy the typechecker with cleared wanteds and increment its level.
4. Instantiate every quantified variable as a fresh meta-variable at that
   deeper level, substituting through kinds and later variables.
5. Reverse each grouped equality list before submitting it, matching GHC's
   dependency-sensitive order.
6. Create temporary nominal wanted equalities with a new `FunDepOrigin`.
7. Run a fresh nested `Solver`.
8. Discard its Core evidence declarations.
9. Discard residual non-insoluble equalities.
10. Inspect nested failed constraints for definite contradiction.
11. Count only unification of a metavariable at the original or an outer level
    as useful progress; filling fresh deeper variables does not count.
12. Restore the global marker as the minimum of the saved and nested useful
    unification levels.
13. Return `Changed`, `Insoluble`, or `NoChange`.

Add a comment explaining that FunDeps justify type improvement but do not
supply equality evidence.

### Solver ordering

Do not modify the generic dictionary logic in `Solver::interact`.  Modify the
main solver loop so an unsolved wanted canonical dictionary follows this order:

```text
exact inert solution
ordinary instance solution
local FunDep improvement
top-level instance FunDep improvement
inert insertion
```

Concretely:

1. Preserve exact dictionary and superclass reactions.
2. Preserve `top_react`, including ordinary instance resolution.
3. If a wanted dictionary remains unsolved, compare it with same-class inert
   dictionaries.  Skip Given/Given improvement.
4. Solve local equations.  On useful unification, return the original
   constraint to the noncanonical work list and continue.
5. If local improvement made no progress, generate and solve top-level
   instance equations.  Restart the original constraint after useful
   unification.
6. Otherwise insert it normally into the inert set.
7. On definite insolubility, retain the original dictionary in
   `inerts.failed`; do not expose a synthetic equality as the main diagnostic.
8. Kick out all inert constraints affected by successful improvement.

### Tests

Create:

```text
tests/haskell/Types/FunctionalDependencies/LocalImprovement
tests/haskell/Types/FunctionalDependencies/WantedImprovement
tests/haskell/Types/FunctionalDependencies/InstanceImprovement
tests/haskell/Types/FunctionalDependencies/PartialInstanceImprovement
tests/haskell/Types/FunctionalDependencies/MultiResultImprovement
tests/haskell/Types/FunctionalDependencies/InstanceBeforeImprovement
tests/haskell/Types/FunctionalDependencies/LocalBeforeTopImprovement
tests/haskell/Types/FunctionalDependencies/ImportedImprovement
tests/haskell/Types/FunctionalDependencies/InsolubleImprovement
```

Include this ordering regression:

```haskell
class FD a b | a -> b where
    op :: a -> b

instance FD Int Bool

foo :: FD Int b => Int -> Bool
foo = op
```

The exact `FD Int Bool` instance must solve the wanted before attempting to
improve it against the local `FD Int b`.  Port minimized forms of GHC's
`#14745` and `T4254b` local-before-top examples.  Also test partial instance
improvement, grouped multi-result improvement, imported improvement, and an
insoluble case whose diagnostic names the class constraint rather than an
internal equality.

### Verification

Run all FunctionalDependencies tests, existing class/instance/type-family
solver tests, runtime AST serialization, and the full `haskell/Types` subtree.

## Commit 5: Close generalization under FunDeps

Suggested description:

```text
Close fixed type variables under functional dependencies
```

### Generalization

Modify `TypeChecker::find_fixed_tvs` in
`src/computation/typecheck/binds.cc`.  After collecting initial fixed
metavariables from outer levels, restricted bindings, and equality handling,
call:

```cpp
fixed = close_wrt_fun_deps(*this, preds, std::move(fixed));
```

Ensure closure includes nominal equalities in both directions, class FunDeps,
transitive superclass FunDeps, kind variables, and successive constraints
where one dependency enables another.  Remove the obsolete
`closeWrtFunDeps = ???` comment.

Do not replace `get_quantifiable_preds` with dependency closure.  The former
chooses constraints that may appear in a generalized context; the latter only
enlarges the set of variables that cannot be quantified independently.

### Ambiguity behavior

Use the completed solver during existing explicit-signature and class-method
validity checks.  Do not add a separate syntax-only ambiguity checker.

Verify that a class method whose absent parameter is determined is accepted:

```haskell
class C a b | a -> b where
    empty :: a
```

The corresponding declaration without a dependency should retain the
compiler's existing ambiguity behavior.

### Tests

Create:

```text
tests/haskell/Types/FunctionalDependencies/Generalization
tests/haskell/Types/FunctionalDependencies/NestedGeneralization
tests/haskell/Types/FunctionalDependencies/TransitiveGeneralization
tests/haskell/Types/FunctionalDependencies/ClassMethodAmbiguity
tests/haskell/Types/FunctionalDependencies/NoDependencyAmbiguity
tests/haskell/Types/FunctionalDependencies/KindClosure
```

The nested test should capture an outer value in a local binding whose result
type is determined by a FunDep, and verify that the determined variable remains
monomorphic with the outer determinant.  The transitive test should use:

```haskell
C a b
D b c
```

with `a -> b` and `b -> c`, and verify that fixing `a` also fixes `c`.

### Documentation cleanup

- Remove the standalone Functional Dependencies item from `TODO/TODO.md`.
- Add a concise solver comment describing the evidence-free improvement model
  and ordering.
- Do not add compatibility aliases or retain an unused alternate solver path.

## Final verification

Build with:

```sh
ninja -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O
```

Run the new tests and affected compiler suite:

```sh
meson test -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O \
  --print-errorlogs \
  'bali-phy:bali-phy testsuite haskell/Types'
```

Run serialization and the required integration test:

```sh
meson test -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O \
  --print-errorlogs \
  'bali-phy:runtime AST serialization' \
  'bali-phy:bali-phy 5d +A 50'
```

Then run the complete suite:

```sh
meson test -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O \
  --print-errorlogs
```

Finally run `git diff --check` and inspect the jj series to confirm that:

- the five implementation statements remain separate commits;
- unrelated tracked work is not included;
- the separate `noqmwnrm` change remains untouched;
- no compatibility aliases or unused FunDep infrastructure remain.
