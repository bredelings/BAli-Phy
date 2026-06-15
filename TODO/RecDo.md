# RecursiveDo implementation plan

Goal: implement `RecursiveDo` with a phase split similar to GHC:

* parse `rec` and `mdo`;
* segment `mdo` during renaming;
* keep `RecStmt` through typechecking;
* typecheck `RecStmt` using the `mfix` shape;
* lower `RecStmt` to explicit `mfix` during desugaring.

The important architectural change is that the renamer should stop translating
`rec` directly to `mfix`.  Renaming should discover binders, free variables, and
recursive segments.  Typechecking should check the generated `return`, `mfix`,
and bind operators.  Desugaring should construct the explicit `mfix` expression.

## Current state

Relevant files:

* `src/computation/parser/parser.y`
  * Parses `mdo` expressions as `Hs::MDo`.
  * Parses `rec` statements as `Hs::RecStmt`.

* `src/computation/haskell/haskell.H`
  * Defines `RecStmt`, `Do`, and `MDo`.
  * `RecStmt` currently only stores `stmts` and an optional `mfixOp`.

* `src/computation/rename/stmt.cc`
  * Currently rewrites explicit `rec` blocks to a generated `mfix` bind during
    renaming.

* `src/computation/rename/expression.cc`
  * Has notes for `mdo` segmentation, but the segmentation is not implemented.

* `src/computation/typecheck/quals.cc`
  * Aborts if a `RecStmt` reaches typechecking.

* `src/computation/desugar/desugar.cc`
  * Desugars ordinary `do` statements to uses of `>>=`, `>>`, `let`, and failure
    handling.
  * Does not currently handle `RecStmt`.

## Milestone 1: explicit `rec` as a complete vertical slice

This milestone should preserve the existing explicit `rec` behavior while moving
the `mfix` translation from renaming to desugaring.

### 1. Add tests for current and target explicit `rec` behavior

Start with tests under `tests/haskell/RecursiveDo`.

Add cases for:

* basic explicit `rec`;
* a forward reference inside `rec`;
* a binder used after the `rec` block;
* binders of different types;
* a binder with an explicit signature, if local signatures are supported there;
* duplicate binders inside `rec`;
* `rec` without `RecursiveDo`, expected to fail.

The existing `tests/haskell/RecursiveDo/Basic` test should continue to pass
throughout the work.

### 2. Add extension gating early

Reject `rec` and `mdo` unless `LangExt::RecursiveDo` is enabled.

Do this in the renamer rather than relying only on parser behavior, so error
messages can say that the syntax requires `RecursiveDo`.

Expected rules:

* `rec` inside a `do` block requires `RecursiveDo`.
* `mdo` requires `RecursiveDo`.
* Existing source using the pragma should continue to work.

### 3. Extend the `RecStmt` AST

`RecStmt` needs to carry enough information from rename through typecheck into
desugar.

The exact representation should follow local conventions, but it needs fields
for:

* the nested statements;
* recursive binders: variables used before they are bound inside the block;
* later binders: variables used after the block;
* the syntax operators for bind, return, and mfix;
* any typechecked wrappers/dictionaries needed by desugaring.

Be explicit about where typechecked operator information lives.  If `Hs::Var`
wrappers reliably preserve dictionaries, using `Hs::Var` fields may be enough.
If not, use a typed expression/syntax-expression field rather than plain names.

Use deterministic ordering for binder lists.  Tuple construction must not depend
on unstable set ordering.

For the first explicit-`rec` slice, it is acceptable to set both recursive and
later binders to all binders in the `rec` block.  This is conservative for
monadic `mfix`, but it should be documented as a bootstrap step and refined
later.

### 4. Stop rewriting explicit `rec` in the renamer

Replace the current `rename_rec_stmt` behavior in `rename/stmt.cc`.

The new behavior should:

1. Check that `RecursiveDo` is enabled.
2. Collect binders for all statements in the `rec` block.
3. Reject duplicate binders.
4. Rename RHSs with all rec binders in scope.
5. Attach bind, return, and mfix operators to the `RecStmt`.
6. Fill conservative binder metadata:
   * `rec_ids = all binders`;
   * `later_ids = all binders`.
7. Return the block binders as the variables bound by the statement.

At the end of this step, explicit `rec` should survive renaming as `RecStmt`.

### 5. Typecheck `RecStmt`

Replace the abort in `typecheck/quals.cc`.

Typecheck:

```haskell
do { rec { stmts }; rest }
```

as if it were:

```haskell
do
  tuple <- mfix (\ ~tuple -> do
    stmts
    return tuple)
  rest
```

Implementation outline:

1. Build the tuple binder list from `rec_ids union later_ids`.
2. Allocate fresh type variables for tuple elements.
3. Extend the local environment with the tuple binders before checking the
   recursive body.
4. Typecheck the inner `stmts`.
5. Typecheck the generated `return tuple`.
6. Typecheck `mfix` at the expected function shape.
7. Typecheck the bind from the `mfix` result to the following statements.
8. Store the checked/wrapped bind, return, and mfix operators on the `RecStmt`.

This is the riskiest step.  The existing `PatQual` path adds binders only after
checking the RHS, but `RecStmt` needs recursive binders in scope while checking
RHSs inside the block.

### 6. Desugar explicit `RecStmt`

Add a `RecStmt` case to the `Hs::Do` desugaring in `desugar/desugar.cc`.

Lower:

```haskell
do { rec { stmts }; rest }
```

to a normal generated do block equivalent to:

```haskell
do
  tuple_pat <- mfix (\ ~rec_tuple_pat -> do
    stmts
    return tuple_exp)
  rest
```

Details:

* Use the typechecked bind, return, and mfix operators from the `RecStmt`.
* The generated outer tuple pattern should be infallible.
* The lambda argument pattern should be lazy.
* The generated inner do body should end with an explicit `return tuple`.
* Then reuse the existing ordinary `do` desugaring.

After this milestone, explicit `rec` should work without any renamer-side
translation to `mfix`.

## Milestone 2: conservative `mdo`

This milestone implements correct `mdo` behavior without minimal segmentation.
It should reuse the explicit `RecStmt` path.

### 1. Rename `mdo` in recursive scope

For:

```haskell
mdo { s1; ...; sn }
```

rename all binders from the `mdo` statements in a recursive scope:

1. Check that `RecursiveDo` is enabled.
2. Rename all statement LHSs first.
3. Reject duplicate binders across the entire `mdo`.
4. Rename RHSs with all `mdo` binders in scope.

### 2. Lower `MDo` to `Do` during renaming

Do not keep `MDo` after rename.

As a conservative first translation:

```haskell
mdo { s1; ...; sn }
```

becomes:

```haskell
do { rec { s1; ...; s(n-1) }; sn }
```

For an empty or malformed block, use the same validation rules as ordinary `do`.

This translation may introduce unnecessary `mfix` use, but it gives a correct
baseline and keeps typecheck/desugar focused on `Do` plus `RecStmt`.

### 3. Add conservative `mdo` tests

Add tests for:

* basic `mdo` with a forward reference;
* `mdo` with a final expression;
* duplicate binders across an `mdo`;
* `mdo` without `RecursiveDo`, expected to fail.

## Milestone 3: minimal `mdo` segmentation

This milestone improves `mdo` so it only creates `RecStmt`s where required.

### 1. Add a segment representation

Introduce a small internal representation in the renamer, for example:

```cpp
struct RecSegment
{
    bound_var_info defs;
    std::set<std::string> uses;
    std::set<std::string> fwds;
    Hs::LExp stmt;
};
```

The exact types can differ, but keep the concepts explicit:

* `defs`: binders introduced by the statement;
* `uses`: free variables used by the statement;
* `fwds`: variables used before they are bound later in the block.

### 2. Compute forward references

After recursive-scope renaming:

1. Build one segment per statement.
2. Walk from right to left.
3. Track definitions available later.
4. Set each segment's `fwds` to variables it uses that are defined later.

### 3. Group ordered segments

Use GHC's invariant: do not reorder statements.

Group enough later segments into the current segment so that no resulting
segment uses a variable bound by a later segment.

Emit:

* the original statement if the segment is a singleton and has no forward
  references;
* a `RecStmt` if the segment has forward references or contains multiple
  statements that must remain grouped.

### 4. Refine `rec_ids` and `later_ids`

For each generated `RecStmt`:

* `rec_ids`: variables used before they are bound inside the segment;
* `later_ids`: variables defined by the segment and used after it.

The mfix tuple should contain `rec_ids union later_ids`.

This should replace the bootstrap "all binders" approximation where possible.

### 5. Add minimal-segmentation tests

Add tests proving:

* `mdo` with no forward references does not require `MonadFix`;
* `mdo` with one recursive group creates one `RecStmt`;
* `mdo` with multiple independent recursive groups works;
* variables used after a recursive segment are preserved through `later_ids`;
* variables only needed internally are not unnecessarily exposed after the
  segment.

## Cleanup

After all milestones:

* Remove stale comments saying `rec` is desugared in the renamer.
* Rename helpers so phase ownership is clear:
  * renamer helpers should talk about recursive-scope renaming and segmentation;
  * desugarer helpers should talk about lowering to `mfix`.
* Ensure `MDo` does not reach typechecking unless intentionally supported.
* Ensure untyped `RecStmt` does not reach desugaring.
* Update `TODO.md` if the high-level RecursiveDo audit item is satisfied.

## Recommended implementation order

1. Add explicit `rec` tests and extension-gating tests.
2. Extend `RecStmt` metadata.
3. Stop renamer-side `mfix` rewriting for explicit `rec`.
4. Implement `RecStmt` typechecking.
5. Implement `RecStmt` desugaring.
6. Verify explicit `rec` tests.
7. Implement conservative `mdo`.
8. Verify conservative `mdo` tests.
9. Implement minimal `mdo` segmentation.
10. Verify full RecursiveDo test suite.

