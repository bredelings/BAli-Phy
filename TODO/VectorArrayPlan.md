# Boxed vector and array migration plan

This is the reviewed, second-stage plan for moving the current zero-based
`Data.Array` implementation to a boxed `Data.Vector`, migrating its callers,
and then introducing a new bounds-aware `Data.Array` with the standard array
interface.

The current runtime has a separate limitation: sufficiently deep Haskell
recursion can exhaust the C++ stack.  Raising the stack limit can postpone that
failure, but fixing it belongs to the evaluator and is a different project.
This refactor must not claim to solve that problem, and it must not use a
100,000-element recursively generated list as an acceptance test.  Such a test
could fail before or during list production for reasons unrelated to vector or
array construction.

## Review decisions

- The existing `Array i e` is actually a zero-based boxed vector.  Its native
  builtins convert every index and size to `Int`, its bounds are always
  `(0,n-1)`, and its useful instances are for `Array Int`.
- Move that implementation to `Data.Vector` rather than wrapping or preserving
  the misleading API.
- Migrate all callers and delete the old `Data.Array` before adding the new
  array type.  Do not leave two meanings of `Data.Array` in parallel.
- Preserve element laziness and sharing.  The existing array stores registers
  for element computations; it does not require every element to be evaluated
  during construction.
- Implement `Data.Vector.fromList` as one dependent-list traversal.  Evaluate
  the list spine, but retain each head register without evaluating its value.
- Implement native list walks with explicit C++ loops, not recursive C++
  helper functions.  This avoids adding another source of stack growth, but it
  is not an end-to-end constant-stack guarantee because evaluating the Haskell
  list itself can recurse through the evaluator.
- Keep `Foreign.Vector.EVector` distinct initially.  Its `RVector` storage
  contains runtime values, whereas the new boxed vector retains registers for
  lazy Haskell values.  Do not pretend they are representation-compatible.
- Implement arbitrary `Ix` handling in Haskell.  Native array builders should
  receive only linear `(Int,e)` associations.
- Back the new `Data.Array` with `Data.Vector`.  Do not introduce a second
  boxed storage representation.
- Match current GHC array semantics: construction is strict in bounds, list
  spines, and association indices, but lazy in values; missing entries contain
  an undefined-element thunk; excess `listArray` input is ignored; and the last
  duplicate association wins.
- Use moderate semantic tests.  Establish linear construction by the native
  one-pass implementation and by removing repeated `!!`, not by a huge test
  whose result is dominated by the evaluator stack limit.

## Target types and module boundary

The public boxed vector type will be:

```haskell
module Data.Vector

data Vector a
```

The initial public interface will cover the standard names needed by current
callers and ordinary boxed-vector use:

```haskell
(!)       :: Vector a -> Int -> a
(!?)      :: Vector a -> Int -> Maybe a
empty     :: Vector a
singleton :: a -> Vector a
generate  :: Int -> (Int -> a) -> Vector a
replicate :: Int -> a -> Vector a
fromList  :: [a] -> Vector a
toList    :: Vector a -> [a]
length    :: Vector a -> Int
null      :: Vector a -> Bool
map       :: (a -> b) -> Vector a -> Vector b
imap      :: (Int -> a -> b) -> Vector a -> Vector b
slice     :: Int -> Int -> Vector a -> Vector a
take      :: Int -> Vector a -> Vector a
drop      :: Int -> Vector a -> Vector a
(++)      :: Vector a -> Vector a -> Vector a
```

`Data.Vector.Internal` will expose only the operations needed to implement
`Data.Array` efficiently:

```haskell
fromListNDefault :: Int -> a -> [a] -> Vector a
fromIndexedList  :: Int -> a -> [(Int,a)] -> Vector a
replaceIndexed   :: Vector a -> [(Int,a)] -> Vector a
accumIndexed     :: (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
```

The new array representation will be:

```haskell
type role Array nominal representational
data Array i e = Array !i !i !Int !(Vector e)
```

The separate strict lower bound, upper bound, and element count avoid
reconstructing or repeatedly inspecting bounds when indexing.  Elements remain
lazy inside the backing vector.

## Commit 1: Replace the zero-based array with boxed Data.Vector

Suggested description:

```text
Replace zero-based arrays with boxed vectors
```

This is one coherent migration commit: introduce the honest interface, move
the runtime implementation, update every caller, and delete the misleading
old module.

### Haskell vector module

Create `haskell/Data/Vector.hs` with `NoImplicitPrelude` and an explicit export
list.

- Declare abstract `Vector a`.
- Give the parameter a representational role.
- Import only the Prelude components needed for instances and definitions.
- Define the public operations listed above.
- Implement `map` and `imap` through `generate`, so they retain lazy element
  computations instead of converting through lists.
- Implement `take`, `drop`, `slice`, and `(++)` through native register-copying
  operations, not through `toList` and `fromList`.
- Add `Functor`, `Foldable`, `Traversable`, `Eq`, `Ord`, and `Show` instances
  with the conventional vector order.  Traversal and folds may use indexed
  loops; they must not first allocate an intermediate list.
- Remove the old `listArray'`, `array_to_vector`, `vectorToArray`, `mapnA`, and
  `removeElement` names instead of retaining compatibility wrappers.

### Native boxed-vector operations

Move the useful implementation from `src/builtins/Array.cc` into the existing
`src/builtins/Vector.cc` builtin module.  Keep boxed-vector helpers visibly
separate from the existing `RVector`/`EVector` helpers inside that file.

- Rename the runtime constructor tag from `Array` to `Vector`.
- Rename `mkArray` to a native `generate` operation.
- Rename `arraySize` and `getIndex` to boxed-vector length and indexing
  operations.
- Validate negative lengths and out-of-range indexes with exceptions rather
  than assertions.
- Retain generated element applications as environment registers.
- Add native slice and append operations that copy register references without
  forcing elements.
- Delete the obsolete native remove-element operation if no migrated caller
  needs it.
- Remove `src/builtins/Array.cc` and its Meson builtin target after all imports
  use the `Vector` builtin module.

### Linear `fromList`

Add a builtin `Vector:fromList` using the dependent-read operation API:

1. Obtain a USE edge for the list argument.
2. Use an explicit `while` loop to evaluate one list cell at a time.
3. Accept only `(:)` and `[]` constructors.
4. Append the register for the head constructor field without evaluating it.
5. Follow the tail using a dependent USE edge.
6. Reject a malformed list with a source-facing error.
7. Reject a vector length exceeding the Haskell `Int` range.
8. On `[]`, build one `Vector` constructor whose environment contains the
   collected element registers.

Do not call `length`, `!!`, or the `EVector` list conversion.  The C++ walker
must be iterative, while comments and tests must avoid claiming that producing
an arbitrarily deep Haskell list is constant-stack in the current evaluator.

### Caller migration

Update every module importing the old `Data.Array` for zero-based storage.
Apply these direct replacements:

```text
Array Int a  -> Vector a
mkArray n f  -> generate n f
listArray' x -> fromList x
numElements  -> length
elems        -> toList
```

Update at least the currently identified users in:

```text
haskell/BirthDeath.hs
haskell/Tree.hs
haskell/Forest.hs
haskell/Tree/Newick.hs
haskell/IModel.hs
haskell/Numeric/Matrix.hs
haskell/Data/Traversable.hs
haskell/Data/IntSet.hs
haskell/Bio/Alignment.hs
haskell/SModel/Markov.hs
haskell/SModel/MutSel.hs
haskell/SModel/Parsimony.hs
haskell/SModel/Likelihood/FixedA.hs
haskell/SModel/Likelihood/VariableA.hs
haskell/Control/DeepSeq.hs
haskell/Probability/Random.hs
haskell/Probability/Distribution/List.hs
haskell/Probability/Distribution/Categorical.hs
haskell/Probability/Distribution/PhyloAlignment.hs
haskell/Probability/Distribution/Tree/Coalescent.hs
haskell/Probability/Distribution/Tree/Yule.hs
haskell/Probability/Distribution/Tree/UniformTopology.hs
haskell/Probability/Distribution/Tree/Modifiable.hs
```

Where both boxed and numeric vectors are used, qualify imports so
`Data.Vector.Vector` is not confused with `Numeric.LinearAlgebra.Vector` or
`Foreign.Vector.EVector`.

For the two `Probability.Distribution.List` calls currently written as
`listArray n_all (sortOn f xs')`, use `fromList (sortOn f xs')`; those lists
already contain the complete collection, so the count should not remain an
independent source of truth.

Delete `haskell/Data/Array.hs` only after searches show no remaining old
imports or names.

### Tests

Create:

```text
tests/haskell/Data/Vector/Construction
tests/haskell/Data/Vector/Generate
tests/haskell/Data/Vector/LazyElements
tests/haskell/Data/Vector/IndexErrors
tests/haskell/Data/Vector/Slicing
tests/haskell/Data/Vector/Instances
```

Use `NoImplicitPrelude` where possible.  Test empty, singleton, and a moderate
finite list; malformed and out-of-range access; laziness of an unselected
element; generate/map/imap; slice/take/drop/append; and the standard instances.
Do not add a 100,000-element regression test.

### Verification

Run the build, all new vector tests, runtime AST serialization, and the tests
for every migrated high-use module.  Run `5d +A 50` because the migration
touches model and likelihood code.

## Commit 2: Introduce bounds-aware Data.Array construction and indexing

Suggested description:

```text
Add bounds-aware immutable arrays
```

### Internal vector builders

Create `haskell/Data/Vector/Internal.hs`.  Add native operations in
`src/builtins/Vector.cc` for `fromListNDefault` and `fromIndexedList`.

`fromListNDefault n missing xs` must:

- Reject negative `n`.
- Allocate exactly `n` result slots initialized to the `missing` register.
- Walk at most `n` list cells iteratively.
- Store head registers without evaluating values.
- Leave remaining slots pointing to `missing` when the list is short.
- Return immediately after `n` elements without evaluating an excess tail.

`fromIndexedList n missing associations` must:

- Reject negative `n`.
- Initialize exactly `n` slots to the shared `missing` register.
- Walk the association spine iteratively.
- Evaluate each pair constructor and its `Int` index.
- Preserve the value field as an unevaluated register.
- Reject indexes outside `[0,n-1]`.
- Overwrite an earlier slot when the same index occurs again, making the last
  association win.

These private builders are justified because they construct the final storage
in one allocation and are immediately used by `Data.Array`; do not expose them
from public `Data.Vector`.

### Data.Ix corrections

Update `haskell/Data/Ix.hs` as required by standard arrays:

- Make `rangeSize (lower,upper)` return zero for an empty range.
- Preserve tuple range order and row-major tuple indexing.
- Check index membership before calculating or using a linear offset.
- Detect negative or overflowing computed sizes before native allocation.

Add a private `safeIndex` in `Data.Array` that checks `inRange` and verifies the
computed offset against the stored element count.

### Core Data.Array interface

Recreate `haskell/Data/Array.hs` with `NoImplicitPrelude`, an explicit export
list, and the new four-field representation.  Export `Data.Ix` as the standard
module does.

Implement:

```haskell
array       :: Ix i => (i,i) -> [(i,e)] -> Array i e
listArray   :: Ix i => (i,i) -> [e] -> Array i e
(!)         :: Ix i => Array i e -> i -> e
bounds      :: Array i e -> (i,i)
numElements :: Array i e -> Int
indices     :: Ix i => Array i e -> [i]
elems       :: Array i e -> [e]
assocs      :: Ix i => Array i e -> [(i,e)]
```

Define one non-inlined polymorphic `undefinedArrayElement` thunk with the error
text `(Array.!): undefined array element`.

Implement `array` by mapping each source association from `(i,e)` to
`(safeIndex bounds i,e)` and passing the lazy mapped list to
`fromIndexedList`.  This mapping deliberately remains in Haskell because
`Ix.index` is a typeclass method.  The native consumer should consume the
mapped list incrementally; it need not retain the entire intermediate list.

Implement `listArray` with `fromListNDefault`.  It must ignore excess input
without evaluating it and leave missing elements undefined.

Indexing must use the stored bounds and count to calculate a vector offset.
Empty reverse bounds remain observable through `bounds` even though the
backing vector is empty.

### Instances

Add the standard immutable-array behavior for:

- `Functor (Array i)` without requiring `Ix i`;
- `Foldable (Array i)` over element order;
- `Traversable (Array i)` while preserving bounds;
- `Eq` and `Ord` with the necessary `Ix` and element constraints;
- `Show` in `array bounds associations` form.

Implement mapping and traversal over the backing vector, not through repeated
association-list reconstruction.

### Tests

Create:

```text
tests/haskell/Data/Array/Bounds
tests/haskell/Data/Array/ListArray
tests/haskell/Data/Array/Associations
tests/haskell/Data/Array/MissingElement
tests/haskell/Data/Array/Recursive
tests/haskell/Data/Array/TupleIndex
tests/haskell/Data/Array/Instances
```

Test:

- nonzero and negative integer bounds;
- tuple indices and row-major ordering;
- empty reverse bounds;
- short and excess `listArray` input;
- missing elements failing only when indexed;
- out-of-range associations failing during construction;
- last duplicate association winning;
- recursive arrays whose values refer to earlier array elements;
- bounds preserved by mapping and traversal.

Keep collection sizes moderate so evaluator stack behavior does not obscure
array semantics.

### Verification

Run the build, all `Data.Ix`, `Data.Vector`, and `Data.Array` tests, runtime AST
serialization, and `git diff --check`.

## Commit 3: Add standard immutable-array updates and accumulation

Suggested description:

```text
Complete immutable array update operations
```

### Native indexed updates

Add `replaceIndexed` and `accumIndexed` to `Data.Vector.Internal` and
`src/builtins/Vector.cc`.

`replaceIndexed base associations` must copy the base vector's element
registers once, then iteratively consume associations and overwrite slots.
Values remain lazy and the last duplicate wins.

`accumIndexed combine base associations` must copy base registers once and
process associations in source order.  For every association, build the
application `combine old new` using the current slot register and the new value
register, evaluate that application to weak head normal form, and make the
resulting register the slot used by later duplicate associations.  Thus
accumulation is strict in every result of `combine`, as GHC's `accum` and
`accumArray` are, but does not force unrelated initial elements.

Both operations must validate every linear index and must not rebuild through
`toList` and `fromList`.

### Public operations

Implement the remaining common immutable-array interface:

```haskell
(//)       :: Ix i => Array i e -> [(i,e)] -> Array i e
accum      :: Ix i => (e -> a -> e) -> Array i e -> [(i,a)] -> Array i e
accumArray :: Ix i => (e -> a -> e) -> e -> (i,i) -> [(i,a)] -> Array i e
ixmap      :: (Ix i, Ix j) => (i,i) -> (i -> j) -> Array j e -> Array i e
```

- Convert public indices with the same `safeIndex` used by `array`.
- Preserve original bounds for `(//)` and `accum`.
- Initialize `accumArray` with `Vector.replicate` and then use
  `accumIndexed`.
- Implement `ixmap` using `Vector.generate` over the target range, with one
  target-index enumeration and no repeated immutable updates.
- Add any conventional fixities matching the standard interface.

Do not add mutable `ST` or IO arrays in this project.  Their representation
and evaluation model require a separate design analysis.

### Tests

Create:

```text
tests/haskell/Data/Array/Update
tests/haskell/Data/Array/Accum
tests/haskell/Data/Array/AccumArray
tests/haskell/Data/Array/IxMap
```

Cover duplicate updates, association order, preserved bounds, invalid indexes,
empty arrays, and tuple-index accumulation.

### Cleanup

Search for and remove:

- old `Array:mkArray`, `Array:getIndex`, and `Array:arraySize` symbols;
- the old runtime `Array` constructor tag used as a vector;
- `listArray'`, `mapnA`, and array/vector conversion wrappers;
- list round trips in vector and array construction or update paths;
- now-unused imports of `Foreign.Vector` from migrated boxed-container code.

Do not attempt to merge `EVector` and boxed `Vector` as cleanup.  That requires
its own representation and strictness analysis.

## Final verification

Build with:

```sh
ninja -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O
```

Run all new vector and array tests and the complete Haskell-language suite:

```sh
meson test -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O \
  --print-errorlogs \
  'bali-phy:bali-phy testsuite haskell/Data/Vector' \
  'bali-phy:bali-phy testsuite haskell/Data/Array'
```

Use the exact names reported by `meson test --list` if the generated subtree
aggregate names differ.

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

- the vector migration, core array implementation, and array updates remain
  separate commits;
- no old pseudo-array API or compatibility wrappers remain;
- the boxed vector and `EVector` representations have not been conflated;
- unrelated tracked and untracked user files remain untouched;
- the evaluator's deep-recursion stack limitation has not been hidden or
  incorrectly claimed as fixed by this work.
