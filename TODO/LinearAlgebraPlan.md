# Linear algebra improvement plan

This is the reviewed, second-stage plan for bringing BAli-Phy's dense
linear-algebra support closer to `hmatrix`, eliminating unnecessary boxed
numeric-vector conversions, and using Eigen for the native storage and
arithmetic of Haskell numerical vectors and matrices.

The target is a coherent dense `Int`/`Double` subset of `hmatrix`, not its
Complex, sparse, random, modular, or file-I/O facilities.

Implementation status: complete through commit 20.  The final invariant
searches and full verification described below passed when the plan was
completed.

## Review decisions

The plan incorporates the following decisions from its review:

- Use Eigen multiplication unconditionally.  A temporary GCC 16 benchmark
  showed Eigen winning by about `4x4`; the occasional nanosecond-scale win
  for the three-loop kernel on smaller results does not justify a dispatcher,
  policy, and second implementation.
- `(><)` truncates excess input without evaluating the unused tail, but
  rejects short input.
- `fromRows`, and therefore `fromLists`, expands rows of length one.  Other
  incompatible row lengths remain errors.
- Optimized matrix-chain association applies to `mconcat`, `sconcat`, and
  explicit `optimiseMult`, not an arbitrary sequence of already-binary
  `(<>)` applications.
- Retain the general-purpose C++ `matrix<T>` allocation class.  Its 369 uses
  across 49 files are predominantly alignment indices, DP tables, counts,
  parsimony scratch space, and other two-dimensional storage rather than
  linear algebra.
- Use `DenseMatrix<T>` only for runtime values that inhabit Haskell
  `Numeric.LinearAlgebra.Matrix`, and update their direct native consumers.
  The representation boundary follows the Haskell semantic type rather than
  every occurrence of the C++ spelling `matrix<T>`.
- Keep Eigen aliases in a numerical-storage header separate from
  `util/matrix.H`.  General two-dimensional storage should not acquire an
  Eigen dependency merely because numeric builtins use Eigen.
- Delete the unused `homology_matrix`, but do not change `index_matrix` to
  composition as preparation for an Eigen migration.  That refactor no
  longer provides a benefit required by this plan.
- Keep `EVector`/`RVector` for heterogeneous or structural runtime
  collections.  Use native `Vector` when the contents are uniformly numeric
  and used numerically.
- Use a small module split patterned after `hmatrix` so that data operations,
  standard instances, and public algorithms do not accumulate in one module.
- A regular abstract `Vector a` plus the `Element` storage class is
  sufficient.  A data family is not needed.
- Do not add `Foldable` instances for native vectors or matrices.  Like
  `hmatrix`, their element access requires a storage constraint, which the
  standard `Foldable` methods cannot provide.

## Target representation

| Haskell type | Native representation |
| --- | --- |
| `Vector Double` | `Box<DenseVector<double>>` |
| `Vector Int` | `Box<DenseVector<int>>` |
| `Matrix Double` | `Box<DenseMatrix<double>>` |
| `Matrix Int` | `Box<DenseMatrix<int>>` |
| `EVector a` | Existing boxed `RVector` for general runtime values |
| General C++ two-dimensional storage | Existing `matrix<T>` |

The native numerical definitions will live in
`src/util/include/util/dense-matrix.H`:

```c++
template<class T>
using DenseVector = Eigen::Matrix<T, Eigen::Dynamic, 1>;

template<class T>
using DenseMatrix =
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
```

Matrices remain explicitly row-major to preserve the current flattened order
and raw-pointer consumers.  The existing `Matrix = matrix<double>` alias and
the `matrix<T>` and `matrix3<T>` classes in `util/matrix.H` remain available
to non-runtime C++ code.

## Repository and test discipline

- Use the out-of-source build at
  `/home/bredelings/Devel/bali-phy/build/gcc-16-debug-O`.
- Preserve unrelated tracked and untracked user files.
- Put each independently meaningful statement below in its own commit.
- For a bug in a recent commit, create an empty child of the introducing
  commit, make and verify the correction there, then squash it into the
  introducing commit.
- New Haskell-language tests should use `NoImplicitPrelude` where practical.
- Land a feature's tests with its implementation.  If a test must precede an
  unimplemented feature, mark it `xfail`.
- After every commit, build and run the directly affected linear-algebra
  tests.  After native representation or numerical-path changes, also run the
  runtime serialization test and the 5d +A test.

The standard commands are:

```sh
ninja -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O

meson test -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O \
  --print-errorlogs \
  "bali-phy:bali-phy testsuite haskell/Numeric/LinearAlgebra"

meson test -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O \
  --print-errorlogs \
  "bali-phy:runtime AST serialization" \
  "bali-phy:bali-phy 5d +A 50"
```

## Phase 1: Correct the recent construction commit

These are corrections to `vrvotplt` (`Construct matrices by native element
type`).  Use the existing empty child commit, test each correction in a
temporary child, and squash it into that parent before starting the new
feature commits.

### Correction 1: Truncate excess matrix input

Modify `src/builtins/Matrix.cc`:

- Consume exactly `rows * columns` list cells.
- Return immediately after the last required cell.
- Do not evaluate or inspect the remaining tail.
- Preserve errors for short input, negative dimensions, malformed lists, and
  dimension overflow.
- Add successful finite-excess and infinite-input tests.
- Keep `ShortInput` as a failing test.

The infinite test should evaluate `(2 >< 3) [1..]`, proving that the unused
tail is not forced.

### Correction 2: Expand singleton rows in fromLists

Modify `haskell/Numeric/LinearAlgebra.hs`:

- Let the result column count be the maximum row length.
- Accept each row whose length is either that maximum or one.
- Replicate singleton rows to the maximum length.
- Reject all other incompatible lengths.
- Change the existing ragged-row failure to use genuinely incompatible
  lengths, such as two and three.
- Add successful singleton-row tests for `Int` and `Double`.

## Phase 2: Introduce native Vector and the module structure

### Commit 1: Add native numeric vectors

Create:

- `haskell/Numeric/LinearAlgebra/Data.hs`
- `haskell/Numeric/Matrix.hs`
- `haskell/Numeric/Vector.hs`

Refactor `Numeric.LinearAlgebra` into the public reexport and algorithm
module:

- `Numeric.LinearAlgebra.Data`: types, construction, conversion, and
  indexing.
- `Numeric.Matrix`: standard `Matrix` instances.
- `Numeric.Vector`: standard `Vector` instances.
- `Numeric.LinearAlgebra`: reexport data functionality and define products
  and algorithms.

In the data module:

- Define nominal-role abstract `Vector a` and `Matrix a`.
- Retain `Element` as the storage-selection class.
- Give `Element Int` and `Element Double` native vector and matrix
  constructors.
- Define `R = Double` and `I = Int`.

Add the initial vector interface:

```haskell
fromList :: Element a => [a] -> Vector a
toList   :: Element a => Vector a -> [a]
(|>)     :: Element a => Int -> [a] -> Vector a
vector   :: [Double] -> Vector Double
range    :: Int -> Vector Int
idxs     :: [Int] -> Vector Int
```

Semantics:

- `fromList` consumes the complete finite list.
- `n |> xs` consumes exactly `n` cells, ignores excess input, and errors
  on short input.
- `toList` builds the Haskell list by indexing the native vector.  It must
  not construct an intermediate `RVector`.

Add a minimal, immediately used `Container` abstraction:

```haskell
type family IndexOf c

class Element e => Container c e where
    size    :: c e -> IndexOf c
    atIndex :: c e -> IndexOf c -> e
    konst   :: e -> IndexOf c -> c e
    scalar  :: e -> c e
    sumElements :: c e -> e
```

Use:

```haskell
type IndexOf Vector = Int
type IndexOf Matrix = (Int, Int)
```

Add native vector visitation and construction to `src/builtins/Matrix.cc`,
while leaving generic `EVector` operations in `src/builtins/Vector.cc`.
Add `Show` and exact `Eq` support for native vectors.  Retain `Element`
constraints on conversions that read or reinterpret native storage.

Move `sumElements` into `Container` immediately and use it to replace the
only pre-existing use of `Foldable Matrix` in `Markov.relativeFlux`.  Remove
the constrained `Foldable` instance without changing that calculation.

Add these conversions immediately so the new representation is used:

```haskell
flatten  :: Element a => Matrix a -> Vector a
reshape  :: Element a => Int -> Vector a -> Matrix a
asRow    :: Element a => Vector a -> Matrix a
asColumn :: Element a => Vector a -> Matrix a
```

Remove the current matrix-specific `toList`; matrix callers become
`toList . flatten`.

Tests cover both scalar representations, empty vectors, explicit-length
truncation, short input, indexing, flattening, and reshaping.

### Commit 2: Adopt hmatrix core names

Rename the current public operations and update every Haskell caller:

- `nrows` -> `rows`
- `ncols` -> `cols`
- `transpose` -> `tr`
- `getElem i j m` -> `atIndex m (i,j)`
- `identity` -> `ident`
- `scaleMatrix` -> `scale`
- `zero r c` -> `konst 0 (r,c)`

Remove the old names rather than retaining wrappers.  Add an explicit export
list and delete the commented Data.Matrix API inventory.

Update at least:

- `haskell/Markov.hs`
- `haskell/SModel/Markov.hs`
- `haskell/SModel/MarkovModulated.hs`
- the PhyloCTMC modules
- the Numeric.LinearAlgebra tests

## Phase 3: Install Eigen storage at the numerical runtime boundary

### Commit 3: Remove unused homology_matrix

Delete `homology_matrix` and its two out-of-line methods.  The audit found no
users outside its declaration and definitions.  Build the complete tree to
confirm the experimental deletion.

The unfinished `Make index matrix own its cells` child was started only as
preparation for deleting `matrix<T>`.  Abandon that child before continuing;
do not squash it into the verified `homology_matrix` deletion.

### Correction to Commit 1: Isolate Eigen numerical storage

Correct the recent native-vector commit using the child-and-squash workflow:

- Create `src/util/include/util/dense-matrix.H` containing `DenseVector<T>`
  and the explicit row-major `DenseMatrix<T>` alias.
- Move `DenseVector<T>` out of `util/matrix.H` and remove the Eigen include
  from that generic storage header.
- Remove the global `add_project_dependencies(eigen, ...)` setting.
- Include `dense-matrix.H` only from runtime and numerical code that uses the
  aliases; add a target-level Eigen dependency only where one is missing.
- Build the complete tree to verify that ordinary alignment and tool targets
  no longer depend on Eigen through `matrix.H`.

The new header is immediately used by the already-landed native vectors and
by the following matrix commits; it is not parallel unused infrastructure.

### Commit 4: Use Eigen storage for Matrix Int runtime values

Change only objects that inhabit Haskell `Matrix Int` from
`Box<matrix<int>>` to `Box<DenseMatrix<int>>`:

- construction, visitation, indexing, display, conversion, and arithmetic in
  `src/builtins/Matrix.cc` and `src/computation/object.{H,cc}`;
- parsimony cost matrices and their direct consumers in
  `src/builtins/Parsimony.cc`, `src/tools/parsimony.{H,cc}`,
  `src/tools/parsimony2.{H,cc}`, and `src/substitution/parsimony.cc`;
- pair-HMM transition-count values in `src/builtins/Alignment.cc` and the
  directly connected count/probability functions.

Use `DenseMatrix<int>` for cost or count values that cross this Haskell
boundary.  Retain `matrix<int>` for alignment index grids, DP tables, and
parsimony scratch matrices.  Do not add an implicit conversion between the
two representations.

This search must be empty.  The current matches are all values that inhabit
Haskell `Matrix Int`; unrelated internal integer grids are not boxed:

```sh
rg 'Box<matrix<int>>' src
```

Run the complete build, Numeric.LinearAlgebra and parsimony tests, runtime
serialization, and 5d +A.

### Commit 5: Use Eigen storage for Matrix Double runtime values

Change objects that inhabit Haskell `Matrix Double` from `Box<Matrix>` to
`Box<DenseMatrix<double>>`.  Update all direct runtime consumers together so
no object is interpreted using two native representations:

- `src/builtins/{Matrix,SModel,Likelihood,LikelihoodSEV,SMC}.cc`;
- `src/substitution/{ops,likelihood,likelihoodSEV}.{H,cc}`;
- `src/models/parameters.{H,cc}`;
- `src/math/exponential.{H,cc}`;
- `src/computation/object.{H,cc}`.

Use `.data()`, `.rows()`, and `.cols()` only on these Eigen-backed runtime
values.  Preserve the existing row-major raw-pointer likelihood kernels.
Leave the global C++ `Matrix = matrix<double>` alias and unrelated local
`matrix<double>` distance, plotting, and count grids unchanged.

After this commit, the Matrix builtin visitor accepts only
`Box<DenseMatrix<int>>` and `Box<DenseMatrix<double>>`.  These searches must
have no matches for Numeric.LinearAlgebra runtime values:

```sh
rg 'Box<Matrix>|Box<matrix<(int|double)>>' \
  src/builtins src/substitution src/models src/math
```

Run the complete build, runtime serialization, Numeric.LinearAlgebra,
Markov/SModel/likelihood tests, and 5d +A.

### Commit 6: Remove copies on runtime Matrix-to-Eigen paths

Make Eigen-native algorithms accept `DenseMatrix<double>`,
`DenseVector<double>`, `Eigen::Ref`, or generic Eigen expressions directly.
Delete `fromEigen` if it remains unused and remove `toEigen` calls whose
source is already `DenseMatrix<double>`.

For a remaining legacy `matrix<double>` caller, use a row-major `Eigen::Map`
over its existing storage and call the canonical Eigen implementation.  Mark
that adapter with a brief `NOTE` explaining the legacy caller and the
condition for removing it.  Do not copy merely to cross this boundary, and
do not migrate the legacy grid solely to delete the adapter.

## Phase 4: Make arithmetic hmatrix-compatible and fast

### Commit 7: Use Eigen for matrix multiplication

Replace the `DenseMatrix` implementation of `multiply_matrices` with
unconditional Eigen evaluation:

```c++
result.noalias() = left * right;
```

Do the same for matrix-vector and vector-matrix products.  Do not retain the
old three-loop path for Haskell numerical matrices or introduce a cutoff.
This does not alter algorithms over the general `matrix<T>` container.

Benchmark without adding a permanent benchmark framework:

- `Double` and `Int`
- square sizes 4, 20, and 61
- rectangular products `10x15 * 15x20`, `15x20 * 20x5`, and
  `20x5 * 5x10`
- median of at least ten optimized-build runs

A regression greater than 10% for the 4, 20, or 61 square cases blocks the
commit and is investigated as an Eigen expression or storage-order problem;
it does not trigger a second multiplication implementation.

### Commit 8: Use elementwise Num operations with broadcasting

Change `Num (Vector a)` and `Num (Matrix a)` to match `hmatrix`:

- `+`, `-`, and `*` are elementwise.
- Unary operations preserve the native representation.
- Numeric literals create singleton containers.
- A vector of length one broadcasts to the other vector length.
- Matrix dimensions broadcast independently when either extent is one.
- Incompatible non-singleton dimensions are errors.
- Expanded operands are not materialized.

Add one internal `conform_dimension` operation in `Matrix.cc`.  It is used
immediately by vector and matrix addition, subtraction, multiplication, and
later division.

Remove `%*%` and update internal users to `(*)`.

Tests cover scalar, row, column, row-plus-column, and vector broadcasting,
incompatible dimensions, and both supported scalar representations.

### Commit 9: Add hmatrix product operators

Implement:

```haskell
dot   :: Vector a -> Vector a -> a
(<.>) :: Vector a -> Vector a -> a
(#>)  :: Matrix a -> Vector a -> Vector a
(<#)  :: Vector a -> Matrix a -> Vector a
(<>)  :: Matrix a -> Matrix a -> Matrix a
outer :: Vector a -> Vector a -> Matrix a
```

Use:

```haskell
infixr 8 <>
```

Place the `Semigroup (Matrix a)` instance in `Numeric.Matrix`, separate
from the public operator definition.  Binary semigroup composition scales
when either operand is `1x1`, otherwise performs conformable matrix
multiplication, and rejects incompatible shapes.

### Commit 10: Optimize matrix multiplication chains

Implement the standard `O(n^3)` matrix-chain dynamic program privately in
`Numeric.Matrix`.

Requirements:

- Validate the complete dimension chain before multiplying.
- Use `Integer` for estimated scalar-operation costs.
- Record the cheapest split for every subchain.
- Evaluate the recorded tree with the native `(<>)`.
- Combine all `1x1` matrices into one scalar and apply it once.
- Override both `sconcat` and `mconcat`.
- Define `mempty` as the singleton matrix containing one.
- Export `optimiseMult` as the optimized `mconcat`.

Test empty, singleton, scalar-containing, valid multi-matrix, and invalid
chains.  Benchmark the standard example whose naive cost is 4500 scalar
multiplications and optimized cost is 2750.

## Phase 5: Remove numeric use of RVector

### Commit 11: Use Vector in the Markov and eigensystem path

Change frequency vectors from `EVector Double` to `Vector Double` in:

- `haskell/EigenExp.hs`
- `haskell/Markov.hs`
- corresponding functions in `src/builtins/Matrix.cc`
- `src/builtins/SModel.cc`
- `src/math/exponential.H`
- `src/math/exponential.cc`

This includes:

- `getEigensystemRaw`
- `lExpRaw`
- `equilibriumLimit`
- `checkStationary`
- `checkReversible`
- `flow`
- CTMC starting and equilibrium frequencies
- the stored frequency field in `Markov`

C++ numerical functions accept
`Eigen::Ref<const DenseVector<double>>` where appropriate.  No
`std::vector<double>` intermediate remains.

Run the Markov/SModel tests and 5d +A.

### Commit 12: Use Vector for substitution-model numeric arrays

Migrate flat numeric vectors in `haskell/SModel/*.hs` and their
`SModel.cc` implementations:

- equilibrium frequencies
- exchangeabilities
- mutation-selection fitness arrays
- codon and doublet frequency arrays
- empirical frequency tables
- weighted frequency operations

For nested values, retain `EVector` only as the outer structural container
and use `EVector (Vector Double)` for numeric children.

### Commit 13: Remove blanket RVector-to-double conversion

Migrate the remaining explicit `RVector -> std::vector<double>` sites in:

- `Distribution.cc`
- `PopGen.cc`
- `SMC.cc`

When the public Haskell API should remain list-based, retain that public
signature and convert with `fromList` or `toList` in the small Haskell
wrapper.  The raw foreign signature uses `Vector Double`.

Then delete from `src/computation/runtime/ast.{H,cc}`:

- `RVector(const std::vector<double>&)`
- `RVector::operator std::vector<double>()`

Do not remove the ability of a general `RVector` to contain `Double`
expressions; remove only the blanket native conversion escape hatch.

This check must produce no matches:

```sh
rg 'operator vector<double>|\(vector<double>\).*RVector' src
```

## Phase 6: Fill out the dense data interface

### Commit 14: Add hmatrix dense construction and conversion

Implement for `Int` and `Double`:

- `matrix`
- `fromLists`, `toLists`
- `row`, `col`
- `fromRows`, `toRows`
- `fromColumns`, `toColumns`
- `build`
- `ident`
- `diag`, `diagl`, `diagRect`, `takeDiag`
- `linspace`

`fromLists` remains because it is part of `hmatrix`.  Reimplement it
through `fromRows . map fromList`, leaving only one row-conformance rule.

### Commit 15: Add dense extraction and block operations

Implement:

- `subVector`, `takesV`, `vjoin`
- `subMatrix`
- `takeRows`, `dropRows`, `takeColumns`, `dropColumns`
- `flipud`, `fliprl`
- `Extractor` and `(??)`
- `fromBlocks`
- `(|||)` and `(===)`
- `diagBlock`
- `repmat`
- `toBlocks`, `toBlocksEvery`

Use Eigen blocks and gathers on `DenseMatrix` runtime values internally.
Preserve `hmatrix` singleton expansion in `fromBlocks`.

### Commit 16: Add dense mapping and reductions

Implement:

- `cmap`
- `prodElements` (`sumElements` was added with the initial `Container`)
- `minElement`, `maxElement`
- `minIndex`, `maxIndex`
- `find`
- `sortVector`, `sortIndex`
- `conj` as the identity for the supported real scalar types
- `cmod` for `Int`

`build` and mapping can produce lazy Haskell lists that the dependent-read
constructors consume.  Do not add a callback ABI or runtime callback
infrastructure.

## Phase 7: Add Double-specific numerical algorithms

Each algorithm family is a separate commit because it is independently useful
and testable.

### Commit 17: Complete Fractional and Floating dense operations

Add native elementwise `Fractional` and `Floating` instances for
`Vector Double` and `Matrix Double`, including singleton broadcasting for
binary operations.  Implement them with Eigen array expressions, not
`cmap` through Haskell callbacks.

### Commit 18: Add dense solves and factorizations

Expose:

- `det`
- `inv`
- `linearSolve`
- `linearSolveLS`
- `chol`
- `expm`

Use `Maybe` where failure is an expected numerical result, such as singular
`linearSolve`.  Test solutions using residuals rather than exact floating
equality.

### Commit 19: Add spectral and orthogonal decompositions

Expose:

- `eigSH`
- `eigenvaluesSH`
- `svd`
- `thinSVD`
- `singularValues`
- `qr`
- `thinQR`

Match `hmatrix` result conventions:

- `A ~= U <> diag s <> tr V` for SVD.
- `A ~= Q <> R` for QR.
- Symmetric eigenvectors are returned as matrix columns.

General nonsymmetric `eig`, which requires complex vectors and matrices,
remains outside this plan.

## Phase 8: Final cleanup and verification

### Commit 20: Document the supported hmatrix subset

- Remove obsolete comments and old API names.
- Document that `Int` supports dense construction, manipulation, arithmetic,
  and products.
- Document that decompositions and transcendental operations require
  `Double`.
- Document that `EVector` is for general runtime collections and `Vector`
  is for contiguous numeric storage.
- Update `NEWS.md` with the interface break and migration examples.
- Do not add compatibility aliases.

Run the invariant searches:

```sh
rg '\bnrows\b|\bncols\b|\bscaleMatrix\b|%\*%' haskell tests/haskell
rg 'toEigen|fromEigen' src
rg 'Box<std::vector<Exp>>' src/builtins/Matrix.cc src/math
rg 'Box<Matrix>|Box<matrix<(int|double)>>' \
  src/builtins src/substitution src/models src/math
```

All four should be empty.  General unboxed `matrix<T>` uses are expected to
remain throughout alignment, DP, parsimony scratch, and tool code.

Final verification:

```sh
ninja -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O

meson test -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O \
  --print-errorlogs

meson test -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O \
  --print-errorlogs \
  "bali-phy:bali-phy 5d +A 50"
```

If a later implementation exposes a bug in one of these new commits, create
an empty child of the introducing commit, fix and verify it there, and squash
it back before continuing.
