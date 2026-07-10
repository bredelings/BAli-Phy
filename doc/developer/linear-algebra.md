# Haskell dense linear algebra

`Numeric.LinearAlgebra` provides BAli-Phy's dense, Eigen-backed subset of the
`hmatrix` interface.  `Vector Int`, `Vector Double`, `Matrix Int`, and
`Matrix Double` use contiguous native storage rather than general runtime
collections.

Both element types support construction, conversion, indexing, extraction,
block operations, elementwise arithmetic with singleton broadcasting, matrix
and vector products, reductions, and optimized matrix multiplication chains.
`Double` additionally supports elementwise `Fractional` and `Floating`
operations, linear solves, Cholesky factorization, matrix exponentials,
symmetric eigendecomposition, SVD, and QR.

The interface intentionally excludes complex, sparse, random, modular, and
matrix file-I/O facilities from `hmatrix`.  General nonsymmetric
eigendecomposition is also excluded because it requires complex result types.

Use `Vector` for uniformly numeric data that is stored or processed
numerically.  Use `EVector` for heterogeneous values, unevaluated expressions,
or structural collections such as a collection of numeric vectors.  `EVector`
is not a compatibility representation for numeric algorithms.

Code using the former native `Data.Matrix` interface should import
`Numeric.LinearAlgebra` and use the hmatrix names:

| Former name | Current name |
| --- | --- |
| `nrows` | `rows` |
| `ncols` | `cols` |
| `transpose` | `tr` |
| `getElem i j m` | `atIndex m (i,j)` |
| `identity` | `ident` |
| `scaleMatrix` | `scale` |
| `zero r c` | `konst 0 (r,c)` |
| `%*%` | `(*)` for elementwise multiplication; `(<>)` for matrix multiplication |

There are no compatibility aliases for the former names.
