#ifndef INVERSE_H
#define INVERSE_H
#include "mytypes.H"

/// Compute A^-1
Matrix inverse(const Matrix& A);

/// Solve A X = B (e.g. return X)
Matrix solve(const Matrix& A, const Matrix& B);
#endif
