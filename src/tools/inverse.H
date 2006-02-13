#ifndef INVERSE_H
#define INVERSE_H
#include "mytypes.H"

/// Compute A^-1
ublas::matrix<double,ublas::column_major> inverse(const ublas::matrix<double,ublas::column_major>& A);

/// Solve A X = B (e.g. return X)
ublas::matrix<double,ublas::column_major> solve(const ublas::matrix<double,ublas::column_major>& A,const ublas::matrix<double,ublas::column_major>& B);

#endif
