#ifndef EIGENVALUE_H
#define EIGENVALUE_H

#include "mytypes.H"
#include "tnt/tnt_array2d.h"
#include "tnt/jama_eig.h"
#include <boost/numeric/ublas/banded.hpp>
#include <boost/numeric/ublas/config.hpp>

class EigenValues {
  const int size_;
  JAMA::Eigenvalue<double>* solution;
public:
  int size() const {return size_;}

  ublas::banded_matrix<double> Diagonal() const;
  Matrix Rotation() const;

  EigenValues(const Matrix& M);
  ~EigenValues() {delete solution;}
};

#endif
