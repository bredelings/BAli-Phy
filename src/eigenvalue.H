#ifndef EIGENVALUE_H
#define EIGENVALUE_H

#include "mytypes.H"
#include "tnt/tnt_array2d.h"
#include "tnt/jama_eig.h"
#include <boost/numeric/ublas/banded.hpp>
#include <boost/numeric/ublas/config.hpp>
#include "clone.H"

class EigenValues: public Cloneable {
  Matrix O;
  std::vector<double> D;

  void EigenValues::get_diagonal(JAMA::Eigenvalue<double>& E);
  void EigenValues::get_rotation(JAMA::Eigenvalue<double>& E);
public:
  EigenValues* clone() const {return new EigenValues(*this);}

  int size() const {return D.size();}

  const std::vector<double>& Diagonal() const {return D;}
  std::vector<double>& Diagonal() {return D;}

  const Matrix& Rotation() const {return O;}

  EigenValues(const Matrix& M);
  EigenValues(int n);
};

#endif
