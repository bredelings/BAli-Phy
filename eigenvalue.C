#include "eigenvalue.H"

using namespace ublas;
using namespace TNT;
using namespace JAMA;

banded_matrix<double> EigenValues::Diagonal() const {
  banded_matrix<double> D(size_,size_);        // Diagonal matrix
  Array1D<double> D2(size_,D.data().begin()); // diagonal
  Array1D<double> D3;
  solution.getRealEigenvalues(D3);
  D2.inject(D3);

  return D;
}

Matrix EigenValues::Rotation() const {
  Matrix O(size_,size_);

  Array2D<double> O2(size_,size_,O.data().begin()); // view into O
  Array2D<double> O3;
  solution.getV(O3);
  O2.inject(O3);

  return O;
}

EigenValues::EigenValues(const Matrix& M)
  :size_(M.size1()),
   solution(Array2D<double>(size_,size_,const_cast<double*>(M.data().begin()))) {

  assert(size() == M.size2());
#ifndef NDEBUG        //Make sure no imag eigenvalues
  Array1D<double> D4;
  solution.getImagEigenvalues(D4);
  for(int i=0;i<size();i++)
    assert(D4[i] == 0);
#endif
}
