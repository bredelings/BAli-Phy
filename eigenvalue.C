#include "eigenvalue.H"

using namespace ublas;
using namespace TNT;
using namespace JAMA;

banded_matrix<double> EigenValues::Diagonal() const {
  banded_matrix<double> D(size_,size_);        // Diagonal matrix

  Array1D<double> D3;
  solution->getRealEigenvalues(D3);
  for(int i=0;i<size_;i++)
    D(i,i) = D3[i];

  return D;
}

Matrix EigenValues::Rotation() const {
  Matrix O(size_,size_);

  Array2D<double> O3;
  solution->getV(O3);

  for(int i=0;i<size_;i++)
    for(int j=0;j<size_;j++)
      O(i,j) = O3[i][j];

  return O;
}

//FIXME - we are leaking memory here!
// Implement a more sane way of copying the data from ublas to TNT
//  - and don't do in inside the intializer!
EigenValues::EigenValues(const Matrix& M)
  :size_(M.size1()),solution(0) {
  Array2D<double> A(size_,size_);
  for(int i=0;i<size_;i++)
    for(int j=0;j<size_;j++)
      A[i][j] = M(i,j);

  solution = new JAMA::Eigenvalue<double>(A);
  assert(size() == M.size2());


#ifndef NDEBUG        //Make sure no imag eigenvalues
  Array1D<double> D4;
  solution->getImagEigenvalues(D4);
  for(int i=0;i<size();i++)
    assert(D4[i] == 0);
#endif
}


