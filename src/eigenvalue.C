#include "eigenvalue.H"

using namespace ublas;
using namespace TNT;
using namespace JAMA;

void EigenValues::get_diagonal(JAMA::Eigenvalue<double>& E) {

  Array1D<double> D2;
  E.getRealEigenvalues(D2);

  for(int i=0;i<D.size();i++)
    D[i] = D2[i];
}

void EigenValues::get_rotation(JAMA::Eigenvalue<double>& E) {
  Array2D<double> O2;
  E.getV(O2);

  for(int i=0;i<O.size1();i++)
    for(int j=0;j<O.size2();j++)
      O(i,j) = O2[i][j];
}

EigenValues::EigenValues(int n)
  :O(n,n),D(n)
{ }

EigenValues::EigenValues(const Matrix& M)
  :O(M.size1(),M.size2()),D(M.size1())
{
  assert(M.size1() == M.size2());

  // Make a TNT array from M
  Array2D<double> A(M.size1(),M.size2());
  for(int i=0;i<M.size1();i++)
    for(int j=0;j<M.size2();j++)
      A[i][j] = M(i,j);

  // solve the eigenvalue problem
  JAMA::Eigenvalue<double> solution(A);

#ifndef NDEBUG        //Make sure no imag eigenvalues
  Array1D<double> D4;
  solution.getImagEigenvalues(D4);
  for(int i=0;i<size();i++)
    assert(D4[i] == 0);
#endif

  get_diagonal(solution);
  get_rotation(solution);
}

