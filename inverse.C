#include "inverse.H"
#include <iostream>
#include <boost/numeric/bindings/atlas/cblas1.hpp>
#include <boost/numeric/bindings/atlas/clapack.hpp>
#include <boost/numeric/bindings/traits/ublas_matrix.hpp>
#include <boost/numeric/ublas/io.hpp>

namespace ublas = boost::numeric::ublas;
namespace atlas = boost::numeric::bindings::atlas;

Matrix solve(const Matrix& A,const Matrix& B) {
  Matrix A1 = A;
  Matrix B1 = B;
  atlas::gesv (A1, B1);
  return B1;
}

Matrix inverse(const Matrix& M) {
  Matrix I(M.size1(),M.size2());
  for(int i=0;i<I.size1();i++)
    for(int j=0;j<I.size2();j++)
      if (i==j)
	I(i,j) = 1;
      else
	I(i,j) = 0;
  
  return solve(M,I);
}
