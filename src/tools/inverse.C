#include "inverse.H"
#include "myexception.H"
#include <iostream>
#include "mytypes.H"

#ifdef WITH_ATLAS
#include <boost/numeric/bindings/atlas/cblas1.hpp>
#include <boost/numeric/bindings/atlas/clapack.hpp>
#include <boost/numeric/bindings/traits/ublas_matrix.hpp>
#include <boost/numeric/ublas/io.hpp>

namespace ublas = boost::numeric::ublas;
namespace atlas = boost::numeric::bindings::atlas;
#endif

typedef ublas::matrix<double,ublas::column_major> MatrixC;


MatrixC solve(const MatrixC& A,const MatrixC& B) {
  MatrixC A1 = A;
  MatrixC B1 = B;

#ifdef WITH_ATLAS
  atlas::gesv (A1, B1);
#else
  throw myexception()<<"Can't invert a matrix: not compiled with ATLAS.";
#endif

  return B1;
}

MatrixC inverse(const MatrixC& M) {
  MatrixC I(M.size1(),M.size2());
  for(int i=0;i<I.size1();i++)
    for(int j=0;j<I.size2();j++)
      if (i==j)
	I(i,j) = 1;
      else
	I(i,j) = 0;
  
  return solve(M,I);
}
