#include "mytypes.H"
#include <iostream>

#include "tnt/tnt_array2d.h"
#include "tnt/jama_eig.h"
#include <boost/numeric/ublas/banded.hpp>
#include <boost/numeric/ublas/config.hpp>
#include <boost/numeric/ublas/io.hpp>

using namespace ublas;

//return inline matrix_expression?
Matrix exp(Matrix& M,const double t=1.0) {
  const int size = M.size1();
  assert(M.size2() == size);

  TNT::Array2D<double> array(size,size,M.data().begin());

  JAMA::Eigenvalue<double> solution(array);

  ublas::banded_matrix<double> D(size,size);
  TNT::Array1D<double> D2(size,D.data().begin()); // diagonal
  solution.getRealEigenvalues(D2);

  for(int i=0;i<size;i++)
    D2[i] = exp(t*D2[i]);

  Matrix O(size,size);   // rotation matrix
  TNT::Array2D<double> O2(size,size,O.data().begin());
  solution.getV(O2); // for some reason, this doesn't actually set O,
                     // because it make a NEW thing to point to, that
                     // has the solution
  Matrix E = prod(O,prod(D,trans(O)));
  return E;
}


const double alpha = .03;

int main() {
  Matrix rate(4,4);

  for(int i=0;i<rate.size1();i++) 
    for(int j=0;j<rate.size2();j++) 
      rate(i,j) = alpha;

  for(int i=0;i<rate.size1();i++) {
    double sum=0;
    for(int j=0;j<rate.size2();j++) {
      if (j==i) continue;
      sum += rate(i,j);
    }
    rate(i,i) = -sum;
  }
  
  std::cout<<rate<<std::endl;
  Matrix result = exp(rate);
  std::cout<<result<<std::endl;
  return 1;
}
