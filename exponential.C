#include "exponential.H"
#include "eigenvalue.H"


using namespace ublas;



// how do we make the M constant? - const_cast?
// return inline matrix_expression?

Matrix exp(const Matrix& M,const double t) {

  EigenValues solution(M);

  Matrix O = solution.Rotation();
  banded_matrix<double> D = solution.Diagonal();

  for(int i=0;i<D.size1();i++)
    std::cerr<<"eigenvalue "<<i<<" = "<<D(i,i)<<std::endl;
  std::cerr<<std::endl;

  // Exponentiate Eigenvalues
  for(int i=0;i<solution.size();i++)
    D(i,i) = exp(t*D(i,i));

  // this is wrong - because O isn't scaled as an orthogonal matrix!
  // need to take the inverse!  I think O is orthogonal only if 
  // M is symmetric
  Matrix LU = 0;
  Matrix DOinverse = D;
  atlas::lu_solve(O,D);
  Matrix E = prod(O,DOinverse);

  for(int i=0;i<D.size1();i++)
    std::cerr<<"exp eigenvalue "<<i<<" = "<<D(i,i)<<std::endl;
  std::cerr<<std::endl;

  std::cerr<<"O = \n";
  for(int i=0;i<O.size1();i++) {
    for(int j=0;j<O.size2();j++)
      std::cerr<<O(i,j)<<" ";
    std::cerr<<endl;
  }
  

  for(int i=0;i<E.size1();i++)
    for(int j=0;j<E.size2();j++)
      assert(E(i,j) >= 0.0);

  return E;
}

#ifdef TEST_EXP

#include <iostream>
#include <boost/numeric/ublas/io.hpp>

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

  //  rate[0][0] = alpha;
  //  rate[0][1] = -alpha;
  //  rate[1][0] = alpha;
  //  rate[1][1] = alpha;
  
  Matrix result = exp(rate,1.0);


  std::cout<<rate<<std::endl;
  Matrix result2 = exp(rate,2.0);
  std::cout<<result<<std::endl;
  std::cout<<prod(result,result)<<std::endl;
  std::cout<<result2<<std::endl;

  return 1;
}

#endif //TEST_EXP
