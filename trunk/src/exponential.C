#include "exponential.H"
#include "eigenvalue.H"


using namespace ublas;



// how do we make the M constant? - const_cast?
// return inline matrix_expression?

Matrix exp(const SMatrix& S,const BMatrix& D,const double t) {
  const int n = S.size1();

  BMatrix DP(n,n);
  BMatrix DN(n,n);
  for(int i=0;i<D.size1();i++) {
    DP(i,i) = sqrt(D(i,i));
    DN(i,i) = 1.0/DP(i,i);
  }
  
  SMatrix S2 = prod(DP,prod<Matrix>(S,DP));

  Matrix E = exp(S2,t);
  E = prod(DN,prod<Matrix>(E,DP));

  for(int i=0;i<E.size1();i++)
    for(int j=0;j<E.size2();j++) {
      assert(E(i,j) >= -1.0e-13);
      if (E(i,j)<0)
	E(i,j)=0;
    }

  return E;
}

Matrix exp(const SMatrix& M,const double t) {

  EigenValues solution(M);

  Matrix O = solution.Rotation();
  banded_matrix<double> D = solution.Diagonal();

  // Exponentiate Eigenvalues
  for(int i=0;i<solution.size();i++)
    D(i,i) = exp(t*D(i,i));


  //Matrix E = prod(O,prod(D,trans(O)));

  int size = O.size1();
  Matrix E(size,size);
  for(int i=0;i<size;i++)
    for(int j=0;j<size;j++) {
      double temp =0;
      for(int k=0;k<size;k++)
	temp += O(i,k)*O(j,k)*D(k,k);
      E(i,j) = temp;
    }
	

  for(int i=0;i<E.size1();i++)
    for(int j=0;j<E.size2();j++)
      assert(E(i,j) >= -1.0e-13);

  return E;
}

// for stretch branches: gamma_exp(S,D,t/beta,beta)
// for integrating out the branches against the exponential: gamma_exp(S,D,1.0,mu)

Matrix gamma_exp(const SMatrix& S,const BMatrix& D,double alpha,double beta) {
  const int n = S.size1();

  BMatrix DP(n,n);
  BMatrix DN(n,n);
  for(int i=0;i<D.size1();i++) {
    DP(i,i) = sqrt(D(i,i));
    DN(i,i) = 1.0/DP(i,i);
  }
  
  SMatrix S2 = prod(DP,prod<Matrix>(S,DP));

  Matrix E = gamma_exp(S2,alpha,beta);
  E = prod(DN,prod<Matrix>(E,DP));

  for(int i=0;i<E.size1();i++)
    for(int j=0;j<E.size2();j++) {
      assert(E(i,j) >= -1.0e-13);
      if (E(i,j)<0)
	E(i,j)=0;
    }

  return E;
}

Matrix gamma_exp(const SMatrix& M,double alpha,double beta){

  EigenValues solution(M);

  Matrix O = solution.Rotation();
  banded_matrix<double> D = solution.Diagonal();

  // Exponentiate Eigenvalues
  for(int i=0;i<solution.size();i++)
    D(i,i) = pow(1.0-beta*D(i,i),-alpha);


  //Matrix E = prod(O,prod(D,trans(O)));

  int size = O.size1();
  Matrix E(size,size);
  for(int i=0;i<size;i++)
    for(int j=0;j<size;j++) {
      double temp =0;
      for(int k=0;k<size;k++)
	temp += O(i,k)*O(j,k)*D(k,k);
      E(i,j) = temp;
    }
	

  for(int i=0;i<E.size1();i++)
    for(int j=0;j<E.size2();j++)
      assert(E(i,j) >= -1.0e-13);

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
