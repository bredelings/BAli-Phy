/*
   Copyright (C) 2004-2007 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

///
/// \file exponential.C
///
/// \brief Contains function for computing matrix exponentials.
///

#include <vector>
#include "exponential.H"
#include "eigenvalue.H"


// The approach used in this file works because of general properties
// of the transition matrix of reversible (continuous time) markov
// chains (CTMC).
// 
// Specifically, we have
//  1. pi[i] * Q(i,j)= pi[j] * Q(j,i)
// We note that 
//  2. pi[i] * Q(i,j) is symmetric.
// Therefore we can multiply both sides by the symmetric matrix
// 1/sqrt( pi[i] * pi[j] ),  yielding 
//  3. Q(i,j) * sqrt(pi[i] / pi[j]) is ALSO symmetric.
// We consider PI as the diagonal diagonal matrix such that 
// PI(i,j) = pi[i].  Then we can express this as
//  4. PI^0.5 * Q * PI^-0.5
// We then note that
//  5. exp(PI^0.5 * Qt * PI^-0.5) = PI^0.5 * exp(Qt) * PI^-0.5
// We can compute the first exponential using Singular Value
// Decomposition, because the argument is a symmetric matrix.
//  Therefore we note that
//  5.  exp(Qt) = PI^-0.5 * exp(PI^0.5 * Qt * PI^-0.5) * PI^0.5
// This holds in general for reversible Q, regardless of how Q is
// constructued! 


using std::vector;

// compute the exponential of a matrix that is given in terms of its SVD
Matrix exp(const EigenValues& solution,double t) 
{
  const Matrix& O = solution.Rotation();
  std::vector<double> D = solution.Diagonal();

  // Exponentiate Eigenvalues
  for(int i=0;i<solution.size();i++)
    D[i] = exp(t*D[i]);


  //Matrix E = prod(O,prod(D,trans(O)));

  int size = O.size1();
  Matrix E(size,size);
  for(int i=0;i<size;i++)
    for(int j=0;j<size;j++) {
      double temp =0;
      for(int k=0;k<size;k++)
	temp += O(i,k)*O(j,k)*D[k];
      E(i,j) = temp;
    }
	

  for(int i=0;i<E.size1();i++)
    for(int j=0;j<E.size2();j++)
      assert(E(i,j) >= -1.0e-13);

  return E;
}

/// Compute the exponential of a matrix from a reversible markov chain
Matrix exp(const EigenValues& eigensystem,const vector<double>& D,const double t) {
  const int n = D.size();

  // Compute D^-a * E * D^a
  std::vector<double> DP(n);
  std::vector<double> DN(n);
  for(int i=0;i<D.size();i++) {
    DP[i] = sqrt(D[i]);
    DN[i] = 1.0/DP[i];
  }

  // compute E = exp(S2)
  Matrix E = exp(eigensystem,t);

  // Compute D^-a * E * D^a
  for(int i=0;i<E.size1();i++)
    for(int j=0;j<E.size2();j++)
      E(i,j) *= DN[i]*DP[j];


  for(int i=0;i<E.size1();i++)
    for(int j=0;j<E.size2();j++) {
      assert(E(i,j) >= -1.0e-13);
      if (E(i,j)<0)
	E(i,j)=0;
    }

  return E;
}

// exp(Q) = D^-a * exp(E) * D^a
// E = exp(D^a * Q * D^-a) = exp(D^1/2 * S * D^1/2)

// how do we make the M constant? - const_cast?
// return inline matrix_expression?

Matrix exp(const SMatrix& S,const vector<double>& D,double t) 
{
  const int n = S.size1();

  // compute S2 = D^1/2 * S * D^1/2
  std::vector<double> DP(n);
  std::vector<double> DN(n);

  for(int i=0;i<D.size();i++) {
    DP[i] = sqrt(D[i]);
    DN[i] = 1.0/DP[i];
  }

  //SMatrix S2 = prod(DP,prod<Matrix>(S,DP));
  SMatrix S2 = S;
  for(int i=0;i<S2.size1();i++)
    for(int j=0;j<=i;j++)
      S2(i,j) *= DP[i]*DP[j];


  // compute E = exp(S2)
  Matrix E = exp(S2,t);

  for(int i=0;i<E.size1();i++)
    for(int j=0;j<E.size2();j++)
      E(i,j) *= DN[i]*DP[j];


  // Double-check that E(i,j) is always positive
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

  return exp(solution,t);
}

// for stretch branches: gamma_exp(S,D,t/beta,beta)
// for integrating out the branches against the exponential: gamma_exp(S,D,1.0,mu)

Matrix gamma_exp(const SMatrix& S,const vector<double>& D,double alpha,double beta) {
  const int n = S.size1();

  std::vector<double> DP(n);
  std::vector<double> DN(n);
  for(int i=0;i<D.size();i++) {
    DP[i] = sqrt(D[i]);
    DN[i] = 1.0/DP[i];
  }
  
  SMatrix S2 = S;
  for(int i=0;i<S2.size1();i++)
    for(int j=0;j<=i;j++)
      S2(i,j) *= DP[i]*DP[j];

  Matrix E = gamma_exp(S2,alpha,beta);

  //  E = prod(DN,prod<Matrix>(E,DP));
  for(int i=0;i<E.size1();i++)
    for(int j=0;j<E.size2();j++)
      E(i,j) *= DN[i]*DP[j];

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
  std::vector<double> D = solution.Diagonal();

  // Exponentiate Eigenvalues
  for(int i=0;i<solution.size();i++)
    D[i] = pow(1.0-beta*D[i],-alpha);


  //Matrix E = prod(O,prod(D,trans(O)));

  int size = O.size1();
  Matrix E(size,size);
  for(int i=0;i<size;i++)
    for(int j=0;j<size;j++) {
      double temp =0;
      for(int k=0;k<size;k++)
	temp += O(i,k)*O(j,k)*D[k];
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
