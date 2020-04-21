/*
  Copyright (C) 2004-2007, 2010, 2012, 2014, 2017-2019 Benjamin Redelings

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
#include "math/exponential.H"
#include "math/eigenvalue.H"


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
//  6. To get better answers when t is small (e.g. 0), we note that
//     exp(Qt) - I = PI^-0.5 * exp(PI^0.5 * Qt * PI^-0.5) * PI^0.5 - I
//                 = PI^-0.5 * [exp(PI^0.5 * Qt * PI^-0.5) - I] * PI^0.5
//  7. The singular value decomposition decomposes the symmetric matrix S = O D O^t, where O^t = O^-1
//  8. We therefore have
//     exp(Qt) - I = PI^-0.5 * [exp(O Dt O^-1) - I] * PI^0.5
//                 = PI^-0.5 * [O exp(Dt) O^-1 - O*O^-1] * PI^0.5
//                 = PI^-0.5 * [O (exp(Dt)-I) O^-1 ] * PI^0.5
//  9. We can compute exp(D) - I by simply computing expm1(d[i]*t) for each eigenvalue d[i]
// 10. Then exp(Qt) = I + PI^-0.5 * [O *expm1(tD)*O^-1 ] * PI^0.5
//     Using expm1 here allows us to get exp(Q*0) = I when t=0, and also when t is not close to 0.

using std::vector;

// compute the exp(M) - I from the SVD for M (i.e. M=O D O^t )
Matrix expm1(const EigenValues& solution,double t) 
{
    const Matrix& O = solution.Rotation();
    std::vector<double> D = solution.Diagonal();

    // Exponentiate Eigenvalues
    for(int i=0;i<solution.size();i++)
	D[i] = expm1(t*D[i]);

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
	
#ifndef NDEBUG
    for(int i=0;i<E.size1();i++)
	for(int j=0;j<E.size2();j++)
	    assert(E(i,j) + ((i==j)?1.0:0.0) >= -1.0e-10);
#endif

    return E;
}

// compute the exp(M) I from the SVD for M (i.e. M=O D O^t )
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

#ifndef NDEBUG
    for(int i=0;i<E.size1();i++)
	for(int j=0;j<E.size2();j++)
	    assert(E(i,j) >= -1.0e-10);
#endif

    return E;
}

/// Compute the exponential of a matrix from a reversible markov chain
Matrix exp_small(const EigenValues& eigensystem, const vector<double>& pi, const double t)
{
    Matrix E = expm1(eigensystem,t);

    const int n = pi.size();

    std::vector<double> DP(n);
    std::vector<double> DN(n);
    for(int i=0;i<pi.size();i++) {
	DP[i] = sqrt(pi[i]);
	DN[i] = 1.0/DP[i];
    }

    for(int i=0;i<E.size1();i++)
	for(int j=0;j<E.size2();j++)
	    E(i,j) *= DN[i]*DP[j];

    for(int i=0;i<E.size1();i++)
	E(i,i) += 1.0;

    return E;
}

/// Compute the exponential of a matrix from a reversible markov chain
Matrix exp_large(const EigenValues& eigensystem, const vector<double>& pi, const double t)
{
    Matrix E = exp(eigensystem,t);

    const int n = pi.size();

    std::vector<double> DP(n);
    std::vector<double> DN(n);
    for(int i=0;i<pi.size();i++) {
	DP[i] = sqrt(pi[i]);
	DN[i] = 1.0/DP[i];
    }

    for(int i=0;i<E.size1();i++)
	for(int j=0;j<E.size2();j++)
	    E(i,j) *= DN[i]*DP[j];

    return E;
}

Matrix exp(const EigenValues& eigensystem, const vector<double>& pi, const double t)
{
    bool small = true;
    for(double eigenvalue: eigensystem.Diagonal())
        if (eigenvalue * t < -1)
            small = false;

    auto E = small?exp_small(eigensystem, pi, t):exp_large(eigensystem, pi, t);

    // Force positive, and renormalize rows.
    for(int i=0;i<E.size1();i++)
    {
        double sum = 0;
	for(int j=0;j<E.size2();j++) {
	    assert(E(i,j) >= -1.0e-10);
            // Force positive
            E(i,j) = std::max(0.0, E(i,j));
            sum += E(i,j);
	}
        // Renormalize rows
        assert(t > 10 or std::abs(sum - 1.0) < 1.0e-10*E.size1());
        double factor = 1.0/sum;
	for(int j=0;j<E.size2();j++)
            E(i,j) *= factor;
    }
    return E;
}
