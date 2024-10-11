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
#include <unsupported/Eigen/MatrixFunctions>


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
	
//#ifndef NDEBUG
//    for(int i=0;i<E.size1();i++)
//	for(int j=0;j<E.size2();j++)
//	    assert(E(i,j) + ((i==j)?1.0:0.0) >= -1.0e-10);
//#endif

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

//#ifndef NDEBUG
//    for(int i=0;i<E.size1();i++)
//	for(int j=0;j<E.size2();j++)
//	    assert(E(i,j) >= -1.0e-10);
//#endif

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

void positivize_and_renormalize_matrix(Matrix& E)
{
    // Force positive, and renormalize rows.
    for(int i=0;i<E.size1();i++)
    {
        double sum = 0;
	for(int j=0;j<E.size2();j++) {
	    // assert(E(i,j) >= -1.0e-10);

            // Force positive
            E(i,j) = std::max(0.0, E(i,j));
            sum += E(i,j);
	}

        // assert(t > 10 or std::abs(sum - 1.0) < 1.0e-10*E.size1());

        // Renormalize rows
        double factor = 1.0/sum;
	for(int j=0;j<E.size2();j++)
            E(i,j) *= factor;
    }
}

Matrix exp(const EigenValues& eigensystem, const vector<double>& pi, const double t)
{
    bool small = true;
    for(double eigenvalue: eigensystem.Diagonal())
        if (eigenvalue * t < -1)
            small = false;

    auto E = small?exp_small(eigensystem, pi, t):exp_large(eigensystem, pi, t);

    positivize_and_renormalize_matrix(E);

    return E;
}

void positivize_and_renormalize_matrix(Eigen::MatrixXd& E)
{
    // Force positive, and renormalize rows.
    for(int i=0;i<E.rows();i++)
    {
        double sum = 0;
        for(int j=0;j<E.cols();j++) {
            // assert(E(i,j) >= -1.0e-10);

            // Force positive
            E(i,j) = std::max(0.0, E(i,j));
            sum += E(i,j);
        }

        // assert(t > 10 or std::abs(sum - 1.0) < 1.0e-10*E.size1());

        // Renormalize rows
        double factor = 1.0/sum;
        for(int j=0;j<E.cols();j++)
            E(i,j) *= factor;
    }
}

Eigen::MatrixXd exp(const Eigen::MatrixXd& Q, double t)
{
    // 1. Take the matrix exponential
    Eigen::MatrixXd E = (Q*t).exp();

    // 2. Ensure that all entries are non-negative and rows sum to 1
    positivize_and_renormalize_matrix(E);

    return E;
}

Eigen::MatrixXd toEigen(const Matrix& Q)
{
    int N1 = Q.size1();
    int N2 = Q.size2();

    Eigen::MatrixXd EQ(N1,N2);

    for(int i=0;i<N1;i++)
        for(int j=0;j<N2;j++)
            EQ(i,j) = Q(i,j);

    return EQ;
}

Matrix fromEigen(const Eigen::MatrixXd& EM)
{
    int N1 = EM.rows();
    int N2 = EM.cols();

    Matrix M(N1,N2);

    for(int i=0;i<N1;i++)
        for(int j=0;j<N2;j++)
            M(i,j) = EM(i,j);

    return M;
}

double rate_away(const vector<double>& pi, const Eigen::MatrixXd& Q)
{
    double rate = 0;
    for(int s=0;s<Q.rows();s++)
    {
        assert(Q(s,s) <= 0);
        rate -= pi[s] * Q(s,s);
    }
    assert(rate >= 0);
    return rate;
}

std::vector<double> compute_stationary_freqs(const Matrix& Q)
{
    constexpr double tol = 1.0e-7;

    assert(Q.size1() == Q.size2());
    int n = Q.size1();

    for(int i=0;i<n;i++)
    {
	double sum = 0;
	for(int j=0;j<n;j++)
	    if (i != j)
	    {
		sum += Q(i,j);
		assert(Q(i,j) >= 0);
	    }
	assert(std::abs(sum + Q(i,i)) < tol);
    }

    // We start with pi * Q = 0, sum(pi) = 1.
    // We transpose to get Q * pi = 0.
    // For the sum, We add an extra row of 1s to get 1[i] * pi[i] = 1.

    // 1. QQ = Q, but with an extra row of 1's
    Eigen::MatrixXd QQ(n+1,n);
    for(int i=0;i<n;i++)
    {
        for(int j=0;j<n;j++)
            QQ(i,j) = Q(j,i);

        // Must be initialized for normalization below.
        QQ(n,i) = 0;
    }

    // 2. Treat different multiples of Q the same.
    //    This necessary to avoid ignoring the sum(pi)=1 constraint for large |Q|.
    double scale = QQ.cwiseAbs().sum();
    QQ /= scale;

    // 3. This sets up the sum(pi)
    for(int j=0;j<n;j++)
        QQ(n,j) = 1;

    // 3. b = 0*n + 1
    Eigen::VectorXd b(n+1);
    for(int i=0;i<n;i++)
        b[i] = 0;
    // This sets up the result of sum(pi)
    b[n] = 1;

    // 4. Solve the equations
    // Eigen::VectorXd epi = QQ.ColPivHouseholderQr.solve(b);  Maybe faster?
    Eigen::VectorXd epi = QQ.fullPivLu().solve(b);

    double err = (QQ * epi - b).cwiseAbs().sum();

    double err_neg = 0;
    for(int i=0;i<n;i++)
    {
        err_neg = std::min(err_neg,epi[i]);
        epi[i] = std::max<double>(epi[i],0);
    }

    double sum = epi.sum();
    epi /= sum;

    double err2 = (QQ * epi - b).cwiseAbs().sum();

    if (err > tol or std::abs(err_neg) > tol or std::abs(1 - sum) > tol or err2 > tol)
    {
        std::cerr<<"compute_stationary_freqs: err1 = "<<err<<"   err2 = "<<err2<<"   err_neg = "<<err_neg<<"   1-sum = "<<1-sum<<"\n";
    }

    // 5. Copy back to an EVector double;
    std::vector<double> pi(n);
    for(int i=0;i<n;i++)
        pi[i] = epi[i];

    return pi;
}

bool checkStationary(const Eigen::MatrixXd& Q, const Eigen::VectorXd& pi, double tol)
{
    assert(tol >= 0);
    assert(Q.rows() == Q.cols());
    assert(Q.rows() == pi.rows());
    assert(std::abs(pi.sum() - 1.0) < tol);

    // 1. Treat different multiples of Q the same.
    double scale = Q.cwiseAbs().sum();

    // 2. pi'[j] = \sum_i Q(j,i)*pi(i) = 0
    double err = (Q.transpose()*pi).cwiseAbs().sum()/scale;
    assert(err >= 0);

    // 3. Check the error tolerance.
    return (err < tol);
}

bool checkStationary(const Matrix& Q, const std::vector<double>& pi, double tol)
{
    int n = Q.size1();
    assert(Q.size2() == n);

    // 1. Q2 = Q
    Eigen::MatrixXd Q2 = toEigen(Q);

    // 2. pi2 = pi
    Eigen::VectorXd pi2(n);
    for(int i=0;i<n;i++)
        pi2[i] = pi[i];

    return checkStationary(Q2, pi2, tol);
}

bool checkReversible(const Eigen::MatrixXd& Q, const Eigen::VectorXd& pi, double tol)
{
    // S(i,j) = pi[i]*Q(i,j) should be symmetric.

    auto S = pi.asDiagonal()*Q;

    double scale = S.cwiseAbs().sum();

    double error = (S - S.transpose()).cwiseAbs().sum() / scale;

    return (error < tol);
}

bool checkReversible(const Matrix& Q, const std::vector<double>& pi, double tol)
{
    int n = Q.size1();
    assert(Q.size2() == n);
    assert(pi.size() == n);

    // 1. Q2 = Q
    Eigen::MatrixXd Q2 = toEigen(Q);

    // 2. pi2 = pi
    Eigen::VectorXd pi2(n);
    for(int i=0;i<n;i++)
        pi2[i] = pi[i];

    return checkReversible(Q2, pi2, tol);
}

