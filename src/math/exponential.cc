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
#include <set>
#include <iostream>
#include <unsupported/Eigen/MatrixFunctions>
#include "exponential.H"
#include "util/log-level.H"


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
    typedef Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> MatrixE;

    auto& O = solution.eigenvectors();
    Eigen::VectorXd D = solution.eigenvalues();
    int n = D.size();

    Matrix E(n,n);
    Eigen::Map<MatrixE> EE(E.begin(), n, n);

    // Exponentiate Eigenvalues
    D = (D.array() * t).expm1();

    // Compute result matrix
    EE = O*Eigen::DiagonalMatrix<double, Eigen::Dynamic>(D)*O.transpose();

//#ifndef NDEBUG
//    for(int i=0;i<E.size1();i++)
//      for(int j=0;j<E.size2();j++)
//          assert(E(i,j) + ((i==j)?1.0:0.0) >= -1.0e-10);
//#endif

    return E;
}

// compute the exp(M) I from the SVD for M (i.e. M=O D O^t )
Matrix exp(const EigenValues& solution,double t)
{
    typedef Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> MatrixE;

    auto& O = solution.eigenvectors();
    Eigen::VectorXd D = solution.eigenvalues();
    int n = D.size();

    // Exponentiate Eigenvalues
    D = (D.array() * t).exp();

    Matrix E(n,n);
    Eigen::Map<MatrixE> EE(E.begin(), n, n);
    EE = O*Eigen::DiagonalMatrix<double, Eigen::Dynamic>(D)*O.transpose();

//#ifndef NDEBUG
//    for(int i=0;i<E.size1();i++)
//      for(int j=0;j<E.size2();j++)
//          assert(E(i,j) >= -1.0e-10);
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

Matrix exp(const EigenValues& eigensystem, const vector<double>& pi, const double t)
{
    bool small = true;
    for(double eigenvalue: eigensystem.eigenvalues())
        if (eigenvalue * t < -1)
            small = false;

    if (small)
        return exp_small(eigensystem, pi, t);
    else
        return exp_large(eigensystem, pi, t);
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

double positivize_and_renormalize_matrix(Matrix& E)
{
    double error = 0;
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

        // The row sum should be 1!
        error = std::max(error, std::abs(1-sum));
    }

    return error;
}

double positivize_and_renormalize_matrix(Eigen::MatrixXd& E)
{
    double error = 0;
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

        // The row sum factor should be 1!
        error = std::max(error, std::abs(1-sum));
    }

    return error;
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

    if (log_verbose and (err > tol or std::abs(err_neg) > tol or std::abs(1 - sum) > tol or err2 > tol))
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



// Find strongly connected components using Kosaraju's algorithm
static std::vector<std::vector<int>> stronglyConnectedComponents(const Eigen::MatrixXd& Q) {
    int n = Q.rows();
    std::vector<std::vector<int>> adj(n), adjT(n);
    for (int i = 0; i < n; ++i)
        for (int j = 0; j < n; ++j)
            if (i != j && Q(i, j) > 0) {
                adj[i].push_back(j);
                adjT[j].push_back(i);
            }

    std::vector<bool> visited(n, false);
    std::vector<int> order;

    std::function<void(int)> dfs1 = [&](int v) {
        visited[v] = true;
        for (int u : adj[v])
            if (!visited[u]) dfs1(u);
        order.push_back(v);
    };

    for (int i = 0; i < n; ++i)
        if (!visited[i]) dfs1(i);

    std::vector<int> comp(n, -1);
    int cid = 0;

    std::function<void(int)> dfs2 = [&](int v) {
        comp[v] = cid;
        for (int u : adjT[v])
            if (comp[u] == -1) dfs2(u);
    };

    for (int i = n - 1; i >= 0; --i) {
        int v = order[i];
        if (comp[v] == -1) {
            dfs2(v);
            cid++;
        }
    }

    std::vector<std::vector<int>> scc(cid);
    for (int i = 0; i < n; ++i)
        scc[comp[i]].push_back(i);
    return scc;
}

// Check if SCC is closed (no outgoing edges)
static bool isClosedClass(const Eigen::MatrixXd& Q, const std::vector<int>& cls) {
    std::set<int> members(cls.begin(), cls.end());
    for (int i : cls) {
        for (int j = 0; j < Q.cols(); ++j) {
            if (Q(i, j) > 0 && !members.count(j))
                return false;
        }
    }
    return true;
}

// Compute stationary distribution for irreducible closed class
static Eigen::VectorXd stationaryDistribution(const Eigen::MatrixXd& Q) {
    const Eigen::Index n = Q.rows();
    assert(Q.cols() == n);

    // Form A = Qᵀ so that we solve A * π = 0
    Eigen::MatrixXd A = Q.transpose();

    // 🔹 Step 1: Normalize to improve conditioning
    double max_diag = A.diagonal().cwiseAbs().maxCoeff();
    if (max_diag > 0.0)
        A /= max_diag;  // scale so that the largest |rate| ≈ 1

    // 🔹 Step 2: Replace one row with normalization constraint
    //     sum_i π_i = 1
    A.row(n - 1).setOnes();

    // Right-hand side: all zeros except last = 1
    Eigen::VectorXd b = Eigen::VectorXd::Zero(n);
    b[n - 1] = 1.0;

    // 🔹 Step 3: Solve A * π = b
    // Use a stable QR decomposition (good for possibly rank-deficient Q)
    Eigen::VectorXd pi = A.colPivHouseholderQr().solve(b);

    // 🔹 Step 4: Handle numerical negatives and renormalize
    for (Eigen::Index i = 0; i < n; ++i)
        if (pi[i] < 0 && pi[i] > -1e-12)  // small roundoff
            pi[i] = 0.0;

    double sum_pi = pi.sum();
    if (sum_pi != 0.0)
        pi /= sum_pi;  // ensure sum = 1 exactly

    return pi;
}

// Compute equilibrium limit
Eigen::VectorXd equilibriumLimit(const Eigen::VectorXd& pi0, const Eigen::MatrixXd& Q)
{
    int n = Q.rows();
    auto sccs = stronglyConnectedComponents(Q);
    std::vector<std::vector<int>> closed, transient;

    // Partition SCCs
    for (auto& cls : sccs) {
        if (isClosedClass(Q, cls))
            closed.push_back(cls);
        else
            transient.push_back(cls);
    }

    // If everything is closed, just mix within initial support
    if (transient.empty()) {
        Eigen::VectorXd pi_inf = Eigen::VectorXd::Zero(n);
        for (auto& cls : closed) {
            Eigen::MatrixXd Qsub(cls.size(), cls.size());
            for (int i = 0; i < (int)cls.size(); ++i)
                for (int j = 0; j < (int)cls.size(); ++j)
                    Qsub(i, j) = Q(cls[i], cls[j]);
            Eigen::VectorXd pi_cls = stationaryDistribution(Qsub);

            double weight = 0.0;
            for (int i : cls) weight += pi0(i);

            for (int k = 0; k < (int)cls.size(); ++k)
                pi_inf(cls[k]) += weight * pi_cls(k);
        }
        return pi_inf;
    }

    // Build index maps
    std::vector<int> T, R;
    for (auto& cls : transient)
        T.insert(T.end(), cls.begin(), cls.end());
    for (auto& cls : closed)
        R.insert(R.end(), cls.begin(), cls.end());

    int nT = T.size(), nR = R.size();
    Eigen::MatrixXd Q_TT(nT, nT), Q_TR(nT, nR);
    for (int i = 0; i < nT; ++i)
        for (int j = 0; j < nT; ++j)
            Q_TT(i, j) = Q(T[i], T[j]);
    for (int i = 0; i < nT; ++i)
        for (int j = 0; j < nR; ++j)
            Q_TR(i, j) = Q(T[i], R[j]);

    // Compute absorption matrix B = (-Q_TT)^{-1} * Q_TR
    Eigen::MatrixXd B = (-Q_TT).colPivHouseholderQr().solve(Q_TR);

    // Compute alpha_R = pi0_T * B + pi0_R
    Eigen::VectorXd pi0_T(nT), pi0_R(nR);
    for (int i = 0; i < nT; ++i) pi0_T(i) = pi0(T[i]);
    for (int i = 0; i < nR; ++i) pi0_R(i) = pi0(R[i]);
    Eigen::VectorXd alpha_R = pi0_T.transpose() * B + pi0_R.transpose();

    // Compute stationary for each closed class
    Eigen::VectorXd pi_inf = Eigen::VectorXd::Zero(n);
    int offset = 0;
    for (auto& cls : closed) {
        Eigen::MatrixXd Qsub(cls.size(), cls.size());
        for (int i = 0; i < (int)cls.size(); ++i)
            for (int j = 0; j < (int)cls.size(); ++j)
                Qsub(i, j) = Q(cls[i], cls[j]);
        Eigen::VectorXd pi_cls = stationaryDistribution(Qsub);

        // total absorption weight for this class
        double W = 0.0;
        for (int i = 0; i < (int)cls.size(); ++i)
            W += alpha_R(offset + i);
        for (int i = 0; i < (int)cls.size(); ++i)
            pi_inf(cls[i]) += W * pi_cls(i);
        offset += cls.size();
    }
    return pi_inf;
}


// Compute lim_{t->inf} pi0*exp(Q*t)
std::vector<double> equilibriumLimit(const std::vector<double>& pi0, const Matrix& Q)
{
    constexpr double tol = 1.0e-7;

    int n = Q.size1();

    Eigen::VectorXd epi0(n+1);
    for(int i=0;i<n;i++)
        epi0[i] = pi0[i];

    auto eQ = toEigen(Q);

    for(int i=0;i<n;i++)
    {
        double sum = 0;
        for(int j=0;j<n;j++)
            if (i!=j)
                sum += eQ(i,j);
        eQ(i,i) = -sum;
    }

    auto epi = equilibriumLimit(epi0, eQ);
    
    // Check that all the rates are 0.
    double err1 = (epi.transpose() * eQ).cwiseAbs().maxCoeff();

    double err_neg = 0;
    for(int i=0;i<n;i++)
    {
        err_neg = std::min(err_neg,epi[i]);
        epi[i] = std::max<double>(epi[i],0);
    }

    double sum = epi.sum();
    epi /= sum;

    // Check that all the rates are 0.
    double err2 = (epi.transpose() * eQ).cwiseAbs().maxCoeff();

    if (log_verbose and (err1 > tol or std::abs(err_neg) > tol or std::abs(1 - sum) > tol or err2 > tol))
    {
        std::cerr<<"compute_stationary_freqs: maxcoeff = "<<eQ.cwiseAbs().maxCoeff()<<"   err1 = "<<err1<<"   err2 = "<<err2<<"   err_neg = "<<err_neg<<"   1-sum = "<<1-sum<<"\n";
    }

    // 5. Copy back to an EVector double;
    std::vector<double> pi(n);
    for(int i=0;i<n;i++)
        pi[i] = epi[i];

    return pi;
}

