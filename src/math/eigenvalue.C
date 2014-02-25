/*
   Copyright (C) 2004 Benjamin Redelings

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

#include "math/eigenvalue.H"
#include <eigen3/Eigen/Eigenvalues>
#include <eigen3/Eigen/Dense>

EigenValues::EigenValues(const Matrix& M)
  :O(M.size1(),M.size2()),D(M.size1())
{
  int n = M.size1();
  assert(M.size1() == M.size2());

  // 1. Make an eigen array from M
  Eigen::MatrixXd M2(n,n);
  for(int i=0;i<n;i++)
    for(int j=0;j<n;j++)
      M2(i,j) = M(i,j);

  // 2. Solve the eigenvalue problem
  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> solution(M2,Eigen::ComputeEigenvectors);

  // 3. Copy values back into current data structures
  auto d = solution.eigenvalues();
  for(int i=0;i<n;i++)
    D[i] = d[i];

  auto o = solution.eigenvectors();

  for(int i=0;i<n;i++)
    for(int j=0;j<n;j++)
      O(i,j) = o(i,j);
}

