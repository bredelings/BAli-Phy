/*
   Copyright (C) 2004,2006 Benjamin Redelings

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

#include "inverse.H"
#include "myexception.H"
#include <iostream>

#include <Eigen/Dense>
#include <Eigen/LU>

Eigen::MatrixXd copy(const Matrix& M)
{
  Eigen::MatrixXd M2(M.size1(), M.size2());
  for(int i=0;i<M.size1();i++)
    for(int j=0;j<M.size2();j++)
      M2(i,j) = M(i,j);
  return M2;
}

Matrix copy(const Eigen::MatrixXd& M)
{
  Matrix M2(M.rows(), M.cols());
  for(int i=0;i<M.rows();i++)
    for(int j=0;j<M.cols();j++)
      M2(i,j) = M(i,j);
  return M2;
}

Matrix solve(const Matrix& A,const Matrix& B) 
{
  auto A1 = copy(A);
  auto B1 = copy(B);

  Eigen::FullPivLU<Eigen::MatrixXd> LU(A1);
  Eigen::MatrixXd S = LU.solve(B1);
  return copy(S);
}

Matrix inverse(const Matrix& M) 
{
  int n = M.size1();
  assert( n == M.size2());

  Matrix I(n,n);
  for(int i=0;i<I.size1();i++)
    for(int j=0;j<I.size2();j++)
      if (i==j)
	I(i,j) = 1;
      else
	I(i,j) = 0;
  
  return solve(M,I);
}
