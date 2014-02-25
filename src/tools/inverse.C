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
