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

#ifndef INVERSE_H
#define INVERSE_H

#include "matrix.H"

namespace ublas = boost::numeric::ublas;

/// Compute A^-1
ublas::matrix<double,ublas::column_major> inverse(const ublas::matrix<double,ublas::column_major>& A);

/// Solve A X = B (e.g. return X)
ublas::matrix<double,ublas::column_major> solve(const ublas::matrix<double,ublas::column_major>& A,const ublas::matrix<double,ublas::column_major>& B);

#endif
