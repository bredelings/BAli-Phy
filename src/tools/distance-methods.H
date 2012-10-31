/*
   Copyright (C) 2004-2006 Benjamin Redelings

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

#ifndef DISTANCE_METHODS_H
#define DISTANCE_METHODS_H

#include "tree/tree.H"
#include "mytypes.H"

Matrix C(const Matrix& M);

Matrix DistanceMatrix(const Tree& T);

Matrix EdgesDistanceMatrix(const Tree& T);

std::vector<double> FastMTM(const Tree& T,const Matrix& D,
		       const std::vector<std::vector<int> >& leaf_set);

std::vector<double> LeastSquares(const Tree& T, const Matrix & D,
			    const std::vector<std::vector<int> >& leaf_set);

std::vector<double> LeastSquares(const Tree& T, const Matrix & D, const Matrix& W,
			    const std::vector<std::vector<int> >& leaf_set);

std::vector<double> FastLeastSquares(const Tree& T, const Matrix & D,
				const std::vector<std::vector<int> >& leaf_set);

Tree NJ(Matrix D);

#endif
