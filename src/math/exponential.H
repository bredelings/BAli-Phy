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
/// \file exponential.H
///
/// \brief Defines function for computing matrix exponentials.
///



#ifndef EXPONENTIAL_H
#define EXPONENTIAL_H
#include "mytypes.H"
#include "math/eigenvalue.H"

#include <boost/numeric/ublas/symmetric.hpp>
typedef ublas::symmetric_matrix<double> SMatrix;

Matrix exp(const EigenValues& eigensystem,const std::vector<double>& D,double t);
Matrix exp(const SMatrix& S,const std::vector<double>& D,double t=1.0);
Matrix exp(const SMatrix& M,const double t=1.0);

Matrix gamma_exp(const SMatrix& S,const std::vector<double>& D,double alpha,double beta);
Matrix gamma_exp(const SMatrix& M,double alpha,double beta);

#endif
