/*
   Copyright (C) 2004-2005,2010 Benjamin Redelings

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
/// \file mytypes.H
///
/// \brief This file defines some types.
///

#ifndef MYTYPES_H
#define MYTYPES_H

#include <vector>
#include <string>
#include <fstream>
#include <valarray>

#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/symmetric.hpp>

#include "object.H"
namespace ublas = boost::numeric::ublas;
//namespace atlas = boost::numeric::bindings::atlas;

typedef ublas::matrix<double> Matrix;
typedef ublas::symmetric_matrix<double> SymmetricMatrix;
typedef Box<Matrix> MatrixObject;
typedef Box<SymmetricMatrix> SymmetricMatrixObject;

// template <typename T>
// using Vector<T> = typedef POD<std::vector<T> >

#include "math/log-double.H"

typedef log_double_t efloat_t;

inline std::ostream& operator<<(std::ostream& o,const efloat_t& e) {
  return o<<log(e);
}

#endif
