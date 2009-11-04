/*
   Copyright (C) 2004-2005 Benjamin Redelings

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

#ifndef MYTYPES_H
#define MYTYPES_H

#include <cassert>
#include <vector>
#include <string>
#include <fstream>
#include <valarray>

using std::vector;
using std::string;
using std::ifstream;
using std::ofstream;
using std::endl;

#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/symmetric.hpp>
#include <boost/numeric/ublas/banded.hpp>
//#include <boost/numeric/bindings/atlas/clapack.hpp>

namespace ublas = boost::numeric::ublas;
//namespace atlas = boost::numeric::bindings::atlas;

typedef ublas::matrix<double> Matrix;
typedef ublas::symmetric_matrix<double> SMatrix;
typedef ublas::banded_matrix<double> BMatrix;
typedef ublas::vector<double> Vector;
typedef ublas::zero_vector<double> zero_vector;
typedef ublas::unit_vector<double> unit_vector;
typedef ublas::matrix_row<Matrix> matrix_row;
typedef ublas::matrix_column<Matrix> matrix_column;

#include "log-double.H"

typedef log_double_t efloat_t;

typedef ublas::matrix<efloat_t> eMatrix;

inline std::ostream& operator<<(std::ostream& o,const efloat_t& e) {
  return o<<log(e);
}

#endif
