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

#ifndef EIGENVALUE_H
#define EIGENVALUE_H

#include "mytypes.H"
#include "tnt/tnt_array2d.h"
#include "tnt/jama_eig.h"
#include <boost/numeric/ublas/banded.hpp>
#include "object.H"

class EigenValues: public Object {
  Matrix O;
  std::vector<double> D;

  void get_diagonal(JAMA::Eigenvalue<double>& E);
  void get_rotation(JAMA::Eigenvalue<double>& E);
public:
  EigenValues* clone() const {return new EigenValues(*this);}

  int size() const {return D.size();}

  const std::vector<double>& Diagonal() const {return D;}
  std::vector<double>& Diagonal() {return D;}

  const Matrix& Rotation() const {return O;}

  EigenValues(const Matrix& M);
  EigenValues(int n);
};

#endif
