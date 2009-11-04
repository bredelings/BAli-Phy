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

#ifndef OPTIMIZE_H
#define OPTIMIZE_H

#include <vector>
#include <valarray>

namespace optimize {
  using std::valarray;

  typedef valarray<double> Vector;

  inline double dot(const Vector& v1,const Vector& v2) {
    return (v1*v2).sum();
  }

  class function {
  public:
    virtual double operator()(const Vector&) const =0;
    virtual ~function() {}
  };

  /// Derivative of f, using dx as the delta
  Vector gradient(const function& f,const Vector& x,const Vector& dx);

  /// Derivative of f in direction dx, using scale*dx as the delta
  double derivative(const function& f,const Vector& x,const Vector& dx,const double scale=0.1);

  /// 2nd Derivative of f in direction dx, using scale*dx as the delta
  double derivative2(const function& f,const Vector& x,const Vector& dx,const double scale=0.1);

  Vector search_basis(const Vector& start,const function& f,double delta = 1.0e-6,int maxiterations=200);

  Vector search_gradient(const Vector& start,const function& f,double delta = 1.0e-6,int maxiterations=200);

}

#endif
