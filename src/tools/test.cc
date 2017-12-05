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

#include <iostream>
#include <cassert>
#include "optimize.H"

using namespace optimize;
using std::vector;

class parabola:public function {
  double x;
  double y;
public:
  double operator()(const Vector& v) const {
    assert(v.size() == 2);
    return -( (v[0]-x)*(v[0]-x)+(v[1]-y)*(v[1]-y) );
  }

  parabola(double x1,double y1):x(x1),y(y1) {}
};


int main() {
  parabola P(2.0,2.0);

  Vector start(2);
  start[0] = 0;
  start[1] = 0;

  //  vector<double> finish = search_basis(start,P);

  Vector finish = search_gradient(start,P);

  std::cout<<finish[0]<<"  "<<finish[1]<<std::endl;

  return 0;
}
