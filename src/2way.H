/*
   Copyright (C) 2004-2005,2009 Benjamin Redelings

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

#ifndef TWOWAY_H
#define TWOWAY_H

#include <vector>
#include "alignment.H"
#include "tree.H"

namespace A2 {

namespace states {
  const int M  = 0;
  const int G1 = 1;
  const int G2 = 2;
  const int E  = 3;
  const int S  = 4;
}

  alignment construct(const alignment& old, const std::vector<int>& path, int n1,int n2, 
		      const Tree& T, const std::vector<int>& seq1,const std::vector<int>& seq2);




}

#endif
