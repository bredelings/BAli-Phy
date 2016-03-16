/*
   Copyright (C) 2004,2007 Benjamin Redelings

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

#include <vector>
#include <cassert>
#include "rng.H"

template <typename T>
void random_shuffle(std::vector<T>& v) 
{
  for(int i=1;i<v.size();i++) 
  {
    int l = myrandom(i+1);
    if (l<i)
      std::swap(v[l],v[i]);
  }
}

template <typename T>
std::vector<T> randomize(std::vector<T> v) 
{
  random_shuffle(v);
  return v;
}
}


std::vector<int> random_permutation(int size);
