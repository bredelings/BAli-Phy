/*
   Copyright (C) 2005 Benjamin Redelings

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

#include "bootstrap.H"

using std::vector;

void bootstrap_sample_indices(vector<int>& sample,unsigned blocksize) 
{
  unsigned size = sample.size();

  if (blocksize > size) 
    blocksize = size;

  int i=0;
  while(i<size) {
    int j = myrandom(size+1-blocksize);
    for(int k=0;k<blocksize and i < size;k++)
      sample[i++] = j++;
  }
}

vector<int> bootstrap_sample_indices(unsigned size,unsigned blocksize) 
{
  vector<int> sample(size);

  bootstrap_sample_indices(sample,blocksize);

  return sample;
}

