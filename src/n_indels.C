/*
   Copyright (C) 2006,2009 Benjamin Redelings

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

#include "n_indels.H"
#include "dp/2way.H"
#include "util.H"

using std::vector;

namespace states = A2::states;

vector<int> get_indel_lengths(const alignment& A,const Tree& T, int b)
{
  vector<int> indels;

  vector<int> pairwiseA = get_path(A, T.branch(b).target(), T.branch(b).source());

  int last_state = states::M;
  for(unsigned i=0; i<pairwiseA.size(); i++) 
  {
    int current_state = pairwiseA[i];
    
    if ((current_state == states::G1) or (current_state == states::G2)) {
      if (last_state != current_state)
	indels.push_back(1);
      else
	indels.back()++;
    }

    last_state = current_state;
  }
  return indels;
}

unsigned n_indels(const alignment& A,const Tree& T, int b)
{
  return get_indel_lengths(A,T,b).size();
}

unsigned total_length_indels(const alignment& A,const Tree& T, int b)
{
  return sum(get_indel_lengths(A,T,b));
}

unsigned n_indels(const alignment& A,const Tree& T)
{
  unsigned total=0;
  for(int b=0;b<T.n_branches();b++)
    total += n_indels(A,T,b);
  return total;
}

unsigned total_length_indels(const alignment& A,const Tree& T)
{
  unsigned total=0;
  for(int b=0;b<T.n_branches();b++)
    total += total_length_indels(A,T,b);
  return total;
}
