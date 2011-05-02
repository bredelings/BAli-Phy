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

#include "matcache.H"

using std::vector;

/// Set branch 'b' to have length 'l', and compute the transition matrices
void MatCache::setlength(int b,double l,Tree& T,const substitution::MultiModel& SModel) {
  assert(l >= 0);
  assert(b >= 0 and b < T.n_branches());
  T.branch(b).set_length(l);
  for(int m=0;m<SModel.n_base_models();m++)
    transition_P_[b][m] = SModel.transition_p(l,0,m);
}

void MatCache::recalc(const Tree& T,const substitution::MultiModel& SModel) {
  for(int b=0;b<T.n_branches();b++)
    for(int m=0;m<SModel.n_base_models();m++)
      transition_P_[b][m] = SModel.transition_p(T.branch(b).length(),0,m);
}

int MatCache::n_branches() const
{
  return n_branches_;
}

int MatCache::n_models() const
{
  return n_models_;
}

int MatCache::n_states() const
{
  return n_states_;
}

MatCache::MatCache(const Tree& T,const substitution::MultiModel& SM)
  :n_branches_(T.n_branches()),
   n_models_(SM.n_base_models()),
   n_states_(SM.state_letters().size()),
   transition_P_(n_branches_, vector<Matrix>(n_models_,
					     Matrix(n_states_, n_states_)
					     ) 
		 )
{ 
  recalc(T,SM);
}
