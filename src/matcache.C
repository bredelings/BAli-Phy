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
    transition_P_[m][b] = SModel.transition_p(l,m);
}
  
void MatCache::recalc(const Tree& T,const substitution::MultiModel& SModel) {
  for(int b=0;b<T.n_branches();b++)
    for(int m=0;m<SModel.n_base_models();m++)
      transition_P_[m][b] = SModel.transition_p(T.branch(b).length(),m);
}

MatCache::MatCache(const Tree& T,const substitution::MultiModel& SM) 
  :transition_P_(vector< vector <Matrix> >(SM.n_base_models(),
					   vector<Matrix>(T.n_branches(),
							  Matrix(SM.Alphabet().size(),
								 SM.Alphabet().size()
								 )
							  ) 
					   ) 
		 )
  { 
    recalc(T,SM);
  }

