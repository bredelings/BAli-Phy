/*
   Copyright (C) 2004-2009 Benjamin Redelings

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

#include "prior.H"
#include "substitution/substitution.H"
#include "setup.H"
#include "probability/probability.H"
#include "util.H"
#include "dp/2way.H"

matrix<int> get_path_counts(const pairwise_alignment_t& a)
{
  using namespace A2;

  matrix<int> counts(5,5,0);

  for(int i=1;i<a.size();i++) 
    counts(a[i-1], a[i])++;

  return counts;
}

/// Probability of a pairwise alignment
log_double_t prior_branch_from_counts(const matrix<int>& counts,const indel::PairHMM& Q)
{
  using namespace A2;

  log_double_t P=1;

  // Account for S-? start probability
  for(int i=0;i<Q.size2();i++)
    if (counts(states::S,i))
      P *= Q.start(i);

  // Account for the mass of transitions
  for(int i=0;i<3;i++)
    for(int j=0;j<3;j++) {
      log_double_t Qij = Q(i,j);
      // FIXME - if we propose really bad indel parameters, we can get log(Q_ij) where Qij == 0
      if (counts(i,j))
	P *= pow(Qij,counts(i,j));
    }
  
  // Account for ?-E end probability
  if (not counts(states::S,states::E))
    for(int i=0;i<Q.size1();i++)
      if (counts(i,states::E))
       P *= Q(i,states::E);

  return P;
}

/// Probability of a pairwise alignment
log_double_t prior_branch(const pairwise_alignment_t& a,const indel::PairHMM& Q) 
{
  matrix<int> counts = get_path_counts(a);

  return prior_branch_from_counts(counts,Q);
}
