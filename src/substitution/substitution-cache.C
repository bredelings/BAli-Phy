/*
   Copyright (C) 2004-2005,2007,2009 Benjamin Redelings

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

#include "substitution-cache.H"
#include "util.H"
#include "models/parameters.H"

using std::vector;


void Likelihood_Cache::invalidate_all() {
  int B = dp->t().n_branches();
  for(int b=0;b<2*B;b++)
    invalidate_one_branch(b);
}

void Likelihood_Cache::invalidate_directed_branch(int b) {

  const auto t = dp->t();
  
  if (up_to_date(b))
  {
    vector<int> branches;
    branches.reserve(t.n_branches());
    branches.push_back(b);
    for(int i=0;i<branches.size();i++)
    {
      int b2 = branches[i];
      if (up_to_date(b2))
      {
	t.append_branches_after(b2, branches);
	invalidate_one_branch(b2);
      }
    }
  }

#ifndef NDEBUG
  for(int b2: t.all_branches_after_inclusive(b))
      assert(not up_to_date(b2));
#endif
}

void Likelihood_Cache::invalidate_node(int n) {
  const auto t = dp->t();
  
  for(int b: t.branches_out(n))
    invalidate_directed_branch(b);
}

void Likelihood_Cache::invalidate_one_branch(int b) {
  const context* C = dp->P;
  const_cast<context*>(C)->set_parameter_value(dp->conditional_likelihoods_for_branch[b], 0);
}

void Likelihood_Cache::invalidate_branch(int b) {
  invalidate_directed_branch(b);
  invalidate_directed_branch(dp->t().reverse(b));
}

void Likelihood_Cache::invalidate_branch_alignment(int b)
{
  const auto t = dp->t();
  for(int b2: t.branches_after(b))
    invalidate_directed_branch(b2);

  for(int b2: t.branches_after(t.reverse(b)))
    invalidate_directed_branch(b2);
}

/// Are cached conditional likelihoods for branch b, up to date?
bool Likelihood_Cache::up_to_date(int b) const
{
  return not dp->P->get_parameter_value(dp->conditional_likelihoods_for_branch[b]).is_int();
}

/// Ensure there is backing store for us to work with
void Likelihood_Cache::set_branch(int b, Likelihood_Cache_Branch* LCB)
{
  assert(LCB);
  const context* C = dp->P;
  const_cast<context*>(C)->set_parameter_value(dp->conditional_likelihoods_for_branch[b], LCB);
  (*this)[b];
}

/// Cached conditional likelihoods for branch b
const Likelihood_Cache_Branch& Likelihood_Cache::operator[](int b) const {
  return dp->P->get_parameter_value(dp->conditional_likelihoods_for_branch[b]).as_<Likelihood_Cache_Branch>();
}

Likelihood_Cache::Likelihood_Cache(const data_partition* dp_)
  :dp(dp_)
{ }



