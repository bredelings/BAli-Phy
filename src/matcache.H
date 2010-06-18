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

#ifndef MATCACHE_H
#define MATCACHE_H

#include <vector>
#include "smodel.H"
#include "tree.H"
#include "mytypes.H"

/// Substitution Model w/ cache
class MatCache {

  int n_branches_;

  int n_models_;

  int n_states_;

  std::vector< std::vector<Matrix> > transition_P_;

public:

  int n_models() const;

  int n_branches() const;

  int n_states() const;

  /// Show all the matrices
  const std::vector< std::vector< Matrix > >& transition_P() const { return transition_P_; }

  /// For a given rate, Show all the matrices for each branch
  const std::vector< Matrix > & transition_P(int b) const { return transition_P_[b]; }

  /// For a given rate, Show all the matrices for each branch
  const std::vector< Matrix > & operator[](int b) const { return transition_P_[b]; }

  /// For a given rate, and branch, show the matrix
  const Matrix& transition_P(int b,int r) const 
  {
#ifndef NDEBUG
    // is this to make it work when we just have a pair?
    // Then we can't hack out the second branch...
    // Hmm.. could we make one node the root?
    assert(b < n_branches());
    if (b == n_branches()) b--;
#endif
    return transition_P_[b][r];
  }

  /// Set branch 'b' to have length 'l', and compute the transition matrices
  void setlength(int b,double l,Tree&,const substitution::MultiModel&);
  
  /// Recalculate all the cached transition matrices
  void recalc(const Tree&,const substitution::MultiModel&);

  MatCache(const Tree& T,const substitution::MultiModel& SM);
};

#endif
