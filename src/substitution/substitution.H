/*
   Copyright (C) 2004-2005,2007,2009-2010 Benjamin Redelings

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

///
/// \file substitution.H
///
/// \brief Contains functions related to calculating the substitution likelihood.
///

#ifndef SUBSTITUTION_H
#define SUBSTITUTION_H

#include <vector>
#include "matrix.H"
#include "math/log-double.H"

class data_partition;

/// A namespace for functions related to calculating the substitution likelihood.
namespace substitution {

  std::vector<std::vector<std::pair<int,int>>> sample_ancestral_states(const data_partition& P);

  std::vector<Matrix> 
  get_column_likelihoods(const data_partition&, const std::vector<int>& b, const matrix<int>& index,int delta=0);

  /// Find the likelihood matrices for data at leaf node n.
  std::vector<Matrix>
  get_leaf_seq_likelihoods(const data_partition& P, int n, int delta);

  /// Full likelihood - all columns, all rates
  log_double_t Pr(const data_partition&);

  log_double_t other_subst(const data_partition&, const std::vector<int>& nodes);
  
  std::vector<std::vector<double> > get_model_likelihoods_by_alignment_column(const data_partition&);

  std::vector<std::vector<double> > get_model_probabilities_by_alignment_column(const data_partition&);

  extern int total_peel_leaf_branches;
  extern int total_peel_internal_branches;
  extern int total_peel_branches;
  extern int total_calc_root_prob;
  extern int total_likelihood;
}

#endif
