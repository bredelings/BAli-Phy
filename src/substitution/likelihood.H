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
/// \file likelihood.H
///
/// \brief Contains functions related to calculating the substitution likelihood.
///

#ifndef SUBSTITUTION_H
#define SUBSTITUTION_H

#include <vector>
#include "util/matrix.H"
#include "util/math/log-double.H"
#include "substitution/cache.H"
#include "computation/expression/expression_ref.H"

#include "dp/2way.H" // for pairwise_alignment_t

class data_partition;

/// A namespace for functions related to calculating the substitution likelihood.
namespace substitution {

    Likelihood_Cache_Branch get_column_likelihoods(const data_partition&, const std::vector<int>& b, const matrix<int>& index,int delta=0);

    /// Find the likelihood matrices for data at leaf node n.
    Likelihood_Cache_Branch get_leaf_seq_likelihoods(const data_partition& P, int n, int delta);

    log_double_t other_subst(const data_partition&, const std::vector<int>& nodes);
  
    extern int total_peel_leaf_branches;
    extern int total_peel_internal_branches;
    extern int total_peel_branches;
    extern int total_calc_root_prob;
    extern int total_likelihood;
    extern long total_root_clv_length;

    log_double_t calc_root_probability(const Likelihood_Cache_Branch& LCB1,
                                       const Likelihood_Cache_Branch& LCB2,
                                       const Likelihood_Cache_Branch& LCB3,
                                       const pairwise_alignment_t& A0,
                                       const pairwise_alignment_t& A1,
                                       const pairwise_alignment_t& A2,
                                       const Matrix& F);

    object_ptr<const Likelihood_Cache_Branch>
    simple_sequence_likelihoods(const EVector& sequence,
				const alphabet& a,
				const EVector& smap,
				int n_models);

    object_ptr<const Likelihood_Cache_Branch>
    peel_leaf_branch_simple(const EVector& sequence, const alphabet& a, const EVector& transition_P);

    object_ptr<const Likelihood_Cache_Branch>
    peel_leaf_branch(const EVector& sequence, const alphabet& a, const EVector& transition_P, const EVector& smap);

    object_ptr<const Likelihood_Cache_Branch>
    peel_internal_branch(const Likelihood_Cache_Branch& LCB1,
                         const Likelihood_Cache_Branch& LCB2,
                         const pairwise_alignment_t& A0,
                         const pairwise_alignment_t& A1,
                         const EVector& transition_P,
                         const Matrix& F);

    object_ptr<const Likelihood_Cache_Branch>
    peel_branch(const EVector& LCN,
		const EVector& LCB,
		const EVector& A_,
		const EVector& transition_P,
		const Matrix& F);


    log_double_t calc_root_prob(const EVector& LCN,
				const EVector& LCB,
				const EVector& A_,
				const Matrix& F);


    Matrix get_letter_likelihoods(int l, const alphabet& a, const data_partition& P);

    Likelihood_Cache_Branch
    get_column_likelihoods(const data_partition& P, const std::vector<int>& b, const matrix<int>& index, int delta);

    Vector<std::pair<int,int>> sample_root_sequence(const EVector& LCN,
					       const EVector& LCB,
					       const EVector& A_,
                                               const Matrix& F);

    Vector<std::pair<int,int>> sample_branch_sequence(const Vector<std::pair<int,int>>& parent_seq,
						 const pairwise_alignment_t& parent_A,
						 const EVector& LCN,
						 const EVector& LCB,
						 const EVector& A_,
						 const EVector& transition_P,
						 const Matrix& F);
}
#endif
