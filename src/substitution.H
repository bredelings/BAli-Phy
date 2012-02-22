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

#include "alignment.H"
#include "tree.H"
struct Mat_Cache;
class Parameters;
#include "parameters.H"
#include "substitution-cache.H"
#include "substitution-index.H"

/// A namespace for functions related to calculating the substitution likelihood.
namespace substitution {

  inline double sum(const std::vector<double>& f,int l1,const alphabet& a)
  {
    double total=0;
    for(int l=0;l<a.size();l++)
      if (a.matches(l,l1))
	total += f[l];
    return total;
  }

  inline double sum(const std::valarray<double>& f,int l1,const alphabet& a)
  {
    double total=0;
    for(int l=0;l<a.size();l++)
      if (a.matches(l,l1))
	total += f[l];
    return total;
  }

  inline double sum(const Matrix Q,int l1, int l2, const alphabet& a)
  {
    double total=0;
    for(int l=0;l<a.size();l++)
      if (a.matches(l,l2))
	total += Q(l1,l);
    return total;
  }

  /// Find the likelihood matrix for data behind branches 'b', for columns with a character in at least one node in 'req'.  Reorder the columns according to permutation 'seq', and add 'delta' padding matrices at the beginning.
  std::vector<Matrix>
  get_column_likelihoods(const data_partition&, const std::vector<int>& b,
			 const std::vector<int>& seq,int delta=0);

  /// Find the likelihood matrices for data at leaf node n.
  std::vector<Matrix>
  get_leaf_seq_likelihoods(const data_partition& P, int n, int delta);

  /// Full likelihood - all columns, all rates
  efloat_t Pr(const data_partition&);

  efloat_t other_subst(const data_partition&, const std::vector<int>& nodes);
  
  efloat_t Pr(const std::vector< std::vector<int> >& sequences, const alignment& A, subA_index_t& I,
	      const Mat_Cache& MC,const Tree& T,::Likelihood_Cache& cache,
	      const MultiModelObject& MModel);
  efloat_t Pr(const data_partition&,Likelihood_Cache& LC);

  std::vector<std::vector<double> > get_model_likelihoods_by_alignment_column(const data_partition&);

  std::vector<std::vector<double> > get_model_probabilities_by_alignment_column(const data_partition&);

  // Full likelihood - all columns, all rates (star tree)
  efloat_t Pr_star(const data_partition&);

  // Full likelihood - if some seemingly aligned sequences at the root have + -> - -> +
  efloat_t Pr_unaligned_root(const data_partition&);

  // Full likelihood - if everything is unaligned....
  efloat_t Pr_unaligned(const data_partition&);

  // Full likelihood of the single sequence with the lowest likelihood
  efloat_t Pr_single_sequence(const data_partition&);

  extern int total_peel_leaf_branches;
  extern int total_peel_internal_branches;
  extern int total_peel_branches;
  extern int total_calc_root_prob;
  extern int total_likelihood;
}

#endif
