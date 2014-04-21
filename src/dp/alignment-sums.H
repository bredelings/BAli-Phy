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

///
/// \file alignment-sums.H
///
/// \brief Defines functions for sampling from and summing over parts of the alignment.
///

#ifndef ALIGNMENTSUMS_H
#define ALIGNMENTSUMS_H

#include <vector>

#include "math/log-double.H"
#include "dp-engine.H"
#include "alignment/alignment.H"
#include "models/parameters.H"
#include "dp-matrix.H"
#include <boost/dynamic_bitset.hpp>
#include "dp/5way.H"

/// Define type for a function which return the distributions for each column and rate give SOME leaves
typedef std::vector< Matrix > (*distributions_t)(const data_partition&,const std::vector<int>&,int,const boost::dynamic_bitset<>&);


/// Distributions function for a star tree
std::vector< Matrix > distributions_star(const data_partition& P,const std::vector<int>& seq,int root,const boost::dynamic_bitset<>& group);

/// Distributions function for a full tree
std::vector< Matrix > distributions_tree(const data_partition& P,const std::vector<int>& seq,int root,const boost::dynamic_bitset<>& group);


/// Sum of likelihoods for columns which don't contain any characters in sequences mentioned in 'nodes'
log_double_t other_subst(const data_partition& P, const std::vector<int>& nodes);

/// Sum of branch priors for branches not connected to no
log_double_t other_prior(const data_partition& P, const std::vector<int>& nodes);


/// Routine for simultaneously sampling between several Parameter choices, and summing out some nodes
int sample_two_nodes_multi(std::vector<Parameters>& p,const std::vector< A5::hmm_order >& orders,
			   const std::vector<log_double_t>& rho, bool do_OS,bool do_OP);

/// Routine for simultaneously sampling between several Parameter choices, and summing out some nodes
int sample_tri_multi(std::vector<Parameters>& p,const std::vector< std::vector<int> >& nodes,
		      const std::vector<log_double_t>& rho, bool do_OS,bool do_OP);


/// Routine for simultaneously sampling between several Parameter choices, and summing out some nodes
int sample_tri_multi(std::vector<Parameters>& p,const std::vector< std::vector<int> >& nodes,
		      const std::vector<log_double_t>& rho, bool do_OS,bool do_OP, int bandwidth);


struct sample_tri_multi_calculation
{
#ifndef NDEBUG_DP
  const Parameters P0;
#endif
  std::vector<std::vector<int> > nodes;
  std::vector<log_double_t> rho;
  std::vector<std::vector<boost::shared_ptr<DPmatrixConstrained> > > Matrices;
  std::vector< std::vector<log_double_t> > OS;
  std::vector< std::vector<log_double_t> > OP;
  std::vector<log_double_t> Pr;
  log_double_t C1;
  int bandwidth;

  sample_tri_multi_calculation(std::vector<Parameters>& p,const std::vector< std::vector<int> >& nodes_,
			       bool do_OS,bool do_OP,int b=-1);

  void set_proposal_probabilities(const std::vector<log_double_t>&);

  int choose(std::vector<Parameters>& p, bool=true);
};

//------------------- Checking Routines ------------------------//

void check_match_P(const data_partition& P, log_double_t OS, log_double_t OP, const std::vector<int>& path, const DPengine& Matrices);


std::vector<log_double_t> sample_P(const data_partition& P, log_double_t P_choice, log_double_t rho,
			  const std::vector<int>& path, const DPengine& Matrices);

void check_sampling_probabilities(const std::vector< std::vector<log_double_t> >& PR);

#endif
