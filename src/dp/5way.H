/*
   Copyright (C) 2004-2007,2010 Benjamin Redelings

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
/// \file 5way.H
///
/// \brief Defines the HMM for pairwise alignments on 5 branches in an NNI configuration.
///

#ifndef FIVEWAY_H
#define FIVEWAY_H

#include "mytypes.H"
#include "alignment/alignment.H"
#include "models/parameters.H"
#include "2way.H"
#include "hmm.H"


// Returns the state, with the validity of sub-alignments 1,2,3 marked in bits 6,7,8
namespace A5 {

  namespace states = A2::states;
  typedef HMM::bitmask_t bitmask_t;

  // std::vector<int> get_nodes(const Tree& T,int b);
  std::vector<int> get_nodes_random(const Tree& T,int b);

  // 21 bits used total:
  // bits 5,4,3,2,1,0     = node present mask
  // bits 15,14,13/12,11/10,9/8,7/6    = sub-alignment state
  // bits 20,19,18,17,16    = sub-alignment not-present mask
  
  const unsigned int leafbitsmask = (1<<4)-1;
  const unsigned int bitsmask = (1<<6)-1;

  /// construct the list of states
  std::vector<int> construct_states();

  /// Get the std::vector of start probabilities
  std::vector<double> get_start_P(const std::vector<indel::PairHMM>& P);

  int bits_to_substates(int bits);

  /// What is the index (and therefore, name) of state 'state'
  int findstate(int state,const std::vector<int>& states);

  /// Compute the probability of moving from state #S1 to state #S2
  double getQ(int S1,int S2,const std::vector<indel::PairHMM>& P,
		const std::vector<int>& states);

  /// Update the full transition matrix
  void updateQ(Matrix& Q,const std::vector<indel::PairHMM>& P,const std::vector<int>& states);
  /// Full the full transition matrix
  void fillQ(Matrix& Q,const std::vector<indel::PairHMM>& P,const std::vector<int>& states);
  /// Create the full transition matrix
  Matrix createQ(const std::vector<indel::PairHMM>& P,const std::vector<int>& states);

  /// Construct a new alignment, from alignment 'old' and 5way alignment 'path'
  alignment construct(const alignment& old, const std::vector<int>& path, 
		      const std::vector<int>& nodes,const Tree& T,
		      const std::vector< std::vector<int> >& seqs,
		      const std::vector<int>& states_list);

  /// Get the ordered list of columns present in 5way alignment of 'nodes'
  std::vector<int> getorder(const alignment& A,const std::vector<int>& nodes);

  alignment project(const alignment& A,const std::vector<int>& nodes);

  /// What path does 'A' determine through the 5way HMM?
  std::vector<int> get_path(const alignment& A,const std::vector<int>& nodes,const std::vector<int>& states);

  /// Return the log of the acceptance ration for moving from (A1,P1) -> (A2,P2)
  efloat_t correction(const data_partition& P,const std::vector<int>& nodes);

  efloat_t correction(const Parameters& P,const std::vector<int>& nodes);

  efloat_t acceptance_ratio(const Parameters& P1,const std::vector<int>&,
			      const Parameters& P2,const std::vector<int>&);

  extern std::vector<int> states_list;

  std::vector<HMM::bitmask_t> get_bitpath(const data_partition& P, const std::vector<int>& nodes);
}




#endif
