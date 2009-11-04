/*
   Copyright (C) 2004-2007 Benjamin Redelings

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

#ifndef FIVEWAY_H
#define FIVEWAY_H

#include "mytypes.H"
#include "alignment.H"
#include "parameters.H"
#include "2way.H"



// Returns the state, with the validity of sub-alignments 1,2,3 marked in bits 6,7,8
namespace A5 {

  namespace states = A2::states;

  vector<int> get_nodes(const Tree& T,int b);
  vector<int> get_nodes_random(const Tree& T,int b);

  // 21 bits used total:
  // bits 5,4,3,2,1,0     = node present mask
  // bits 15,14,13/12,11/10,9/8,7/6    = sub-alignment state
  // bits 20,19,18,17,16    = sub-alignment not-present mask
  
  const unsigned int leafbitsmask = (1<<4)-1;
  const unsigned int bitsmask = (1<<6)-1;

  /// construct the list of states
  vector<int> construct_states();

  /// Get the vector of start probabilities
  vector<double> get_start_P(const vector<indel::PairHMM>& P,const vector<int>& br);

  int bits_to_substates(int bits);

  /// What is the index (and therefore, name) of state 'state'
  int findstate(int state,const vector<int>& states);

  /// Compute the probability of moving from state #S1 to state #S2
  double getQ(int S1,int S2,const vector<indel::PairHMM>& P,const vector<int>& br,
		const vector<int>& states);

  /// Update the full transition matrix
  void updateQ(Matrix& Q,const vector<indel::PairHMM>& P,const vector<int>& br,const vector<int>& states);
  /// Full the full transition matrix
  void fillQ(Matrix& Q,const vector<indel::PairHMM>& P,const vector<int>& br,const vector<int>& states);
  /// Create the full transition matrix
  Matrix createQ(const vector<indel::PairHMM>& P,const vector<int>& br,const vector<int>& states);

  /// Construct a new alignment, from alignment 'old' and 5way alignment 'path'
  alignment construct(const alignment& old, const vector<int>& path, 
		      const vector<int>& nodes,const Tree& T,
		      const vector< vector<int> >& seqs,
		      const vector<int>& states_list);

  /// Get the ordered list of columns present in 5way alignment of 'nodes'
  vector<int> getorder(const alignment& A,const vector<int>& nodes);

  alignment project(const alignment& A,const vector<int>& nodes);

  /// What path does 'A' determine through the 5way HMM?
  vector<int> get_path(const alignment& A,const vector<int>& nodes,const vector<int>& states);

  /// Return the log of the acceptance ration for moving from (A1,P1) -> (A2,P2)
  efloat_t correction(const data_partition& P,const vector<int>& nodes);

  efloat_t correction(const Parameters& P,const vector<int>& nodes);

  efloat_t acceptance_ratio(const Parameters& P1,const vector<int>&,
			      const Parameters& P2,const vector<int>&);

  extern vector<int> states_list;

}




#endif
