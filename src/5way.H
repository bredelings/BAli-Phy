#ifndef FIVEWAY_H
#define FIVEWAY_H

#include "mytypes.H"
#include "alignment.H"
#include "parameters.H"
#include "2way.H"



// Returns the state, with the validity of sub-alignments 1,2,3 marked in bits 6,7,8
namespace A5 {

  namespace states = A2::states;

  vector<int> get_nodes(const tree& T,int b);
  vector<int> get_nodes_random(const tree& T,int b);

  // 21 bits used total:
  // bits 5,4,3,2,1,0     = node present mask
  // bits 15,14,13/12,11/10,9/8,7/6    = sub-alignment state
  // bits 20,19,18,17,16    = sub-alignment not-present mask
  
  const unsigned int leafbitsmask = (1<<4)-1;
  const unsigned int bitsmask = (1<<6)-1;

  /// construct the list of states
  vector<int> construct_states();

  /// Get the vector of start probabilities
  vector<efloat_t> get_start_P(const vector<indel::PairHMM>& P,const vector<int>& br,
			       const vector<int>& states);

  int bits_to_substates(int bits);

  /// What is the index (and therefore, name) of state 'state'
  int findstate(int state,const vector<int>& states);

  /// Compute the probability of moving from state #S1 to state #S2
  efloat_t getQ(int S1,int S2,const vector<indel::PairHMM>& P,const vector<int>& br,
		const vector<int>& states);
  /// Create the full transition matrix
  eMatrix createQ(const vector<indel::PairHMM>& P,const vector<int>& br,const vector<int>& states);

  /// Construct a new alignment, from alignment 'old' and 5way alignment 'path'
  alignment construct(const alignment& old, const vector<int>& path, 
		      const vector<int>& nodes,const tree& T,
		      const vector< vector<int> >& seqs,
		      const vector<int>& states_list);

  /// Get the ordered list of columns present in 5way alignment of 'nodes'
  vector<int> getorder(const alignment& A,const vector<int>& nodes);

  alignment project(const alignment& A,const vector<int>& nodes);

  /// What path does 'A' determine through the 5way HMM?
  vector<int> get_path(const alignment& A,const vector<int>& nodes,const vector<int>& states);

  /// Return the log of the acceptance ration for moving from (A1,P1) -> (A2,P2)
  double log_correction(const alignment& A,const Parameters& P,const vector<int>& nodes);

  double log_acceptance_ratio(const alignment& A1,const Parameters& P1,const vector<int>&,
			      const alignment& A2,const Parameters& P2,const vector<int>&);

  extern vector<int> states_list;

};




#endif
