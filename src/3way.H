#ifndef THREEWAY_H
#define THREEWAY_H

#include "mytypes.H"
#include "alignment.H"
#include "parameters.H"
#include "2way.H"


// Returns the state, with the validity of sub-alignments 1,2,3 marked in bits 6,7,8
namespace A3 {

  namespace states = A2::states;

  vector<int> get_nodes(const Tree& T,int n0);
  vector<int> get_nodes_random(const Tree& T,int n0);

  vector<int> get_nodes_branch_random(const Tree& T,int n0,int n1);

// bits 3,2,1,0     = node present mask
// bits 98,76,54    = sub-alignment state
// bits 12,11,10    = sub-alignment not-present mask
  
// Three G1-type states, each of which stores state for two alignments
//  - these alignment can only have 3 states each (M,G1,G2, not E) and so
//    total 3*3 = 9 states.  

  const unsigned int leafbitsmask = (1<<3)-1;
  const unsigned int bitsmask = (1<<4)-1;

  extern vector<int> states_list;

  /// construct the list of states
  vector<int> construct_states();

  /// The number of states, minus the end state (state state also ignored)
  const int nstates = 1+3+3+1+3*9;
  /// The name of the E (end) state
  const int endstate = nstates;

  vector<int> get_state_emit();

  vector<double> get_start_P(const vector<indel::PairHMM>& P,const vector<int>& branches);

  int bits_to_states(int bits);

  int getstates(int S);

  int findstate(int states);

  /// Does state S emit in sequence 1?
  inline
  bool di(int S) {
    S = states_list[S];
    if (S&(1<<1))
      return 1;
    else
      return 0;
  }

  /// Does state S emit in sequence 2?
  inline
  bool dj(int S) {
    S = states_list[S];
    if (S&(1<<2))
      return 1;
    else
      return 0;
  }

  /// Does state S emit in sequence 3?
  inline
  bool dk(int S) {
    S = states_list[S];
    if (S&(1<<3))
      return 1;
    else
      return 0;
  }

  /// Does state S emit in sequence 0?
  inline
  bool dl(int S) {
    S = states_list[S];
    if (S&(1<<0))
      return 1;
    else
      return 0;
  }

  /// Does state S emit in sequence 2 or 3?
  inline 
  bool dc(int S) {
    if (dj(S)==0 and dk(S)==0)
      return 0;
    else
      return 1;
  }

  double getQ(int S1,int S2,const vector<indel::PairHMM>& P,const vector<int>& br);

  Matrix createQ(const vector<indel::PairHMM>& P,const vector<int>& br);

  alignment construct(const alignment& old, const vector<int>& path, 
		      int n0,int n1,int n2,int n3,const Tree& T,
		      const vector<int>& seq1,const vector<int>& seq2, const vector<int>& seq3);

  vector<int> getorder(const alignment& A,int n0,int n1,int n2,int n3);
  alignment project(const alignment& A1,int n0,int n1,int n2,int n3);
  alignment project(const alignment& A1,const vector<int>&);

  /// Return the log of the acceptance ration for moving from (A1,P1) -> (A2,P2)
  efloat_t correction(const data_partition& P,const vector<int>& nodes);

  /// Return the log of the acceptance ration for moving from (A1,P1) -> (A2,P2)
  efloat_t correction(const Parameters& P,const vector<int>& nodes);

  efloat_t acceptance_ratio(const Parameters& P1,const vector<int>&,
			    const Parameters& P2,const vector<int>&);

}


vector<int> get_path_3way(const alignment& A,int n0,int n1,int n2,int n3);
vector<int> get_path_3way(const alignment& A,const vector<int>& nodes);


#endif
