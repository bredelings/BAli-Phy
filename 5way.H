#ifndef TWAY_H
#define TWAY_H

#include "mytypes.H"
#include "alignment.H"
#include "parameters.H"

namespace states {
  const int M  = 0;
  const int G1 = 1;
  const int G2 = 2;
  const int E  = 3;
};

// Returns the state, with the validity of sub-alignments 1,2,3 marked in bits 6,7,8
namespace A5 {

  vector<int> get_branches(const alignment& A, const tree& T,int b);
  vector<int> get_nodes(const alignment& A, const tree& T,int b);

// bits 3,2,1,0     = node present mask
// bits 98,76,54    = sub-alignment state
// bits 12,11,10    = sub-alignment not-present mask
  
// Three G1-type states, each of which stores state for two alignments
//  - these alignment can only have 3 states each (M,G1,G2, not E) and so
//    total 3*3 = 9 states.  

  /// The number of states, minus the end state (state state also ignored)
  const int nstates = 1+3+3+1+3*9;
  /// The name of the E (end) state
  const int endstate = nstates;

  vector<int> get_state_emit();

  vector<double> get_start_P(const vector<double>&);

  int bits_to_states(int bits);

  int getstates(int S);

  int findstate(int states);

  /// Does state S emit in sequence 2?
  int di(int S);
  /// Does state S emit in sequence 3?
  int dj(int S);
  /// Does state S emit in sequence 4?
  int dk(int S);
  /// Does state S emit in sequence 5?
  int dl(int S);
  /// Does state S emit in sequence 0?
  int dm(int S);
  /// Does state S emit in sequence 1?
  int dn(int S);

  double getQ(int S1,int S2,const IndelModel& IModel);
  Matrix createQ(const IndelModel& IModel);

  alignment construct(const alignment& old, const vector<int>& path, 
		      const vector<int>& norder,const tree& T,
		      const vector< vector<int> >& seqs);

  vector<int> getorder(const alignment& A,const vector<int>& norder);
  alignment project(const alignment& A,const vector<int>& norder);
};


vector<int> get_path_5way(const alignment& A,const vector<int>& norder);


#endif
