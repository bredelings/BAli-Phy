#ifndef THREEWAY_H
#define THREEWAY_H

#include "mytypes.H"
#include "alignment.H"
#include "parameters.H"
#include "2way.H"


// Returns the state, with the validity of sub-alignments 1,2,3 marked in bits 6,7,8
namespace A3 {

  namespace states = A2::states;

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

  /// Does state S emit in sequence 1?
  int di(int S);
  /// Does state S emit in sequence 2?
  int dj(int S);
  /// Does state S emit in sequence 3?
  int dk(int S);
  /// Does state S emit in sequence 0?
  int dl(int S);
  /// Does state S emit in sequence 2 or 3?
  int dc(int S);

  double getQ(int S1,int S2,const IndelModel& IModel);
  Matrix createQ(const IndelModel& IModel);

  alignment construct(const alignment& old, const vector<int>& path, 
		      int n0,int n1,int n2,int n3,const tree& T,
		      const vector<int>& seq1,const vector<int>& seq2, const vector<int>& seq3);

  vector<int> getorder(const alignment& A,int n0,int n1,int n2,int n3);
  alignment project(const alignment& A1,int n0,int n1,int n2,int n3);
};


vector<int> get_path_3way(const alignment& A,int n0,int n1,int n2,int n3);


#endif
