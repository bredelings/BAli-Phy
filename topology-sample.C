#include <cmath>
#include <valarray>
#include <algorithm>
#include "sample.H"
#include <iostream>
#include "likelihood.H"
#include "logsum.H"
#include "choose.H"
#include "bits.H"

//FIXME - these files are so complicated that we NEED to check the 
// results
//CHECK - all those states that can't be reached - can s2_ == s2?
//FIGURE OUT - how to factor in substitution differences?
//CHECK (how?) - do the differences for both +/- (5 alignments) and 
//               different substitution scores, match?


using std::valarray;

//need 10 bits (2+2+2+2+2) to remember saved stated for all 3 sub-alignments
const int nstates=1024;
const int present10=783;
const int present01=1008;
const int present11=1023;
const int statesmask = 1023;

namespace states {
  const int M  = 0;
  const int G1 = 1;
  const int G2 = 2;
  const int E  = 3;
};

bool legal(int bits) {
  if (not bitset(bits,4)) {
    int temp = 0;
    if (bitset(bits,0))
      temp++;
    if (bitset(bits,1))
      temp++;
    if (bitset(bits,2) or bitset(bits,3) or bitset(bits,5))
      temp++;
							     
    if (temp>1)   // not legal for n5
      return false;
  }
   
  if (not bitset(bits,5)) {
    int temp = 0;
    if (bitset(bits,2))
      temp++;
    if (bitset(bits,3))
      temp++;
    if (bitset(bits,0) or bitset(bits,1) or bitset(bits,4))
      temp++;
							     
    if (temp > 1) // not legal for n5
      return false;
  }
  return true;
}

static int bits_to_state(int bits,int b1,int b2) {
  if (not bitset(bits,b1)) {
    if (bitset(bits,b2))
      return states::G1;
    else
      return 3;
  }
  else {
    if (bitset(bits,b2))
      return states::M;
    else
      return states::G2;
  }
}

// Takes a bitmask of character-presence in sequences 0,1,2,3,4,5
// Returns the state for each alignment, 
//  with the validity of sub-alignments 0,1,2,3,4 marked in bits 10,11,12,13,14
static int get_column_states(int bits) {
  int S=0;
  S |= bits_to_state(bits,4,0)<<0;
  S |= bits_to_state(bits,4,1)<<2;
  S |= bits_to_state(bits,5,2)<<4;
  S |= bits_to_state(bits,5,3)<<6;
  S |= bits_to_state(bits,4,5)<<8;

  for(int i=0;i<5;i++) {
    if (bits_present(S>>(2*i),3)) { // if alignment i is missing
      S &= ~(3<<(2*i));      // clear its state
      S = setbit(S,10+i);    // and mark it missing
    }
  }
  return S;
}

// Takes the alignments for this column, and fill in the missing
//  states using states calculated from the previous column
int get_all_states(int all_states1,int column_states2) {
  int S = column_states2;
  for(int i=0;i<5;i++) {
    if (bitset(column_states2,10+i)) // alignment i is missing
      S |= (all_states1&(3<<(2*i)));
  }
  return S & statesmask;
}


// can we always recover the info that was lost in masking stuff out?
static int chain_to_alignments(int bits1,int s1) {
  bits1 = setbit(bits1,4, bits_present(s1,present10));
  bits1 = setbit(bits1,5, bits_present(s1,present01));

  int column_states1 = get_column_states(bits1);

  int saved_states1 = s1;
  if (bitset(bits1,4))   //zero out non-saved states
    saved_states1 &= ~present10;
  
  if (bitset(bits1,5))   //zero out non-saved states
    saved_states1 &= ~present01;

  int all_states1 = get_all_states(saved_states1,column_states1);

  return all_states1;
}


static int alignments_to_chain(int bits2,int all_states2) {
  int s2 = all_states2;

  if (bitset(bits2,4))   // save only state for 5-2,5-3
    s2 |= present10;
  if (bitset(bits2,5))   // save only state for 4-0,4-1
    s2 |= present01;

  assert(all_states2 == chain_to_alignments(bits2,s2));

  return s2;
}

static double p_move(int states1, int states2,const IndelModel& Theta) {
  double P=0;
  for(int i=0;i<5;i++) {
    if (not bitset(states2,10+i)) {    // this sub-alignment is present in this column
      int s1 = (states1>>(2*i))&3;
      int s2 = (states2>>(2*i))&3;
      P += Theta.Q[s1][s2];
    }
  }
  return P;
}

double get_DP_array(vector< vector<double> >& P, const vector<int>& bits, const IndelModel& IModel) {
  //bits2   = bitmask of presence of n1,n2,n3,n4
  //states2 = state of the sub-alignment HMMs at this location
  //s2      = state of this presence/absence chain at this location

  /************** Calculate DP array ********************/
  for(int character=1;character<P.size();character++) {
    for(int state=0;state<4;state++) {
      int bits2 = bits[character] | (state<<4);

      if (not legal(bits2))
	continue;

      for(int s1=0;s1<nstates;s1++) {
	if (P[character-1][s1] == log_0)
	  continue;

	// Get previous states (hidden, or calculated)
	int all_states1 = chain_to_alignments(bits[character-1],s1);

	// Get current states (only present ones)
	int column_states2 = get_column_states(bits2);

	double p = p_move(all_states1,column_states2,IModel);

	// Copy previous states into alignments where not present
	int all_states2 = get_all_states(all_states1,column_states2);
	int s2 = alignments_to_chain(bits2,all_states2);

	P[character][s2] = logsum(P[character][s2],p + P[character-1][s1]);
      }
    }
  }
  int endstates = (states::E<<8) | (states::E<<6) | (states::E<<4) | 
    (states::E<<2) | (states::E);
  double Pr=log_0;
  for(int s1=0;s1<nstates;s1++) {
    int all_states1 = chain_to_alignments(bits[bits.size()-1],s1);

    Pr = logsum(Pr,P[P.size()-1][s1] + p_move(all_states1,endstates,IModel));
  }
  return Pr;
}

vector<int> sample_path(const vector< vector<double> >& P,const vector<int>& bits,
			const IndelModel& IModel) {
  vector<int> path;
  path.reserve(P.size());

  vector<double> choices(nstates);
  int endstates = (states::E<<8) | (states::E<<6) | (states::E<<4) | 
    (states::E<<2) | (states::E);
  for(int s1=0;s1<nstates;s1++) {
    int all_states1 = chain_to_alignments(bits[bits.size()-1],s1);
    
    choices[s1] = P[P.size()-1][s1] + p_move(all_states1,endstates,IModel);
  }

  int s2 = choose(choices);
  for(int character=P.size()-1;;) {

    path.push_back(s2);

    character--;
    if (not (character>0)) break;

    int bits2 = bits[character+1];
    bits2 = setbit(bits2,4,bits_present(s2,present10));
    bits2 = setbit(bits2,5,bits_present(s2,present01));

    assert(legal(bits2));

    for(int s1=0;s1<nstates;s1++) {
      if (P[character][s1] == log_0) {
	choices[s1] = log_0;
	continue;
      }

      int all_states1 = chain_to_alignments(bits[character],s1);
      
      int column_states2 = get_column_states(bits2);

      int all_states2 = get_all_states(all_states1,column_states2);
      int s2_ = alignments_to_chain(bits2,all_states2);

      if (s2 == s2_) {
	double p = p_move(all_states1,column_states2,IModel);
	choices[s1] = P[character][s1] + p;
      }
      else
	choices[s1] = log_0;
    }
    
    s2 = choose(choices);
  }
  std::reverse(path.begin(),path.end());
  return path;
}

vector<int> get_path(const alignment& A,int n0,int n1,int n2,int n3,int n4,int n5) {
  vector<int> path;
  path.reserve(A.length());

  vector<int> bits;
  for(int column=0;column<A.length();column++) {
  }
  return bits;
}


void sample_topology(alignment& A,Parameters& Theta,int n4) {
  const SequenceTree& T = Theta.T;

  /****** Get the node names ********/
  assert(T[n4].left);
  assert(T[n4].right);

  int n5 = T.parent(n4);

  assert(T[n5].left);
  assert(T[n5].right);

  int n0 = T[n4].left->name;
  int n1 = T[n4].right->name;
  int n2 = T[n5].left->name;
  int n3 = T[n5].right->name;

  if (n2 == n4)
    n2 = T.parent(n5);
  else if (n3==n4)
    n3 = T.parent(n5);


  /****** Generate the Different Topologies *******/
  const alignment old = A;

  SequenceTree T2 = T;
  SequenceTree T3 = T;

  /* FIXME - problem with exchanging with parents of root
  T2.exchange(n1,n2);
  T3.exchange(n1,n3);
  */

  vector<int> bits;
  vector<int> columns;
  vector< vector<double> > P;

  // Set up initial conditions
  P.push_back(vector<double>(nstates,log_0));
  bits.push_back(0);
  
  for(int s1=0;s1<3;s1++)
    for(int s2=0;s2<3;s2++)
      for(int s3=0;s3<3;s3++) 
	for(int s4 = 0;s4<3;s4++)
	  for(int s5 = 0;s5<3;s5++) {
	    int h = (s5<<8)|(s4<<6)|(s3<<4)|(s2<<2)|s1;
	    P[0][h] = Theta.IModel.pi[s1] + Theta.IModel.pi[s2] + Theta.IModel.pi[s3]
	      + Theta.IModel.pi[s4] + Theta.IModel.pi[s5];
      }

  // Determine which characters are present in each neighboring sequence
  for(int column=0;column<A.length();column++) {
    int state=0;
    if (not A.gap(column,n0))
      state |= 1<<0;
    if (not A.gap(column,n1))
      state |= 1<<1;
    if (not A.gap(column,n2))
      state |= 1<<2;
    if (not A.gap(column,n3))
      state |= 1<<3;

    if (state) {
      P.push_back(vector<double>(nstates,log_0));
      bits.push_back(state);
      columns.push_back(column);
    }
  }

  // calculate the forward probabilities
  double Pr = get_DP_array(P,bits,Theta.IModel);

  // do traceback
  vector<int> path1 = get_path(old,n0,n1,n2,n3,n4,n5);
  vector<int> path2 = sample_path(P,bits,Theta.IModel);

  // modify the matrix
  for(int character=0;character<path2.size();character++) {
    int s = path2[character];
    if (bits_present(s,present10))
      A(columns[character],n4) = alphabet::not_gap;
    else
      A(columns[character],n4) = alphabet::gap;

    if (bits_present(s,present01))
      A(columns[character],n5) = alphabet::not_gap;
    else
      A(columns[character],n5) = alphabet::gap;
  }
  /****************** Do traceback ********************/
  std::cerr<<old<<endl<<endl;

  std::cerr<<A<<endl<<endl;
  double l1 = probability3(old,Theta);
  double l2 = probability3(A,Theta);

  std::cerr<<"L1 = "<<l1<<"    L2 = "<<l2<<std::endl;
  assert(valid(A));
}

