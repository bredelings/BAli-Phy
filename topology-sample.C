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

// Is each alignment present?
static int mask(int bits) {
  int m=3; //state mask
  int M=0;
  if (not bitset(bits,4)) {
    if (not (bitset(bits,0)))
      M |= (m<<0);
    if (not (bitset(bits,1)))
      M |= (m<<2);

    // middle branch
    if (not bitset(bits,5))
      M |= (m<<8);
  }

  if (not bitset(bits,5)) {
    if (not (bitset(bits,2)))
      M |= (m<<4);
    if (not (bitset(bits,3)))
      M |= (m<<6);
  }
  return M;
}

// Takes a bitmask of character presence in sequence 0,1,2,3,4,5
// Returns the state, with the validity of sub-alignments 1,2,3,4,5
//    marked in bits 10,11,12,13,14
static int bits_to_states(int bits) {
  int S=0;
  if (not bitset(bits,4)) {
    if (bitset(bits,0))
      S |= (states::G1<<0);
    else
      S = setbit(S,10);

    if (bitset(bits,1))
      S |= (states::G1<<2);
    else
      S = setbit(S,11);

    if (bitset(bits,5))
      S |= (states::G1<<8);
    else
      S = setbit(S,14);
  }
  else {
    if (bitset(bits,0))
      S |= (states::M<<0);
    else
      S |= (states::G2<<0);
      
    if (bitset(bits,1))
      S |= (states::M<<2);
    else
      S |= (states::G2<<2);

    if (bitset(bits,5))
      S |= (states::M<<8);
    else
      S |= (states::G2<<8);
  }

  if (not bitset(bits,5)) {
    if (bitset(bits,2))
      S |= (states::G1<<4);
    else
      S = setbit(S,12);

    if (bitset(bits,3))
      S |= (states::G1<<6);
    else
      S = setbit(S,13);
  }
  else {
    if (bitset(bits,2))
      S |= (states::M<<4);
    else
      S |= (states::G2<<4);
      
    if (bitset(bits,3))
      S |= (states::M<<6);
    else
      S |= (states::G2<<6);
  }

  return S;
}


// IDEA/FIXME - we could pass in states2 instead of bits2
//  - mask(states2) would work fine (because of the extra markings)
//  - We would probably write '11' into states only if that alignment
//    can be recalculated - the overlap (in unhide) would go away.

// compute states for alignments 
//  - 0,1 if NOT n4, 
//  - 2,3 if NOT n5 
//  - 4   if NEITHER n4 or n5
static int hidden(int states1,int bits2) {

  // start with state from current column
  int s2 = bits_to_states(bits2);

  // add in state from previous column only if NO state in this column
  s2 |= (states1 & mask(bits2));

  // overwrite (with 11) state if 
  //  - 0,1 if n4, 
  //  - 2,3 if n5 
  //  - 4   if EITHER n4 OR n5

  if (bitset(bits2,4))
    s2 |= present10;
  if (bitset(bits2,5))
    s2 |= present01;

  return s2 & statesmask;
}

// compute previous states for each alignment (hidden, or not)
static int unhide(int s1,int bits1) {
  bits1 = setbit(bits1,4,s1 & present10 == present10);
  bits1 = setbit(bits1,5,s1 & present01 == present01);

  if (bitset(bits1,4))   //zero out non-saved states
    s1 &= ~present10;
  
  if (bitset(bits1,5))   //zero out non-saved states
    s1 &= ~present01;

  // Start with saved states
  int states1 = s1;      

  // Add in states calculated
  states1 |= (bits_to_states(bits1) & statesmask);

  return states1;
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

      {
	int temp = 0;
	if (bitset(bits2,0))
	  temp++;
	if (bitset(bits2,1))
	  temp++;
	if (bitset(bits2,2) or bitset(bits2,3) or bitset(bits2,5))
	  temp++;
							     
	if (not bitset(bits2,4) and temp > 1) // not legal for n4
	  continue;
      }

      {
	int temp = 0;
	if (bitset(bits2,2))
	  temp++;
	if (bitset(bits2,3))
	  temp++;
	if (bitset(bits2,0) or bitset(bits2,1) or bitset(bits2,4))
	  temp++;
							     
	if (not bitset(bits2,5) and temp > 1) // not legal for n5
	  continue;
      }

      for(int s1=0;s1<nstates;s1++) {
	if (P[character-1][s1] == log_0)
	  continue;

	// Get previous states (hidden, or calculated)
	int states1 = unhide(s1,bits[character-1]);

	// Get current states (only present ones)
	int states2 = bits_to_states(bits2);

	// Copy previous states into alignments where not present
	int s2 = hidden(states1,bits2);
	
	double p = p_move(states1,states2,IModel);
	
	P[character][s2] = logsum(P[character][s2],p + P[character-1][s1]);
      }
    }
  }
  int endstates = (states::E<<8) | (states::E<<6) | (states::E<<4) | 
    (states::E<<2) | (states::E);
  double Pr=log_0;
  for(int s1=0;s1<nstates;s1++) {
    int states1 = unhide(s1,bits[bits.size()-1]);

    Pr = logsum(Pr,P[P.size()-1][s1] + p_move(states1,endstates,IModel));
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
    int states1 = unhide(s1,bits[bits.size()-1]);
    
    choices[s1] = P[P.size()-1][s1] + p_move(states1,endstates,IModel);
  }

  int s2 = choose(choices);
  for(int character=P.size()-1;;) {

    path.push_back(s2);

    character--;
    if (not (character>0)) break;

    int bits2 = bits[character+1];
    bits2 = setbit(bits2,4,s2 & present10 == present10);
    bits2 = setbit(bits2,5,s2 & present01 == present01);

    for(int s1=0;s1<nstates;s1++) {
      int states1 = unhide(s1,bits[character]);
      
      int states2 = bits_to_states(bits2);

      int s2_ = hidden(states1,bits2);

      if (s2 == s2_) {
	double p = p_move(states1,states2,IModel);
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

void sample_topology(alignment& old,Parameters& Theta,int n4) {
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
  alignment A = old;

  SequenceTree T2 = T;
  T2.exchange(n1,n2);

  SequenceTree T3 = T;
  T3.exchange(n1,n3);

  vector<int> bits;
  vector<int> columns;
  vector< vector<double> > P;

  // Set up initial conditions
  P.push_back(vector<double>(nstates,log_0));
  bits.push_back(0);
  columns.push_back(-1);
  
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
  vector<int> path = sample_path(P,bits,Theta.IModel);

  // modify the matrix
  for(int character=0;character<path.size();character++) {
    int s = path[character];
    if (s & present10 == present10) 
      A(columns[character],n4) = alphabet::not_gap;
    else
      A(columns[character],n4) = alphabet::gap;

    if (s & present01 == present01) 
      A(columns[character],n5) = alphabet::not_gap;
    else
      A(columns[character],n5) = alphabet::gap;
  }
  /****************** Do traceback ********************/
  std::cerr<<old<<endl<<endl;

  std::cerr<<A<<endl<<endl;

  assert(valid(A));
}

