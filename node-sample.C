#include <cmath>
#include <valarray>
#include <algorithm>
#include "myrandom.H"
#include "sample.H"
#include <iostream>
#include "likelihood.H"
#include "logsum.H"

using std::valarray;

bool bitset(int m,int b) {
  return m&(1<<b);
}

int setbit(int m,int b) {
  return m|(1<<b);
}

int setbit(int m,int b,bool c) {
  if (c) m = setbit(m,b);
  return m;
}

int num_bits(int n) {
  int sum=0;
  for(int i=0;i<6;i++) 
    if (bitset(n,i)) sum++;
  return sum;
}

int choose(double x, double y) {
  double sum = logsum(x,y);
  double r = log_unif();
  if (sum + r < x)
    return 0;
  else
    return 1;
  assert(0);
}

int choose(vector<double>::const_iterator here,int size) {
  vector<double> sum(size);

  sum[0] = *here;here++;
  for(int i=1;i<sum.size();i++) {
    sum[i] = logsum(*here,sum[i-1]);
    here++;  
  }

  double r = log_unif() + sum[sum.size()-1];

  for(int i=0;i<sum.size();i++) 
    if (r < sum[i])
      return i;
  assert(0);
}

int choose(const vector<double>& P) {
  return choose(P.begin(),P.size());
}


//need 6 bits (2+2+2) to remember saved stated for all 3 sub-alignments
const int nstates=64;
const int present=63;
const int statesmask = 63;

namespace states {
  const int M  = 0;
  const int G1 = 1;
  const int G2 = 2;
  const int E  = 3;
};

// bits = mask of present nodes 1,2,3 (,4)
int mask(int bits) {
  assert(not bitset(bits,3));   //we can only states, if n4 = '-'

  int m=3; //state mask
  int M=0;
  if (not (bitset(bits,0)))
    M |= (m<<0);
  if (not (bitset(bits,1)))
    M |= (m<<2);
  if (not (bitset(bits,2)))
    M |= (m<<4);
  return M;
}

// Takes a bitmask of character presence in sequence 1,2,3,4
// Returns the state, with the validity of sub-alignments 1,2,3 marked in bits 6,7,8
int bits_to_states(int bits) {
  int S=0;
  if (not bitset(bits,3)) {
    if (bitset(bits,0))
      S |= (states::G1<<0);
    else
      S = setbit(S,6);

    if (bitset(bits,1))
      S |= (states::G1<<2);
    else
      S = setbit(S,7);

    if (bitset(bits,2))
      S |= (states::G1<<4);
    else
      S = setbit(S,8);
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

    if (bitset(bits,2))
      S |= (states::M<<4);
    else
      S |= (states::G2<<4);
  }
  return S;
}

int hidden(int states1,int bits2) {
  int s2=-1;
  if (bitset(bits2,3))
    s2=present;
  else {
    s2 = bits_to_states(bits2);
    s2 |= (states1 & mask(bits2));
  }

  return s2 & statesmask;
}


double p_move(int states1, int states2,const IndelModel& Theta) {
  double P=0;
  for(int i=0;i<3;i++) {
    if (not bitset(states2,6+i)) {    // this sub-alignment is present in this column
      int s1 = (states1>>(2*i))&3;
      int s2 = (states2>>(2*i))&3;
      P += Theta.Q[s1][s2];
    }
  }
  return P;
}


alignment sample(const alignment& old,const Parameters& Theta,int node) {
  const tree& T = Theta.T;

  assert(T[node].left);
  assert(T[node].right);

  int n1 = T.parent(node);
  int n2 = T[node].left->name;
  int n3 = T[node].right->name;

  alignment A = old;

  vector<int> bits;
  vector<int> columns;
  vector< vector<double> > P;

  // Set up initial conditions
  P.push_back(vector<double>(nstates,log_0));
  bits.push_back(0);
  columns.push_back(-1);
  
  for(int s1=0;s1<3;s1++)
    for(int s2=0;s2<3;s2++)
      for(int s3=0;s3<3;s3++) {
	int h = (s3<<4)|(s2<<2)|s1;
	P[0][h] = Theta.IModel.pi[s1] + Theta.IModel.pi[s2] + Theta.IModel.pi[s3];
      }

  // Determine which characters are present in each neighboring sequence
  for(int column=0;column<A.length();column++) {
    int state=0;
    if (not A.gap(column,n1))
      state |= 1<<0;
    if (not A.gap(column,n2))
      state |= 1<<1;
    if (not A.gap(column,n3))
      state |= 1<<2;

    if (state) {
      P.push_back(vector<double>(nstates,log_0));
      bits.push_back(state);
      columns.push_back(column);
    }
  }


  //bits2   = bitmask of presence of n1,n2,n3,n4
  //states2 = state of the sub-alignment HMMs at this location
  //s2      = state of this presence/absence chain at this location

  /************** Calculate DP array ********************/
  for(int character=1;character<P.size();character++) {
    int bits2 = bits[character];

    for(int state=0;state<2;state++) {
      
      if (state)
	bits2 = setbit(bits2,3);
      else if (num_bits(bits2)>1)
	continue;

      for(int s1=0;s1<nstates;s1++) {
	if (P[character-1][s1] == log_0)
	  continue;

	int states1 = s1;
	if (s1 == present) {
	  int bits1 = bits[character-1];
	  bits1 = setbit(bits1,3);
	  states1 = bits_to_states(bits1);
	}

	int s2 = hidden(states1,bits2);
	
	int states2 = bits_to_states(bits2);
	
	double p = p_move(states1,states2,Theta.IModel);
	
	P[character][s2] = logsum(P[character][s2],p + P[character-1][s1]);
      }
    }
  }

  /****************** Do traceback ********************/
  vector<double> choices(nstates);
  int endstates = (states::E<<4) | (states::E<<2) | (states::E);
  for(int s1=0;s1<nstates;s1++) {
    int states1 = s1;
    if (s1 == present) {
      int bits1 = bits[bits.size()-1];
      bits1 = setbit(bits1,3);
      states1 = bits_to_states(bits1);
    }
    
    choices[s1] = P[P.size()-1][s1] + p_move(states1,endstates,Theta.IModel);
  }

  std::cerr<<old<<endl<<endl;

  int s2 = choose(choices);
  for(int character=P.size()-1;;) {

    if (s2==present) 
      A(columns[character],node) = alphabet::not_gap;
    else
      A(columns[character],node) = alphabet::gap;

    character--;
    if (not (character>0)) break;

    int bits2 = setbit(bits[character+1],3,s2==present);

    for(int s1=0;s1<nstates;s1++) {
      int states1 = s1;
      if (s1 == present) {
	int bits1 = bits[character];
	bits1 = setbit(bits1,3);
	states1 = bits_to_states(bits1);
      }
      
      int s2_ = hidden(states1,bits2);

      int states2 = bits_to_states(bits2);

      double p = p_move(states1,states2,Theta.IModel);

      if (s2 == s2_)
	choices[s1] = P[character][s1] + p;
      else
	choices[s1] = log_0;
    }
    
    s2 = choose(choices);
  }

  std::cerr<<A<<endl<<endl;

  assert(valid(A));
  return A;
}

