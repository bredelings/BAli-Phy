#include <valarray>
#include <algorithm>
#include "myrandom.H"
#include "sample.H"
#include <iostream>

using std::valarray;

const double max_float = 3.40282347e+38F;

const double log_0 = -max_float;

inline double logsum(double r, double q)
{
  if (fabs(r-q) > 308)
    return ((r > q) ? r : q);
  
  return (r + log(1 + exp(q - r)));
}


int num_gaps(int n) {
  int sum=0;
  for(int i=0;i<6;i++) 
    if (n & (1<<i)) sum++;
  return sum;
}

int num_shared(int a,int b) {
  return num_gaps(a&b);
}


int choose(double x, double y) {
  double sum = logsum(x,y);
  double r = log(myrandomf());
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

  double r = log(myrandomf()) + sum[sum.size()-1];

  for(int i=0;i<sum.size();i++) 
    if (r < sum[i])
      return i;
  assert(0);
}

int choose(const vector<double>& P) {
  return choose(P.begin(),P.size());
}


int mask(int i) {
  if (i&(1<<3))
    return 6;
  else if (i&(1<<4))
    return 5;
  else
    return 3;
}


alignment sample(const alignment& old,const Parameters& Theta,int node) {
  const tree& T = Theta.T;

  assert(T[node].left);
  assert(T[node].right);

  int n1 = T[node].left->name;
  int n2 = T[node].right->name;
  int n3 = T.parent(node);

  alignment A = old;

  vector< vector<double> > P;
  vector< vector<int> > indels;
  for(int i=0;i<A.length()+1;i++) {
    P.push_back(vector<double>(74,log_0));
    indels.push_back(vector<int>(2,0));
  }

  for(int column=0;column<A.length();column++) {
    int num_present=0;
    if (A(column,n1) != alphabet::gap)
      num_present++;
    if (A(column,n2) != alphabet::gap)
      num_present++;
    if (A(column,n3) != alphabet::gap)
      num_present++;

    if (num_present == 0) 
      indels[column+1][1] = (1<<30); //never
    else if (num_present == 3) 
      indels[column+1][0] = (1<<30); //never
    else if (num_present == 2) { 
      if (A(column,n1) == alphabet::gap) // 4-1
	indels[column+1][1] = 1<<0;
      if (A(column,n2) == alphabet::gap) // 4-2
	indels[column+1][1] = 1<<1;
      if (A(column,n3) == alphabet::gap) // 4-3
	indels[column+1][1] = 1<<2;
      indels[column+1][0] = (1<<30); //never
    } 
    else if (num_present == 1) {
      if (A(column,n1) != alphabet::gap) {
	indels[column+1][0] = 1<<3;           // 1-4
	indels[column+1][1] = (1<<1)|(1<<2);  // 4-2 & 4-3
      }
      if (A(column,n2) != alphabet::gap) {
	indels[column+1][0] = 1<<4;           // 2-4
	indels[column+1][1] = (1<<0)|(1<<2);  // 4-1 & 4-3
      }
      if (A(column,n3) != alphabet::gap) {
	indels[column+1][0] = 1<<5;           // 3-4
	indels[column+1][1] = (1<<0)|(1<<1);  // 4-1 & 4-2
      }
    }

  }

  P[0][63] = 0; /// 111-111
  P[0][71] = 0; /// 111
  P[0][73] = 0; /// 1

  // If we store 00+10->0 and 10+11->1 then we halve calls to logsum
  for(int column=1;column<P.size();column++) {
    for(int state=0;state<2;state++) {
      int indel = indels[column][state];
      
      if (indel == (1<<30))           // P=0
	continue;

      for(int i=0;i<8;i++) {
	if (P[column-1][64+i] == log_0)
	  continue;

	const int prev_state = (i==7);
	
	int prev_indel = indels[column-1][prev_state];

	int target=(i<<3)+7; // i ->111
	if (!state) {
	  if (prev_state)
	    target = (i<<3)+(prev_indel & mask(indel));
	  else
	    target = (i<<3)+(i & mask(indel));
	}
	
	int gaps = num_gaps(indel);
	
	int extended = num_shared(prev_indel,indel);
	if (i != 7) {
	  int extended2 = num_shared(prev_indel|i,indel);
	  if (extended2>extended)
	    assert(state == 1);
	  extended = extended2;
	}

	int opened = gaps - extended;
	
	double p = Theta.lambda_E*extended + Theta.lambda_O*opened;
	
	P[column][target] = logsum(P[column][target],p + P[column-1][64+i]);
      }
    }

    for(int i=0;i<8;i++) {
      for(int j=0;j<8;j++)
	P[column][64+i] = logsum(P[column][64+i],P[column][(j<<3)+i]);
    }

    for(int i=0;i<7;i++)
      P[column][72] = logsum(P[column][72],P[column][64+i]);

    P[column][73] = P[column][71];
  }

  vector<double> choices(8);
  int current = choose(P[P.size()-1].begin()+64,8);
  for(int i=P.size()-1;i>0;i--) {
    if (current==7) 
      A(i-1,node) = alphabet::not_gap;
    else
      A(i-1,node) = alphabet::gap;
      
    for(int j=0;j<8;j++)
      choices[j] = P[i][(j<<3)+current];
    int next = choose(choices);
    current = next;
  }

  assert(valid(A));
  return A;
}

alignment sample(const alignment& old,const Parameters& Theta) {
  alignment A;
  if (myrandomf() < 0.5) {
    int node1 = myrandom(0,Theta.T.num_nodes()-3);
    int node2 = Theta.T.parent(node1);
    A = sample(old,Theta,node1,node2);
  }
  else {
    int node = myrandom(Theta.T.leaves(),Theta.T.num_nodes()-2);
    A = sample(old,Theta,node);
  }
  return A;
}

