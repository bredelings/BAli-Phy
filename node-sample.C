#include <cmath>
#include <valarray>
#include <algorithm>
#include "myrandom.H"
#include "sample.H"
#include <iostream>
#include "likelihood.H"

using std::valarray;

const double max_float = 3.40282347e+38F;

const double log_0 = -max_float;

inline double logsum(double r, double q)
{
  if (std::abs(r-q) > 308)
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


int mask(int i) {
  if (i&(1<<3))
    return 6;
  else if (i&(1<<4))
    return 5;
  else
    return 3;
}

// A. If current node is '-', then we have class I edges (e.g. 1->4)
// B. If current node is '+', then we have class II edges (e.g. 4->1)

// Edges can only be hidden if a node is '-' (case A)
// But only the second kind of edges can be hidden (case B)

int hidden(int hidden1,int state2,int indel1,int indel2) {
  assert(!(hidden1&(1<<30)));
  int state1 = (hidden1 == 7);

  int h=-1;
  if (state2 == 0) {
    if (state1 == 0)      // What edges hidden BY previous column?
      h = hidden1;        //  (None of its edges can be hidden)
    else if (state1 == 1)  // What edges exist IN previous column?
      h = indel1;         //  (It can't hide edges)

    //  Of those edges, this column can hide ones in mask(indel2)
    h &= mask(indel2);
  }
  else if (state2==1) 
    h = 7;

  return h;
}

double p_gap(int hidden1,int state2,int indel1, int indel2,const Parameters& Theta) {
  int gaps = num_gaps(indel2);
  
  int extended = num_shared(indel1, indel2);
  if (hidden1 != 7) 
    extended = num_shared(indel1|hidden1, indel2);

  int opened = gaps - extended;
  
  return Theta.lambda_E*extended + Theta.lambda_O*opened;
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
    P.push_back(vector<double>(8,log_0));
    indels.push_back(vector<int>(2,0));
  }
  P[0][7] = 0; /// In fake 0th column, the character is present
  indels[0][0] = 1<<30;

  // Determine which indels are present if the internal node at 'column'
  // is absent (indels[column][0]) or present (indels[column][1])
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


  /************** Calculate DP array ********************/
  for(int column=1;column<P.size();column++) {
    for(int state=0;state<2;state++) {
      int indel = indels[column][state];
      
      if (indel == (1<<30))           // P=0
	continue;

      for(int h1=0;h1<8;h1++) {
	if (P[column-1][h1] == log_0)
	  continue;

	int prev_indel = indels[column-1][h1==7];

	int h2 = hidden(h1,state,prev_indel,indel);
	
	double p = p_gap(h1,state,prev_indel,indel,Theta);
	
	P[column][h2] = logsum(P[column][h2],p + P[column-1][h1]);
      }
    }
  }

  /****************** Do traceback ********************/
  vector<double> choices(8);
  int current = choose(P[P.size()-1]);
  for(int i=P.size()-1;i>0;i--) {
    if (current==7) 
      A(i-1,node) = alphabet::not_gap;
    else
      A(i-1,node) = alphabet::gap;
      
    for(int h1=0;h1<8;h1++) {
      int prev_indel = indels[i-1][h1==7];
      int curr_indel = indels[i][current==7];
      if ((prev_indel&(1<<30)) or (curr_indel&(1<<30)) or
	  hidden(h1,current==7,prev_indel,curr_indel) != current)
	choices[h1] = log_0;
      else
	choices[h1] = P[i-1][h1] + p_gap(h1,current==7,prev_indel,curr_indel,Theta);
    }
    int next = choose(choices);
    current = next;
  }

  assert(valid(A));
  return A;
}

