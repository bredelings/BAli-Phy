#include <valarray>
#include <iostream>
#include <cmath>
#include "sample.H"
#include "substitution.H"
#include "logsum.H"
#include "likelihood.H"
#include "choose.H"
#include "bits.H"
#include "util.H"
#include "myrandom.H"

//TODO - 1. calculate the probability of 
//  a) the path we came in with
//  b) the path we chose
//  c) the most probable path?

// 2. Calculate the likelihood of the reassembled matrix and the original matrix
//     - see if the difference is the same as the difference between the path probabilities

//Assumptions:
//  a) we assume that the internal node is the parent
//     sequence in each of the sub-alignments

using std::abs;
using std::valarray;

namespace states {
  const int M  = 0;
  const int G1 = 1;
  const int G2 = 2;
  const int E  = 3;
};

// bits 3,2,1,0     = node present mask
// bits 98,76,54    = sub-alignment state
// bits 12,11,10    = sub-alignment not-present mask

// Three G1-type states, each of which stores state for two alignments
//  - these alignment can only have 3 states each (M,G1,G2, not E) and so
//    total 3*3 = 9 states.  

const int nstates = 1+3+3+1+3*9;
const int endstate = nstates;
const int bitsmask = 15;
const int emitmask = 14;

// Returns the state, with the validity of sub-alignments 1,2,3 marked in bits 6,7,8
static int bits_to_states(int bits) {
  int S=(1<<6)|(1<<7)|(1<<8);
  if (not bitset(bits,0)) {
    if (bitset(bits,1))
      S |= (states::G1<<0);
    else
      S = clearbit(S,6);

    if (bitset(bits,2))
      S |= (states::G1<<2);
    else
      S = clearbit(S,7);

    if (bitset(bits,3))
      S |= (states::G1<<4);
    else
      S = clearbit(S,8);
  }
  else {
    if (bitset(bits,1))
      S |= (states::M<<0);
    else
      S |= (states::G2<<0);
      
    if (bitset(bits,2))
      S |= (states::M<<2);
    else
      S |= (states::G2<<2);

    if (bitset(bits,3))
      S |= (states::M<<4);
    else
      S |= (states::G2<<4);
  }
  return S;
}


inline int getstates(int S) {
  assert(0 <= S and S<nstates+1);

  if (S==endstate)
    return (1<<12)|(1<<11)|(1<<10)|(states::E<<8)|(states::E<<6)|(states::E<<4);

  int bits=0;

  //--------- Set bits ---------
  if (S<8) {
    //internal node present
    bits |= (1<<0);

    // other nodes present according to S
    bits |= ((~S)&7)<<1;
  }
  else {
    S -=8;
    if (S/9 == 0) 
      bits |= (1<<1);
    else if (S/9 == 1) 
      bits |= (1<<2);
    else if (S/9 == 2) 
      bits |= (1<<3);
    S = S%9;
  }

  //-------- Get states --------
  int states = bits_to_states(bits);
  for(int i=0;i<3;i++)
    if (not bitset(states,6+i)) {
      int s = S%3;
      S /= 3;
      states |= (s<<(2*i));
    }

  //---------- Merge ----------
  return (states<<4)|bits;
}

inline int findstate(int states) {
  unsigned int mask = ~((~0)<<10);
  for(int S=0;S<=nstates;S++) {
    if ((getstates(S)&mask) == (states&mask))
      return S;
  }
  //couldn't find it?
  assert(0);
}


inline int di(int S) {
  S = getstates(S);
  if (S&(1<<1))
    return 1;
  else
    return 0;
}

inline int dj(int S) {
  S = getstates(S);
  if (S&(1<<2))
    return 1;
  else
    return 0;
}

inline int dk(int S) {
  S = getstates(S);
  if (S&(1<<3))
    return 1;
  else
    return 0;
}

inline int dc(int S) {
  if (dj(S)==0 and dk(S)==0)
    return 0;
  else
    return 1;
}

inline int dl(int S) {
  S = getstates(S);
  if (S&(1<<0))
    return 1;
  else
    return 0;
}

bool silent(int S) {
  if (S==endstate)
    return false;

  int states = getstates(S);
  if (states&emitmask)
    return false;
  else
    return true;
}


inline double getQ(int S1,int S2,const IndelModel& IModel) {
  assert(0 <= S1 and S1 < nstates+1);
  assert(0 <= S2 and S2 < nstates+1);

  int states1 = getstates(S1);
  int states2 = getstates(S2);

  int ap1 = states1>>10;
  int ap2 = states2>>10;

  // If states are unordered, then force numerical order
  //  - this means that sequence 3 comes first
  // Note that the end state IS ordered - but this be handled here.
  if (not (ap1 & ap2) and (ap1>ap2))
      return log_0;

  double P=0;
  for(int i=0;i<3;i++) {
    int s1 = (states1>>(2*i+4))&3;
    int s2 = (states2>>(2*i+4))&3;
    if (bitset(states2,10+i))     // this sub-alignment is present in this column
      P += IModel.Q(s1,s2);
    else if (s1 != s2)            // require state info from s1 hidden in s2
      return log_0;
  }

  if (S1==endstate) {
    if (S2==endstate)
      assert(P==0);
    else
      assert(P<log_0/100);
  }

  return P;
}

inline double getGQ(int S1,int S2,const IndelModel& IModel) {
  double q = getQ(S1,S2,IModel);
  if (S1 == 7) {
    if (S2 == S1)
      q = log_0;
    else
      q += -log(1.0-exp(getQ(S1,S1,IModel)));
  }
  return q;
}

Matrix createQ(const IndelModel& IModel) {
  Matrix Q(nstates+1,nstates+1);

  for(int i=0;i<Q.size1();i++)
    for(int j=0;j<Q.size2();j++)
      Q(i,j) = getQ(i,j,IModel);


  for(int i=0;i<Q.size1();i++) {
    double sum = log_0;
    for(int j=0;j<Q.size2();j++)
      sum = logsum(sum,Q(i,j));
    //    assert(sum == 0);
  }

  return Q;
}


Matrix createGQ(const IndelModel& IModel) {
  Matrix GQ(nstates+1,nstates+1);

  for(int i=0;i<GQ.size1();i++)
    for(int j=0;j<GQ.size2();j++)
      GQ(i,j) = getGQ(i,j,IModel);

  for(int i=0;i<GQ.size1();i++) {
    double sum = log_0;
    for(int j=0;j<GQ.size2();j++)
      sum = logsum(sum,GQ(i,j));
    //    assert(sum == 0);
  }

  return GQ;
}


inline double sum(const valarray<double>& v) {
  return v.sum();
}

class DPmatrix : public vector<Matrix> {
  int size1_;
  int size2_;
  vector< vector<int> > allowed_states;
public:
  const vector<int>& states(int j) const {return allowed_states[j];}
  vector<int>& states(int j) {return allowed_states[j];}

  vector<int> getstate;

  int size1() const {return size1_;}
  int size2() const {return size2_;}
  int nstates() const {return size();}

  DPmatrix(int nstates,int s1,int s2):vector<Matrix>(nstates,Matrix(s1,s2)),
				      allowed_states(s2),getstate(nstates+1)
  {
    size1_ = s1;
    size2_ = s2;
    
    //----- zero-initialize matrices ------//
    for(int i=0;i<s1;i++)
      for(int j=0;j<s2;j++) 
	for(int S=0;S<nstates;S++)
	  (*this)[S](i,j)  = log_0;
  }
};

class DPmatrixHMM: public DPmatrix {

public:
  const vector< valarray<double> >& dists1;
  const vector< valarray<double> >& dists2;
  const valarray<double>& frequency;

  valarray<double> s1_sub;
  valarray<double> s2_sub;

  inline void forward(int,int,const Matrix& GQ);

  inline void forward(int,int,int,int,const Matrix& GQ);

  DPmatrixHMM(int nstates,
	      const vector< valarray<double> >& d1,const vector< valarray<double> >& d2, 
	      const valarray<double>& f):
    DPmatrix(nstates,d1.size()+1,d2.size()+1),
    dists1(d1),dists2(d2),frequency(f),s1_sub(d1.size()+1),s2_sub(d2.size()+1) 
  {
    for(int i=0;i<dists1.size();i++)  
      s1_sub[i] = log(sum( dists1[i] * frequency ));

    for(int i=0;i<dists2.size();i++)
      s2_sub[i] = log(sum( dists2[i] * frequency ));
  }
};

inline void DPmatrixHMM::forward(int x1,int y1,int x2,int y2,const Matrix& GQ) {
  const int maxdelta = std::max(x2-x1,y2-y1);

  for(int delta=1; delta<=maxdelta; delta++) {
    if (delta<size2())
      for(int i=0;i<delta and i<size1();i++) 
	forward(x1+i,y1+delta,GQ);

    if (delta<size1())
      for(int i=0;i<=delta and i<size2();i++)
	forward(x1+delta,y1+i,GQ);
  } 
 
}

inline void DPmatrixHMM::forward(int i2,int c2,const Matrix& GQ) {

  assert(i2<size1());
  assert(c2<size2());

  for(int i=0;i<states(c2).size();i++) {
    int S2 = states(c2)[i];
    Matrix& FS2 = (*this)[S2];

    int i1 = i2;
    int c1 = c2;
    
    if (getstate[S2]&(1<<1))
      i1--;
    if (getstate[S2]&((1<<2)|(1<<3)))
      c1--;

    // check that S2 is valid at c2?

    //--- Don't go off the boundary -----
    if (i1<0 or c1<0)
      continue;

    //--- Compute Arrival Probability ----
    FS2(i2,c2) = log_0;
    for(int j=0;j<states(c1).size();j++) {
      int S1 = states(c1)[j];
      Matrix& FS1 = (*this)[S1];

      FS2(i2,c2) = logsum(FS2(i2,c2), FS1(i1,c1) + GQ(S1,S2));
    }

    //--- Include Emission Probability----
    double sub;
    if (i1 != i2 and c1 != c2)
      sub = log(sum( dists1[i2-1] * frequency * dists2[c2-1] ));
    else if (i1 != i2)
      sub = s1_sub[i2-1];
    else if (c1 != c2)
      sub = s2_sub[c2-1];
    else          // silent state - nothing emitted
      sub = 0;


    FS2(i2,c2) += sub;
  }
}     

vector<int> get_path_3way(const alignment& A,int n0,int n1,int n2,int n3) {

  //----- Store whether or not characters are present -----//
  vector<int> present;
  for(int column=0;column<A.length();column++) {
    int bits=0;
    if (not A.gap(column,n0))
      bits |= (1<<0);
    if (not A.gap(column,n1))
      bits |= (1<<1);
    if (not A.gap(column,n2))
      bits |= (1<<2);
    if (not A.gap(column,n3))
      bits |= (1<<3);
    present.push_back(bits);
  }

  int A10 = states::M;
  int A20 = states::M;
  int A30 = states::M;

  vector<int> path;
  path.reserve(A.length()+1);
  for(int column=0;column<A.length();column++) {
    int bits = present[column];

    if (not bits) 
      continue;

    int states = bits_to_states(bits);
    if (states & (1<<6))
      A10 = (states>>0)&3;
    if (states & (1<<7))
      A20 = (states>>2)&3;
    if (states & (1<<8))
      A30 = (states>>4)&3;

    states |= (A30<<4)|(A20<<2)|(A10<<0);
    states = (states<<4)|bits;
    int S = findstate(states);
    path.push_back(S);
  }
  
  path.push_back(endstate);
  return path;
}

vector<int> generalize(const vector<int>& path) {
  vector<int> path_g = path;
  for(int i=path_g.size()-1;i>0;i--) {
    int S1 = path_g[i-1];
    int S2 = path_g[i];
    if (silent(S1) and silent(S2))
      path_g.erase(path_g.begin()+i);
  }
  return path_g;
}

// FIXME - this doesn't deal with when there is more than one silent state!
double generalize_P(const vector<int>& path, const Matrix& Q) {
  double Pr = 0;
  for(int i=1; i<path.size(); i++) {
    int S1 = path[i-1];
    int S2 = path[i];

    if (silent(S1)) {
      if (silent(S2))
	Pr += Q(S1,S2);
      else
	Pr += log(1.0-exp(Q(S1,S1)));
    }
  }
  return Pr;
}

double path_check(const vector<int>& path, const DPmatrix& Matrices, const Matrix& GQ) {

  double Pr=0;
  
  const int I = Matrices.size1()-1;
  const int C = Matrices.size2()-1;
  int i = 0;
  int c = 0;
  int l = 0;

  // we don't look at transitions FROM the end state, because we 
  //  bail at looking at transitions FROM (I,C) 
  // FIXME - but what if this is actually not E but 7?
  while(true) {
    assert(l<path.size());

    int state1 = path[l];

    if (di(state1)) i++;
    if (dj(state1) or dk(state1)) c++;

    if (state1 == endstate)
      break;

    int state2 = path[l+1];

    vector<double> transition(nstates);
    for(int s=0;s<nstates;s++)
      transition[s] = Matrices[s](i,c)+GQ(s,state2);

    double p = choose_P(state1,transition);
    assert(p > log_0/100);
    
    l++;
    Pr += p;

    assert(i<=I and c<=C);
  }
  assert(l == path.size()-1);
  assert(i == I and c == C);
  assert(Pr > log_0/100);

  return Pr;
}

vector<double> path_Q(const vector<int>& path,const DPmatrixHMM& Matrices,const Matrix& GQ) {
  double P_path=0;
  double P_sub=0;
  int i=0,c=0;
  for(int l=0;l<path.size();l++) {

    int state2 = path[l];
    if (di(state2))
      i++;
    if (dc(state2))
      c++;

    if (l == 0) {
      double sum=log_0;
      for(int S=0;S<nstates;S++)
	if (S != 7)
	  sum = logsum(sum,Matrices[S](0,0)+GQ(S,state2));
      P_path += sum;
    }
    else {
      P_path += GQ(path[l-1],state2);
    }

    double sub=0;
    if (di(state2) and dc(state2))
      sub = log(sum(Matrices.dists1[i-1] * Matrices.frequency * Matrices.dists2[c-1]));
    else if (di(state2))
      sub = Matrices.s1_sub[i-1];
    else if (dc(state2))
      sub = Matrices.s2_sub[c-1];

    P_sub += sub;
  }
  assert(i == Matrices.size1()-1 and c == Matrices.size2()-1);
  vector<double> p;
  p.push_back(P_path);
  p.push_back(P_sub);
  return p;
}


double path_P(const vector<int>& path, const DPmatrix& Matrices, const Matrix& GQ) {
  double P2 = path_check(path,Matrices,GQ);
  std::cerr<<"P(path)2 = "<<P2<<std::endl;

  const int I = Matrices.size1()-1;
  const int C = Matrices.size2()-1;
  int i = I;
  int c = C;
  double Pr=0;

  int l = path.size()-1;
  int state2 = path[l];

  //We should really stop when we reach the Start state.
  // - since the start state is simulated by a non-silent state
  //   NS(0,0) we should go negative
  // - but we would have to check path[-1] to see which state
  //   made sample_path go negative
  // - instead we can check if l==0 - we know that the start state
  //   is at path[-1]
  while (l>0) {

    vector<double> transition(nstates);
    for(int state1=0;state1<nstates;state1++)
      transition[state1] = Matrices[state1](i,c)+GQ(state1,state2);

    int state1 = path[l-1];
    double p = choose_P(state1,transition);
    assert(p > log_0/100);

    if (di(state1)) i--;
    if (dj(state1) or dk(state1)) c--;

    l--;
    state2 = state1;
    Pr += p;
  }
  assert(l == 0);
  assert(i == 0 and c == 0);

  // include probability of choosing 'Start' vs ---+ !
  vector<double> transition(nstates);
  for(int state1=0;state1<nstates;state1++)
    transition[state1] = Matrices[state1](0,0)+GQ(state1,state2);

  double p=log_0;
  for(int state1=0;state1<nstates;state1++)  
    if (not silent(state1))
      p = logsum(p, choose_P(state1,transition) );

  Pr += p;

  assert(Pr > log_0);
  std::cerr<<"P(path) = "<<Pr<<std::endl;
  return Pr;
}


vector<int> sample_path(const DPmatrix& Matrices,const Matrix& GQ) {
  vector<int> path;

  const int I = Matrices.size1()-1;
  const int C = Matrices.size2()-1;
  int i = I;
  int c = C;

  int state2 = endstate;

  //We should really stop when we reach the Start state.
  // - since the start state is simulated by a non-silent state
  //   NS(0,0) we should go negative
  // - check that we came from (0,0) though
  while (i>=0 and c>=0) {
    path.push_back(state2);
    vector<double> transition(nstates);
    for(int state1=0;state1<nstates;state1++)
      transition[state1] = Matrices[state1](i,c)+GQ(state1,state2);

    int state1 = choose(transition);

    if (di(state1)) i--;
    if (dj(state1) or dk(state1)) c--;

    state2 = state1;
  }
  assert(i+di(state2)==0 and c+dc(state2)==0);

  std::reverse(path.begin(),path.end());
  return path;
}

// Does this routine depend on order of unordered columns?
//  - No: columns in subA1 but not in seq1 are ordered only in respect to columns in subA1
//  - columns in seq1, seq2, and seq3 should remain in increasing order.

alignment construct(const alignment& old, const vector<int>& path, 
		    int n0,int n1,int n2,int n3,const tree& T,
		    const vector<int>& seq1,const vector<int>& seq2, const vector<int>& seq3) {

  valarray<bool> group1 = T.partition(n0,n1);
  valarray<bool> group2 = T.partition(n0,n2);
  valarray<bool> group3 = T.partition(n0,n3);

  vector<int> subA1;
  vector<int> subA2;
  vector<int> subA3;
  for(int column=0;column<old.length();column++) {
    if (not all_gaps(old,column,group1))
      subA1.push_back(column);
    if (not all_gaps(old,column,group2))
      subA2.push_back(column);
    if (not all_gaps(old,column,group3))
      subA3.push_back(column);
  }

  
  // Account for silent end state with "-1"
  const int newlength = path.size() - 1 + 
    (subA1.size()-seq1.size()) + (subA2.size() - seq2.size()) + (subA3.size() - seq3.size());

  alignment A = old;
  A.changelength(newlength);
  assert(A.length() == newlength);

  int c1=0,c2=0,c3=0,c4=0,c5=0,c6=0,l=0;
  for(int column=0;column<A.length();column++) {
    //    std::cout<<column<<":  "<<c1<<" "<<c2<<"  "<<c3<<" "<<c4<<"   "<<c5<<"  "<<c6<<"  "<<l<<endl;

    assert(c1>=c2);
    assert(c3>=c4);
    assert(c5>=c6);
    assert(c1 <= subA1.size());
    assert(c3 <= subA2.size());
    assert(c5 <= subA3.size());

    if (c1 < subA1.size() and (c2 == seq1.size() or (c2<seq1.size() and subA1[c1] != seq1[c2]))) {
      for(int i=0;i<A.size2();i++) {
	if (group1[i])
	  A(column,i) = old(subA1[c1],i);
	else
	  A(column,i) = alphabet::gap;
      }
      c1++;
      assert(not all_gaps(A,column));
    }
    else if (c3 < subA2.size() and (c4 == seq2.size() or (c4<seq2.size() and subA2[c3] != seq2[c4]))) {
      for(int i=0;i<A.size2();i++) {
	if (group1[i])
	  A(column,i) = alphabet::gap;
	else
	  A(column,i) = old(subA2[c3],i);
      }
      c3++;
      assert(not all_gaps(A,column));
    }
    else if (c5 < subA3.size() and (c6 == seq3.size() or (c6<seq3.size() and subA3[c5] != seq3[c6]))) {
      for(int i=0;i<A.size2();i++) {
	if (group1[i])
	  A(column,i) = alphabet::gap;
	else
	  A(column,i) = old(subA3[c5],i);
      }
      c5++;
      assert(not all_gaps(A,column));
    }
    else{
      for(int i=0;i<A.size2();i++) 
	A(column,i) = alphabet::gap;

      for(int i=0;i<A.size2();i++) {
	if (group1[i]) {
	  if (di(path[l]))
	    A(column,i) = old(seq1[c2],i);
	}
	else if (group2[i]) {
	  if (dj(path[l]))
	    A(column,i) = old(seq2[c4],i);
	}
	else if (group3[i]) {
	  if (dk(path[l]))
	    A(column,i) = old(seq3[c6],i);
	}
	else {
	  assert(i==n0);
	  if (dl(path[l]))
	    A(column,i) = alphabet::not_gap;
	}
      }

      if (di(path[l])) {c1++;c2++;}
      if (dj(path[l])) {c3++;c4++;}
      if (dk(path[l])) {c5++;c6++;}
      l++;
      assert(not all_gaps(A,column));
    }
    //    std::cout<<column<<":  "<<c1<<" "<<c2<<"  "<<c3<<" "<<c4<<"   "<<c5<<"  "<<c6<<"  "<<l<<endl<<endl;
    assert(not all_gaps(A,column));
  }

  assert(c1 == subA1.size());
  assert(c2 == seq1.size());
  assert(c3 == subA2.size());
  assert(c4 == seq2.size());
  assert(c5 == subA3.size());
  assert(c6 == seq3.size());
  assert(l == path.size()-1);

  for(int i=0;i<T.leaves();i++) 
    assert(A.seqlength(i) == old.seqlength(i));

  std::cerr<<"new = "<<A<<endl;  
  //  std::cerr<<"new(reordered) = "<<project(A,n0,n1,n2,n3)<<endl;
  assert(valid(A));

  return A;
}


vector<valarray<double> > distributions(const alignment& A,const Parameters& Theta,
					const vector<int>& seq,int n0,int n1) {
  const alphabet& a = A.get_alphabet();

  vector< valarray<double> > dist(seq.size());

  for(int i=0;i<dist.size();i++) {
    vector<int> residues(A.size2());
    for(int j=0;j<residues.size();j++)
      residues[j] = A(seq[i],j);
    dist[i].resize(a.size());
    dist[i] = peel(residues,Theta,n0,n1,n0);

    // note: we could normalize frequencies to sum to 1
  }

  return dist;
}

vector<valarray<double> > distributions23(const alignment& A,const Parameters& Theta,
					  const vector<int>& seq,int n0,int n1) {
  const alphabet& a = A.get_alphabet();

  vector< valarray<double> > dist(seq.size());

  for(int i=0;i<dist.size();i++) {
    vector<int> residues(A.size2());
    for(int j=0;j<residues.size();j++)
      residues[j] = A(seq[i],j);
    dist[i].resize(a.size());
    dist[i] = peel(residues,Theta,n1,n0,n0);

    // note: we could normalize frequencies to sum to 1
  }

  return dist;
}

// If we are just getting the order of the columns in the 3-way alignment
// the this shouldn't affect anything else, should it??
vector<int> getorder(const alignment& A,int n0,int n1,int n2,int n3) {

  vector<int> columns;
  vector<int> AP;                     // alignments present

  //----- Record which sub-alignments present per column ------//
  for(int column=0;column<A.length();column++) {
    int bits=0;
    if (not A.gap(column,n0))
      bits |= (1<<0);
    if (not A.gap(column,n1))
      bits |= (1<<1);
    if (not A.gap(column,n2))
      bits |= (1<<2);
    if (not A.gap(column,n3))
      bits |= (1<<3);

    int states = bits_to_states(bits);
    int ap = states>>6;
    AP.push_back(ap);
    if (ap) {
      columns.push_back(column);
    }
  }

  //-------- Re-order unordered columns by AP order ---------//
  for(int i=0;i<columns.size()-1;) {
    int ap1 = AP[columns[i  ]];
    int ap2 = AP[columns[i+1]];
    if (not (ap1&ap2) and ap1 > ap2) {
      std::swap(columns[i],columns[i+1]);
      if (i>0) i--;
    }
    else
      i++;
  }

  return columns;
}

alignment project(const alignment& A1,int n0,int n1,int n2,int n3) {
  alignment A2;
  A2.add_sequence(A1.seq(n0));
  A2.add_sequence(A1.seq(n1));
  A2.add_sequence(A1.seq(n2));
  A2.add_sequence(A1.seq(n3));

  vector<int> columns = getorder(A1,n0,n1,n2,n3);
  A2.changelength(columns.size());
  for(int i=0;i<A2.length();i++) {
    A2(i,0) = A1(columns[i],n0);
    A2(i,1) = A1(columns[i],n1);
    A2(i,2) = A1(columns[i],n2);
    A2(i,3) = A1(columns[i],n3);
  }

  return A2;
}


alignment tri_sample_alignment(const alignment& old,const Parameters& Theta,
			   int node1,int node2) {
  const tree& T = Theta.T;

  const vector<double>& pi = Theta.IModel.pi;
  const valarray<double>& frequency = Theta.frequencies();

  std::cerr<<"old = "<<old<<endl;


  /*---------------- Setup node names ------------------*/
  assert(node1 >= T.leaves());

  int n0 = node1;
  int n1 = T[n0].parent();
  int n2 = T[n0].left();
  int n3 = T[n0].right();

  if (node2 == n1)
    ;
  else if (node2 == n2)
    std::swap(n1,n2);
  else if (node2 == n3)
    std::swap(n1,n3);
  else
    assert(0);


  /*------------- Compute sequence properties --------------*/
  valarray<bool> group1 = T.partition(n0,n1);
  valarray<bool> group2 = T.partition(n0,n2);
  valarray<bool> group3 = T.partition(n0,n3);

  vector<int> columns = getorder(old,n0,n1,n2,n3);

  std::cerr<<"n0 = "<<n0<<"   n1 = "<<n1<<"    n2 = "<<n2<<"    n3 = "<<n3<<std::endl;
  std::cerr<<"old (reordered) = "<<project(old,n0,n1,n2,n3)<<endl;

  // Find sub-alignments and sequences
  vector<int> seq1;
  vector<int> seq2;
  vector<int> seq3;
  vector<int> seq23;
  for(int i=0;i<columns.size();i++) {
    int column = columns[i];
    if (not old.gap(column,n1))
      seq1.push_back(column);
    if (not old.gap(column,n2))
      seq2.push_back(column);
    if (not old.gap(column,n3))
      seq3.push_back(column);

    if (not old.gap(column,n2) or not old.gap(column,n3))
      seq23.push_back(column);
  }

  // Map columns with n2 or n3 to single index 'c'
  vector<int> jcol(seq23.size()+1);
  vector<int> kcol(seq23.size()+1);

  jcol[0] = 0;
  kcol[0] = 0;
  for(int c=1,j=0,k=0;c<seq23.size()+1;c++) {
    if (not old.gap(seq23[c-1],n2))
      j++;    
    if (not old.gap(seq23[c-1],n3))
      k++;
    jcol[c] = j;
    kcol[c] = k;
  }


  // Precompute distributions at n0
  vector< valarray<double> > dists1 = distributions(old,Theta,seq1,n0,n1);
  vector< valarray<double> > dists23 = distributions23(old,Theta,seq23,n0,n1);


  /*-------------- Create alignment matrices ---------------*/
  DPmatrixHMM Matrices(nstates,dists1,dists23,frequency);

  // Cache state info
  for(int S2=0;S2<nstates+1;S2++)
    Matrices.getstate[S2] = getstates(S2);

  // Determine state order
  vector<int> state_order(nstates);
  for(int i=0;i<state_order.size();i++)
    state_order[i] = i;
  std::swap(state_order[7],state_order[nstates-1]);        // silent states must be last

  // Determine which states are allowed to match (,c2)
  for(int c2=0;c2<Matrices.size2();c2++) {
    int j2 = jcol[c2];
    int k2 = kcol[c2];
    for(int i=0;i<state_order.size();i++) {
      int S2 = state_order[i];

      int j1 = j2;
      int k1 = k2;

      //---------- Get (,j1,k1) ----------
      if (Matrices.getstate[S2]&(1<<2))
	j1--;
      if (Matrices.getstate[S2]&(1<<3))
	k1--;
      
      //------ Get c1, check if valid ------
      if (c2==0)
	Matrices.states(c2).push_back(S2);
      else if (j1 == j2 and k1 == k2)
	Matrices.states(c2).push_back(S2);
      else if (j1 == jcol[c2-1] and k1 == kcol[c2-1])
	Matrices.states(c2).push_back(S2);
      else
	; // this state not allowed here
    }
  }


  // Initialize the start probabilities at (0,0)
  double sum=log_0;
  int count = 0;
  for(int S=0;S<nstates;S++) {
    int states = getstates(S);
    int s1 = (states>>4)&3;
    int s2 = (states>>6)&3;
    int s3 = (states>>8)&3;

    if (s1 == states::E or s2 == states::E or s3 == states::E)
      continue;

    // if we are using hidden states, only use one way
    if (s1 == states::G1) {
      if (not bitset(states,10)) continue;
    }
    else if (s2 == states::G1) {
      if (not bitset(states,11)) continue;
    }
    else if (s3 == states::G1) {
      if (not bitset(states,12)) continue;
    }

    Matrices[S](0,0) = pi[s1] + pi[s2] + pi[s3];
    count++;
    sum = logsum(sum,Matrices[S](0,0));
  }

  // check that the sum of Matrices[S](0,0) is 1, number of states examined is 27
  std::cerr<<"sum = "<<sum<<std::endl;
  assert(count==27);


  /*------------------ Compute the DP matrix ---------------------*/
  const Matrix& GQ = createGQ(Theta.IModel);
  const Matrix& Q = createQ(Theta.IModel);

  // Since we are using M(0,0) instead of S(0,0), we need this hack to get ---+(0,0)
  // We can only use non-silent states at (0,0) to simulate S
  Matrices.forward(0,0,GQ);
  
  Matrices.forward(0,0,seq1.size(),seq23.size(),GQ);

  //------------- Sample a path from the matrix -------------------//

  vector<int> path = sample_path(Matrices,GQ);

  // FIXME!! - need to insert extra ---+ states according to a geometric
  alignment A = construct(old,path,n0,n1,n2,n3,T,seq1,seq2,seq3);

  //------------- Check relative path probabilities ---------------//

  // get the paths through the 3way alignment, from the entire alignment
  vector<int> path_old = get_path_3way(project(old,n0,n1,n2,n3),0,1,2,3);
  vector<int> path_new = get_path_3way(project(A,n0,n1,n2,n3),0,1,2,3);

  vector<int> path_old2 = get_path_3way(old,n0,n1,n2,n3);
  vector<int> path_new2 = get_path_3way(A,n0,n1,n2,n3);
  assert(path_new == path_new2); // <- this actually isn't necessary, we just need path_new

  // get the generalized paths - no sequential silent states that can loop
  vector<int> path_old_G = generalize(path_old);
  vector<int> path_new_G = generalize(path_new);
  assert(path_new_G == path);

  double PrOld=0;
  for(int i=0;i<path_old.size()-1;i++) {
    PrOld += Q(path_old[i],path_old[i+1]);
    assert(PrOld > log_0/100);
  }

  // get the likelihoods of the new alignments
  double s1 = substitution(old,Theta);
  double s2 = substitution(A,Theta);

  double l1 = prior_HMM_nogiven(old,Theta) + s1;
  double l2 = prior_HMM_nogiven(A  ,Theta) + s2;

  double a = prior_branch_HMM(project(old,n0,n1,n2,n3),Theta.IModel,0,1);
  double a2 = prior_branch_HMM(old,Theta.IModel,n0,n1);
  double b = prior_branch_HMM(project(old,n0,n1,n2,n3),Theta.IModel,0,2);
  double b2 = prior_branch_HMM(old,Theta.IModel,n0,n2);
  double c = prior_branch_HMM(project(old,n0,n1,n2,n3),Theta.IModel,0,3);
  double c2 = prior_branch_HMM(old,Theta.IModel,n0,n3);

  double lp1 = a + b + c;

  assert(abs(a-a2)<1.0e-7);
  assert(abs(b-b2)<1.0e-7);
  assert(abs(c-c2)<1.0e-7);

  double lp2 = prior_branch_HMM(project(A,n0,n1,n2,n3),Theta.IModel,0,1) +
    prior_branch_HMM(project(A,n0,n1,n2,n3),Theta.IModel,0,2) +
    prior_branch_HMM(project(A,n0,n1,n2,n3),Theta.IModel,0,3);

  // get the probabilities of sampling each of the paths
  double p1 = path_P(path_old_G,Matrices,GQ); 
  double p2 = path_P(path_new_G,Matrices,GQ); 

  // get the probabilities of the path through the 3-way HMM
  vector<double> QP = path_Q(path_old_G,Matrices,GQ);
  double qp1 = QP[0];
  double qs1 = QP[1];
  double q1 = qp1 + qs1;

  QP = path_Q(path_new_G,Matrices,GQ);
  double qp2 = QP[0];
  double qs2 = QP[1];
  double q2 = qp2 + qs2;

  // Adjust for length of n0 changing
  int length_old = old.seqlength(n0);
  int length_new = A.seqlength(n0);

    std::cerr<<"P1 = "<<p1<<"     P2 = "<<p2<<"     P2 - P1 = "<<p2-p1<<endl;
    std::cerr<<"Q1 = "<<q1<<"     Q2 = "<<q2<<"     Q2 - Q1 = "<<q2-q1<<endl;



    //  p1 -= 2.0*Theta.IModel.lengthp(length_old);
    //  p2 -= 2.0*Theta.IModel.lengthp(length_new);

  p1 += generalize_P(path_old,Q);
  p2 += generalize_P(path_new,Q);

  //  q1 -= 2.0*Theta.IModel.lengthp(length_old);
  //  q2 -= 2.0*Theta.IModel.lengthp(length_new);

  q1 += generalize_P(path_old,Q);
  q2 += generalize_P(path_new,Q);

  double diff = p2-p1-(l2-l1);
  double rdiff = diff/(l2-l1);

  if (path_old_G != path_new_G) {
    

    std::cerr<<"P1 = "<<p1<<"     P2 = "<<p2<<"     P2 - P1 = "<<p2-p1<<endl;
    std::cerr<<"Q1 = "<<q1<<"     Q2 = "<<q2<<"     Q2 - Q1 = "<<q2-q1<<endl;

    std::cerr<<"L1 = "<<l1<<"     L2 = "<<l2<<"     L2 - L1 = "<<l2-l1<<endl;
    std::cerr<<"diff = "<<diff<<std::endl;

    std::cerr<<endl;
    std::cerr<<"S1  = "<<s1<<"     S2  = "<<s2<<"     S2 - S1 = "<<s2-s1<<endl;
    std::cerr<<"QS1 = "<<qs1<<"     QS2 = "<<qs2<<"   QS2 - QS1 = "<<qs2-qs1<<endl;

    std::cerr<<endl;
    std::cerr<<"LP1 = "<<lp1<<"     LP2 = "<<lp2<<endl;
    std::cerr<<"QP1 = "<<qp1<<"     QP2 = "<<qp2<<endl;

    std::cerr<<prior_HMM_nogiven(old,Theta) - lp1<<endl;
    std::cerr<<prior_HMM_nogiven(A  ,Theta) - lp2<<endl;

    if (diff != 0.0) {
      if (std::abs(rdiff) > 1.0e-8) {
	old.print(std::cerr);
	A.print(std::cerr);

	project(old,n0,n1,n2,n3).print(std::cerr);
	project(A,n0,n1,n2,n3).print(std::cerr);
      }
    }
  }

  assert(isnan(rdiff) or abs(diff) < 1.0e-8);
  assert(valid(A));

  /*--------------------------------------------------------------*/

  double log_ratio = 2.0*(Theta.IModel.lengthp(length_new)-Theta.IModel.lengthp(length_old));
  if (myrandomf() < exp(log_ratio))
    return A;
  else
    return old;
}


