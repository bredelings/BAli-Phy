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

static bool all_gaps(const alignment& A,int column,const valarray<bool>& mask) {
  for(int i=0;i<A.size2();i++)
    if (mask[i] and not A.gap(column,i))
      return false;
  return true;
}

static bool all_gaps(const alignment& A,int column) {
  for(int i=0;i<A.size2();i++)
    if (not A.gap(column,i))
      return false;
  return true;
}

namespace states {
  const int M  = 0;
  const int G1 = 1;
  const int G2 = 2;
  const int E  = 3;
};

// bits 3,2,1,0     = node present mask
// bits 98,76,54    = sub-alignment state
// bits 12,11,10    = sub-alignment not-present mask
const int nstates = 1+3+3+1+3*16;
const int bitsmask = 15;

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



inline int getstate(int S) {
  assert(0 <= S and S<nstates+1);

  if (S==nstates)
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
    if (S/16 == 0) 
      bits |= (1<<1);
    else if (S/16 == 1) 
      bits |= (1<<2);
    else if (S/16 == 2) 
      bits |= (1<<3);
    S = S%16;
  }

  //-------- Get states --------
  int states = bits_to_states(bits);
  for(int i=0;i<3;i++)
    if (not bitset(states,6+i)) {
      int s = S&3;
      S >>= 2;
      states |= (s<<(2*i));
    }

  //---------- Merge ----------
  return (states<<4)|bits;
}

inline int di(int S) {
  S = getstate(S);
  if (S&(1<<1))
    return 1;
  else
    return 0;
}

inline int dj(int S) {
  S = getstate(S);
  if (S&(1<<2))
    return 1;
  else
    return 0;
}

inline int dk(int S) {
  S = getstate(S);
  if (S&(1<<3))
    return 1;
  else
    return 0;
}

inline int dl(int S) {
  S = getstate(S);
  if (S&(1<<0))
    return 1;
  else
    return 0;
}


inline double getQ(int S1,int S2,const IndelModel& IModel) {
  assert(0 <= S1 and S1 < nstates+1);
  assert(0 <= S2 and S2 < nstates+1);

  int states1 = getstate(S1);
  int states2 = getstate(S2);

  // If states are unordered, then force numerical order
  //  - this means that sequence 3 comes first
  if (not (states1 & states2 & bitsmask))
    if (states1 & bitsmask >= states2 & bitsmask)
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

  if (S1==nstates) {
    if (S2==nstates)
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

  return Q;
}


Matrix createGQ(const IndelModel& IModel) {
  Matrix GQ(nstates+1,nstates+1);

  for(int i=0;i<GQ.size1();i++)
    for(int j=0;j<GQ.size2();j++)
      GQ(i,j) = getGQ(i,j,IModel);

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

  // FIXME   - we need to make silent states (e.g. state 7) last
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

      FS2(i2,c2) = logsum(FS2(i2,c2),FS1(i1,c1) + GQ(S1,S2));
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



vector<int> get_path_3way(const alignment& A,int node1, int node2) {

  //----- Store whether or not characters are present -----//
  vector<valarray<bool> > present(A.length());
  for(int c=0;c<A.length();c++) {
    present[c].resize(A.size2());
    for(int s=0;s<A.size2();s++)
      present[c][s] = not A.gap(c,s);
  }

  int A10,A20,A30;
  vector<int> state;
  state.reserve(A.length()+1);
  for(int column=0;column<A.length();column++) {
    if (A.gap(column,node1)) {
      if (A.gap(column,node2)) 
	continue;
      else
	state.push_back(1);
    }
    else {
      if (A.gap(column,node2))
	state.push_back(2);
      else
	state.push_back(0);
    }
  }
  
  state.push_back(3);
  return state;
}


vector<int> generalize(const vector<int>& path) {
  return path;
}

double path_P(const vector<int>& path, const DPmatrix& Matrices, const Matrix& GQ) {
  return 0;
}


vector<int> sample_path(const DPmatrix& Matrices,const Matrix& GQ) {
  vector<int> path;

  const int I = Matrices.size1()-1;
  const int C = Matrices.size2()-1;
  int i = I;
  int c = C;

  int state2 = nstates;
  while (i>0 or c>0) {
    vector<double> transition(nstates);
    for(int state1=0;state1<nstates;state1++)
      transition[state1] = Matrices[state1](i,c)+GQ(state1,state2);

    int state1 = choose(transition);

    if (di(state1)) i--;
    if (dj(state1) or dk(state1)) c--;

    path.push_back(state1);
    state2 = state1;

    assert(i>=0 and c>=0);
  }
  assert(i == 0 and c == 0);

  std::reverse(path.begin(),path.end());
  return path;
}

// Does this routine depend on order of unordered columns?
// - no, it depends on order of path[], except for filling in {subA}-{seq}.

alignment construct(const alignment& old, const vector<int>& path, 
		    int n0,int n1,int n2,int n3,const tree& T,
		    const vector<int>& seq1,const vector<int>& seq2, const vector<int>& seq3,
		    const vector<int>& order) {

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


  const int newlength = path.size() + 
    (subA1.size()-seq1.size()) + (subA2.size() - seq2.size()) + (subA3.size() - seq3.size());

  alignment A = old;
  A.changelength(newlength);
  assert(A.length() == newlength);

  int c1=0,c2=0,c3=0,c4=0,c5=0,c6=0,l=0;
  for(int column=0;column<A.length();column++) {
    std::cout<<column<<":  "<<c1<<" "<<c2<<"  "<<c3<<" "<<c4<<"   "<<c5<<"  "<<c6<<"  "<<l<<endl;

    assert(c1>=c2);
    assert(c3>=c4);
    assert(c5>=c6);
    assert(c1 <= subA1.size());
    assert(c3 <= subA2.size());
    assert(c5 <= subA3.size());

    if (c1<subA1.size() and c2<seq1.size())
      assert(order[subA1[c1]] <= order[seq1[c2]]);
    if (c3<subA2.size() and c4<seq2.size())
      assert(order[subA2[c3]] <= order[seq2[c4]]);
    if (c5<subA3.size() and c6<seq3.size())
      assert(order[subA3[c5]] <= order[seq3[c6]]);

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
	  // FIXME!! - this is SUPPOSED to be random
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
    std::cout<<column<<":  "<<c1<<" "<<c2<<"  "<<c3<<" "<<c4<<"   "<<c5<<"  "<<c6<<"  "<<l<<endl<<endl;
    assert(not all_gaps(A,column));
  }

  assert(c1 == subA1.size());
  assert(c2 == seq1.size());
  assert(c3 == subA2.size());
  assert(c4 == seq2.size());
  assert(c5 == subA3.size());
  assert(c6 == seq3.size());
  assert(l == path.size());

  for(int i=0;i<T.leaves();i++) 
    assert(A.seqlength(i) == old.seqlength(i));

  std::cerr<<"new = "<<A<<endl;  
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

// The order of the columns in the path through the current alignment
//   depends also on (say) sequences 0 and 1.  
// Though since we chose an order in which any column with bits 2 or 3
//   comes before columns without, perhaps this is moot

inline bool before(const valarray<bool>& v1,const valarray<bool>& v2,int n0,int n1,int n2,int n3) {
  assert(v1.size() == v2.size());

  for(int i=0;i<v1.size();i++)
    if (v1[i] and v2[i])
      return false;

  int bits1=0;
  if (v1[n0]) bits1 |= (1<<0);
  if (v1[n1]) bits1 |= (1<<1);
  if (v1[n2]) bits1 |= (1<<2);
  if (v1[n3]) bits1 |= (1<<3);

  int bits2=0;
  if (v2[n0]) bits2 |= (1<<0);
  if (v2[n1]) bits2 |= (1<<1);
  if (v2[n2]) bits2 |= (1<<2);
  if (v2[n3]) bits2 |= (1<<3);

  if (bits1 < bits2)
    return true;
  else
    return false;
}

vector<int> getorder(const alignment& A,int n0,int n1,int n2,int n3) {
  vector<int> order(A.length());
  for(int c=0;c<order.size();c++)
    order[c] = c;

  vector<valarray<bool> > present(A.length());
  for(int c=0;c<A.length();c++) {
    present[c].resize(A.size2());
    for(int s=0;s<A.size2();s++)
      present[c][s] = not A.gap(c,s);
  }
    
  int c=0;
  while(c<A.length()-1) {
    if (not before(present[order[c+1]],present[order[c]],n0,n1,n2,n3)) {
      c++;
      continue;
    }
    std::swap(order[c],order[c+1]);
    if (c>0) c--;
  }

  return order;
}

alignment tri_sample_alignment(const alignment& old,const Parameters& Theta,
			   int node1,int node2) {
  const tree& T = Theta.T;

  const vector<double>& pi = Theta.IModel.pi;
  const valarray<double>& frequency = Theta.frequencies();

  std::cerr<<"old = "<<old<<endl;


  /*---------------- Setup node names ------------------*/
  assert(node1 >= Theta.T.leaves());

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


  /*----------- Compute sequence properties ------------*/
  valarray<bool> group1 = T.partition(n0,n1);
  valarray<bool> group2 = T.partition(n0,n2);
  valarray<bool> group3 = T.partition(n0,n3);

  vector<int> orderi = getorder(old,n0,n1,n2,n3);
  vector<int> order = invert(orderi);

  // Find sub-alignments and sequences
  vector<int> seq1;
  vector<int> seq2;
  vector<int> seq3;
  vector<int> seq23;
  for(int o=0;o<old.length();o++) {
    int column = orderi[o];
    if (not old.gap(column,n1))
      seq1.push_back(column);
    if (not old.gap(column,n2))
      seq2.push_back(column);
    if (not old.gap(column,n3))
      seq3.push_back(column);

    if (not old.gap(column,n2) or not old.gap(column,n3))
      seq23.push_back(column);
  }

  vector<int> jcol(seq23.size()+1);
  vector<int> kcol(seq23.size()+1);

  //Does this work with the ordering constraint that I have now??

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


  //------------------ Precompute distributions at n0 ------------------//
  vector< valarray<double> > dists1 = distributions(old,Theta,seq1,n0,n1);
  vector< valarray<double> > dists23 = distributions23(old,Theta,seq23,n0,n1);


  //------------------- Create alignment matrices ----------------------//
  const int size1 = seq1.size()+1;
  const int size2 = seq23.size()+1;
  DPmatrixHMM Matrices(nstates,dists1,dists23,frequency);

  for(int S2=0;S2<nstates+1;S2++)
    Matrices.getstate[S2] = getstate(S2);

  for(int c2=0;c2<Matrices.size2();c2++) {
    int j2 = jcol[c2];
    int k2 = kcol[c2];
    for(int S2=0;S2<nstates;S2++) {

      int j1 = j2;
      int k1 = k2;

      //---------- Get (i1,j1,k1) ----------
      if (Matrices.getstate[S2]&(1<<2))
	j1--;
      if (Matrices.getstate[S2]&(1<<3))
	k1--;
      
      //------ Get c1, check if valid ------
      if (j1 == j2 and k1 == k2)
	Matrices.states(c2).push_back(S2);
      else if (j1 == jcol[c2-1] and k1 == kcol[c2-1])
	Matrices.states(c2).push_back(S2);
      else
	; // this state not allowed here
    }
  }


  //-------------------- Initialize the Start  ------------------------//
  for(int S=0;S<nstates;S++) {
    int states = getstate(S);
    int s1 = (states>>4)&3;
    int s2 = (states>>6)&3;
    int s3 = (states>>8)&3;

    // if we are using hidden states, only use one way
    if (s1 == states::G1) {
      if (not bitset(S,10)) continue;
    }
    else if (s2 == states::G1) {
      if (not bitset(S,11)) continue;
    }
    else if (s3 == states::G1) {
      if (not bitset(S,12)) continue;
    }

    Matrices[S](0,0) = pi[s1] + pi[s2] + pi[s3];
  }

  //check that the sum of Matrices[S](0,0) is 1, number of states examined is 27

  //------------------ Compute the DP matrix ---------------------//
  const Matrix& GQ = createGQ(Theta.IModel);

  const int maxlength = std::max(size1,size2);
  for(int n=1; n<maxlength; n++) {
    if (n<size2)
      for(int i=0;i<n and i<size1;i++) 
	Matrices.forward(i,n,GQ);

    if (n<size1)
      for(int i=0;i<=n and i<size2;i++)
	Matrices.forward(n,i,GQ);
  }


  //------------- Sample a path from the matrix -------------------//

  vector<int> path = sample_path(Matrices,GQ);

  alignment A = construct(old,path,n0,n1,n2,n3,T,seq1,seq2,seq3,order);


  double l1 = probability3(old,Theta);
  double l2 = probability3(A,Theta);
  assert(std::abs(l1-l2) < 1000);

  // check that the old path doesn't have probability zero?

  //------------- Check relative path probabilities ---------------//
  if (0) {
  path.push_back(nstates);

  vector<int> path_old = get_path_3way(old,n0,n1,n2,n3);
  vector<int> path_new = get_path_3way(A,n0,n1,n2,n3);

  vector<int> path_old_G = generalize(path_old);
  vector<int> path_new_G = generalize(path_new);
  assert(path_old_G == path);


  double p1 = path_P(path_old_G,Matrices,GQ);
  double p2 = path_P(path_new_G,Matrices,GQ);

  double diff = p2-p1-(l2-l1);
  double rdiff = diff/(l2-l1);

  if (path_old_G != path_new_G) {
    std::cerr<<"P1 = "<<p1<<"     P2 = "<<p2<<"     P2 - P1 = "<<p2-p1<<endl;
    std::cerr<<"L1 = "<<l1<<"     L2 = "<<l2<<"     L2 - L1 = "<<l2-l1<<endl;
    std::cerr<<"diff = "<<diff<<std::endl;
    if (diff != 0.0) {
      if (std::abs(rdiff) > 1.0e-8) {
	old.print(std::cerr);
	A.print(std::cerr);
      }
    }
  }

  assert(isnan(rdiff) or abs(diff) < 1.0e-8);
  }
  /*--------------------------------------------------------------*/
  assert(valid(A));
  return A;
}


