#include <valarray>
#include <iostream>
#include <cmath>
#include "sample.H"
#include "substitution.H"
#include "logsum.H"
#include "likelihood.H"
#include "choose.H"
#include "bits.H"

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




inline double Q(int S1,int S2,const IndelModel& IModel) {
  assert(0 <= S1 and S1 < nstates);
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

  return P;
}

inline double GQ(int S1,int S2,const IndelModel& IModel) {
  double q = Q(S1,S2,IModel);
  if (S1 == 7) {
    if (S2 == S1)
      q = log_0;
    else
      q += -log(1.0-exp(Q(S1,S1,IModel)));
  }
  return q;
  
}

inline double sum(const valarray<double>& v) {
  return v.sum();
}

class DPmatrix : public vector<Matrix> {
  int size1_;
  int size2_;
public:
  int size1() const {return size1_;}
  int size2() const {return size2_;}
  int nstates() const {return size();}

  DPmatrix(int nstates,int s1,int s2):vector<Matrix>(nstates,Matrix(s1,s2)) {
    size1_ = s1;
    size2_ = s2;
    
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

  inline void forward(int,int,const vector<int>&,const vector<int>&,const IndelModel&);


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

inline void DPmatrixHMM::forward(int i2,int c2,const vector<int>& j,const vector<int>& k,
				 const IndelModel& IModel) {

  assert(i2<size1());
  assert(c2<size2());

  int j2 = j[c2];
  int k2 = k[c2];

  for(int S2=0;S2<nstates();S2++) {
    Matrix& FS2 = (*this)[S2];

    //---------- Get (i1,j1,k1) ----------
    int i1 = i2 - di(S2);
    int j1 = j2 - dj(S2);
    int k1 = k2 - dk(S2);

    //------ Get c1, check if valid ------
    int c1 = c2;
    if (i1<0 or j1<0 or k1<0)
      continue;
    if (j1 == j2 and k1 == k2)
      ;
    else if (j1 == j[c2-1] and k1 == k[c2-1])
      c1 = c2-1;
    else 
      continue;

    //--- Compute Arrival Probability ----
    FS2(i2,c2) = log_0;
    for(int S1=0;S1<nstates();S1++) {
      Matrix& FS1 = (*this)[S1];

      double q = GQ(S1,S2,IModel);
      if (q == log_0) continue;

      FS2(i2,c2) = logsum(FS2(i2,c2),FS1(i1,c1) + q);
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



vector<int> sample_path(const DPmatrix& Matrices,const Parameters& Theta) {
  vector<int> path;

  const int I = Matrices.size1()-1;
  const int C = Matrices.size2()-1;
  int i = I;
  int c = C;

  int state2 = nstates;
  while (i>0 or c>0) {
    vector<double> transition(nstates);
    for(int state1=0;state1<nstates;state1++)
      transition[state1] = Matrices[state1](i,c)+GQ(state1,state2,Theta.IModel);

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


  const int newlength = path.size() + 
    (subA1.size()-seq1.size()) + (subA2.size() - seq2.size()) + (subA3.size() - seq3.size());

  alignment A = old;
  A.changelength(newlength);
  assert(A.length() == newlength);

  std::cerr<<"old = "<<old<<endl;

  int c1=0,c2=0,c3=0,c4=0,c5=0,c6=0,l=0;
  for(int column=0;column<A.length();column++) {
    //    std::cout<<column<<" "<<c1<<" "<<c2<<" "<<c3<<" "<<c4<<"  "<<c5<<"  "<<c6<<"  "<<l<<endl;

    assert(c1>=c2);
    assert(c3>=c4);
    assert(c5>=c6);
    assert(c1 <= subA1.size());
    assert(c3 <= subA2.size());
    assert(c5 <= subA3.size());

    if (c1 < subA1.size() and (c2 == seq1.size() or (c2<seq1.size() and subA1[c1] < seq1[c2]))) {
      for(int i=0;i<A.size2();i++) {
	if (group1[i])
	  A(column,i) = old(subA1[c1],i);
	else
	  A(column,i) = alphabet::gap;
      }
      c1++;
      assert(not all_gaps(A,column));
    }
    else if (c3 < subA2.size() and (c4 == seq2.size() or (c4<seq2.size() and subA2[c3] < seq2[c4]))) {
      for(int i=0;i<A.size2();i++) {
	if (group1[i])
	  A(column,i) = alphabet::gap;
	else
	  A(column,i) = old(subA2[c3],i);
      }
      c3++;
      assert(not all_gaps(A,column));
    }
    else if (c5 < subA2.size() and (c6 == seq2.size() or (c6<seq2.size() and subA2[c5] < seq2[c6]))) {
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
    std::cout<<column<<" "<<c1<<" "<<c2<<" "<<c3<<" "<<c4<<"  "<<c5<<"  "<<c6<<"  "<<l<<endl;
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

alignment tri_sample_alignment(const alignment& old,const Parameters& Theta,
			   int node1,int node2) {
  const tree& T = Theta.T;

  const vector<double>& pi = Theta.IModel.pi;
  const valarray<double>& frequency = Theta.frequencies();


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

  // Find sub-alignments and sequences
  vector<int> seq1;
  vector<int> seq2;
  vector<int> seq3;
  vector<int> seq23;
  for(int column=0;column<old.length();column++) {
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

    Matrices[S](0,0) = pi[s1]+pi[s2]+pi[s3];
  }

  //check that the sum of Matrices[S](0,0) is 1.

  //------------------ Compute the DP matrix ---------------------//
  const int maxlength = std::max(size1,size2);
  for(int n=1; n<maxlength; n++) {
    if (n<size2)
      for(int i=0;i<n and i<size1;i++) 
	Matrices.forward(i,n,jcol,kcol,Theta.IModel);

    if (n<size1)
      for(int i=0;i<=n and i<size2;i++)
	Matrices.forward(n,i,jcol,kcol,Theta.IModel);
  }


  //------------- Sample a path from the matrix -------------------//

  vector<int> path = sample_path(Matrices,Theta);

  alignment A = construct(old,path,n0,n1,n2,n3,T,seq1,seq2,seq3);
  /*--------------------------------------------------------------*/

  /*
  vector<int> path1 = get_path(old,node1,node2);
  vector<int> path2 = get_path(A,node1,node2);
  path.push_back(3);
  assert(path2 == path);

  double p1 = path_P(path1,M,G1,G2,Theta);
  double p2 = path_P(path2,M,G1,G2,Theta);

  double l1 = probability3(old,Theta);
  double l2 = probability3(A,Theta);

  double l1B = substitution(old,Theta) + prior_branch_HMM(old,Theta.IModel,node1,node2);
  double l2B = substitution(A  ,Theta) + prior_branch_HMM(A  ,Theta.IModel,node1,node2);
  double diff = p2-p1-(l2-l1);
  double rdiff = diff/(l2-l1);

  double diffB = p2-p1-(l2B-l1B);
  double rdiffB = diffB/(l2B-l1B);

  if (path1 != path2) {
    std::cerr<<"P1 = "<<p1<<"     P2 = "<<p2<<"     P2 - P1 = "<<p2-p1<<endl;
    std::cerr<<"L1 = "<<l1<<"     L2 = "<<l2<<"     L2 - L1 = "<<l2-l1<<endl;
    std::cerr<<"L1B = "<<l1B<<"     L2B = "<<l2B<<"     L2B - L1B = "<<l2B-l1B<<endl<<endl;
    std::cerr<<"diff = "<<diff<<std::endl;
    std::cerr<<"rdiff = "<<rdiff<<std::endl;
    std::cerr<<"rdiffB = "<<rdiffB<<std::endl;
    if (diff != 0.0) {
      if (std::abs(rdiff) > 1.0e-8) {
	old.print(std::cerr);
	A.print(std::cerr);
      }
    }
  }

  assert(isnan(rdiff) or abs(diff) < 1.0e-8);
  */

  /*--------------------------------------------------------------*/
  assert(valid(A));
  return A;
}


