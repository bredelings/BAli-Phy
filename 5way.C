#include "5way.H"
#include "bits.H"
#include "logsum.H"

using std::valarray;

using namespace A5;

vector<int> get_path_5way(const alignment& A,int n0,int n1,int n2,int n3) {

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

namespace A5 {

  /********* Which nodes adjacent to this brranch *********/
  vector<int> get_branches(const alignment& A, const tree& T,int b) {
    vector<int> branches(5);
    
    branches[0] = b;
    int n0 = T.branch(b).child();
    int n1 = T.branch(b).parent();
    assert(n1 = T[n0].parent());
    
    // We assume an internal branch
    branches[1] = T[n0].left().branch_up();
    branches[2] = T[n0].right().branch_up();
    // This is only correct as-is if this is the root branch
    branches[3] = T[n1].left().branch_up();
    branches[4] = T[n1].right().branch_up();

    if (branches[3] == b)
      branches[3] = T[n1].branch_up();
    else if (branches[4] == b)
      branches[4] = T[n1].branch_up();
    
    return branches;
  }

  /********* Which nodes adjacent to this brranch *********/
  vector<int> get_nodes(const alignment& A, const tree& T,int b) {
    vector<int> nodes(6);
    
    nodes[0] = T.branch(b).child();
    nodes[1] = T.branch(b).parent();
    assert(nodes[5] = T[nodes[4]].parent());
    
    // This must be an internal branch
    nodes[2] = T[nodes[0]].left();
    nodes[3] = T[nodes[0]].right();
    nodes[4] = T[nodes[1]].left();
    nodes[5] = T[nodes[1]].right();
    
    if (nodes[4] == nodes[0])
      nodes[4] = T[nodes[1]].parent();
    else if (nodes[5]==nodes[0])
      nodes[5] = T[nodes[1]].parent();
    
    return nodes;
  }

  vector<int> get_state_emit() {
    vector<int> state_emit(nstates+1);
    for(int S2=0;S2<state_emit.size();S2++) {
      state_emit[S2] = 0;
      
      if (di(S2)) 
	state_emit[S2] |= (1<<0);
      
      if (dc(S2)) 
	state_emit[S2] |= (1<<1);
    }
    return state_emit;
  }


  vector<double> get_start_P(const vector<double>& pi) {
    int count = 0;
    double sum = log_0;

    vector<double> start_P(nstates,log_0);
    for(int S=0;S<start_P.size();S++) {
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
      
      start_P[S] = pi[s1] + pi[s2] + pi[s3];
      count++;
      sum = logsum(sum,start_P[S]);
    }    
    // check that the sum of Matrices[S](0,0) is 1, number of states examined is 27
#ifndef NDEBUG
    std::cerr<<"sum = "<<sum<<std::endl;
#endif
    assert(count==27);

    return start_P;
  }

/// Returns the state, with the validity of sub-alignments 1,2,3 marked in bits 6,7,8
int bits_to_states(int bits) {
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

int getstates(int S) {
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
  std::abort();
}

using namespace A5;

int di(int S) {
  S = getstates(S);
  if (S&(1<<1))
    return 1;
  else
    return 0;
}

int dj(int S) {
  S = getstates(S);
  if (S&(1<<2))
    return 1;
  else
    return 0;
}

int dk(int S) {
  S = getstates(S);
  if (S&(1<<3))
    return 1;
  else
    return 0;
}

int dc(int S) {
  if (dj(S)==0 and dk(S)==0)
    return 0;
  else
    return 1;
}

int dl(int S) {
  S = getstates(S);
  if (S&(1<<0))
    return 1;
  else
    return 0;
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

  //  std::cerr<<"new = "<<A<<endl;  
  //  std::cerr<<"new(reordered) = "<<project(A,n0,n1,n2,n3)<<endl;
  assert(valid(A));

  return A;
}



}
