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
#include "rng.H"
#include "3way.H"
#include "dpmatrix.H"


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

using namespace A3;

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

  // Cache which states emit which sequences
  for(int S2=0;S2<nstates+1;S2++) {
    Matrices.state_emit[S2] = 0;

    if (di(S2)) 
      Matrices.state_emit[S2] |= (1<<0);

    if (dc(S2)) 
      Matrices.state_emit[S2] |= (1<<1);
  }

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

      //---------- Get (,j1,k1) ----------
      int j1 = j2;
      if (dj(S2)) 
	j1--;

      int k1 = k2;
      if (dk(S2)) 
	k1--;
      
      //------ Get c1, check if valid ------
      if (c2==0 or (j1 == j2 and k1 == k2) or (j1 == jcol[c2-1] and k1 == kcol[c2-1]) )
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

  //FIXME - we should probably send the matrix 'Q' into DPmatrix, and construct GQ
  const Matrix& GQ = createGQ(Theta.IModel);
  const Matrix& Q = createQ(Theta.IModel);

  // Since we are using M(0,0) instead of S(0,0), we need this hack to get ---+(0,0)
  // We can only use non-silent states at (0,0) to simulate S
  Matrices.forward(0,0,GQ);
  
  Matrices.forward(0,0,seq1.size(),seq23.size(),GQ);

  //------------- Sample a path from the matrix -------------------//

  vector<int> path = Matrices.sample_path(GQ);

  // FIXME!! - need to insert extra ---+ states according to a geometric
  alignment A = construct(old,path,n0,n1,n2,n3,T,seq1,seq2,seq3);

#ifndef NDEBUG
  //--------------- Check alignment construction ------------------//

  // get the paths through the 3way alignment, from the entire alignment
  vector<int> path_old = get_path_3way(project(old,n0,n1,n2,n3),0,1,2,3);
  vector<int> path_new = get_path_3way(project(A,n0,n1,n2,n3),0,1,2,3);

  vector<int> path_old2 = get_path_3way(old,n0,n1,n2,n3);
  vector<int> path_new2 = get_path_3way(A,n0,n1,n2,n3);
  assert(path_new == path_new2); // <- current implementation probably guarantees this
                                 //    but its not a NECESSARY effect of the routine.

  // get the generalized paths - no sequential silent states that can loop
  vector<int> path_new_G = Matrices.generalize(path_new);
  assert(path_new_G == path);
  assert(valid(A));

  //-------------- Check relative path probabilities --------------//
  double s1 = substitution(old,Theta);
  double s2 = substitution(A,Theta);

  double lp1 = prior_branch_HMM(project(old,n0,n1,n2,n3),Theta.IModel,0,1) +
    prior_branch_HMM(project(old,n0,n1,n2,n3),Theta.IModel,0,2) +
    prior_branch_HMM(project(old,n0,n1,n2,n3),Theta.IModel,0,3);

  double lp2 = prior_branch_HMM(project(A,n0,n1,n2,n3),Theta.IModel,0,1) +
    prior_branch_HMM(project(A,n0,n1,n2,n3),Theta.IModel,0,2) +
    prior_branch_HMM(project(A,n0,n1,n2,n3),Theta.IModel,0,3);

  double diff = Matrices.check(Q,GQ,path_old,path_new,lp1,s1,lp2,s2);

  if (abs(diff) > 1.0e-9) {
    std::cerr<<prior_HMM_nogiven(old,Theta) - lp1<<endl;
    std::cerr<<prior_HMM_nogiven(A  ,Theta) - lp2<<endl;

    old.print(std::cerr);
    A.print(std::cerr);

    project(old,n0,n1,n2,n3).print(std::cerr);
    project(A,n0,n1,n2,n3).print(std::cerr);

    assert(0);
  }
#endif

  /*---------------- Adjust for length of n0 changing --------------------*/
  int length_old = old.seqlength(n0);
  int length_new = A.seqlength(n0);

  double log_ratio = 2.0*(Theta.IModel.lengthp(length_new)-Theta.IModel.lengthp(length_old));
  if (myrandomf() < exp(log_ratio))
    return A;
  else
    return old;
}


