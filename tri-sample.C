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

vector< vector<valarray<double> > > distributions(const alignment& A,const Parameters& P,
					const vector<int>& seq,int n0,int n1) {
  const alphabet& a = A.get_alphabet();
  const substitution::MultiRateModel& MRModel = P.SModel();

  vector< vector< valarray<double> > > dist(seq.size(),vector< valarray<double> >(MRModel.nrates()) );

  for(int i=0;i<dist.size();i++) {
    vector<int> residues(A.size2());
    for(int j=0;j<residues.size();j++)
      residues[j] = A(seq[i],j);
    for(int r=0;r<MRModel.nrates();r++) {
      dist[i][r].resize(a.size());
      dist[i][r] = substitution::peel(residues,
				      P.T,
				      MRModel.BaseModel(),
				      P.transition_P(r),
				      n0,n1,n0);
    }

    // note: we could normalize frequencies to sum to 1
  }

  return dist;
}

//FIXME - with a modified 'peel' routine, we could just pass in a 'group' valarray:
//  - voila: only one routine!
//  - then perhaps we could merge with the 'distributions' routine in branch-sample.C

vector< vector<valarray<double> > > distributions23(const alignment& A,const Parameters& P,
					  const vector<int>& seq,int n0,int n1) {
  const alphabet& a = A.get_alphabet();
  const substitution::MultiRateModel& MRModel = P.SModel();

  vector< vector< valarray<double> > > dist(seq.size(),vector< valarray<double> >(MRModel.nrates()) );

  for(int i=0;i<dist.size();i++) {
    vector<int> residues(A.size2());
    for(int j=0;j<residues.size();j++)
      residues[j] = A(seq[i],j);
    for(int r=0;r<MRModel.nrates();r++) {
      dist[i][r].resize(a.size());
      dist[i][r] = substitution::peel(residues,
				      P.T,
				      MRModel.BaseModel(),
				      P.transition_P(r),
				      n1,n0,n0);
    }

    // note: we could normalize frequencies to sum to 1
  }

  return dist;
}

//alignment tri_sample_alignment(const alignment& old,const Parameters& P,
//			       int node1,int node2) {


// FIXME - randomly try any order - but n0 has to be first, somehow
// Generate the division into n0|n1|n2,n3 independantly of the ordering

// FIXME - write a routine which takes On0,On1,On2,On3, and also n0,n1,n2,n3
//         make another routine which just take n0 (which could become
//         tri_sample_one() )

// FIXME - actually resample the path multiple times - pick one on
// opposite side of the middle 
alignment tri_sample_alignment(const alignment& old,const Parameters& P,
			   int node1,int node2) {
  const tree& T = P.T;

  const vector<double>& pi = P.IModel.pi;
  const valarray<double>& frequency = P.SModel().BaseModel().frequencies();

  //  std::cerr<<"old = "<<old<<endl;


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

  //  std::cerr<<"n0 = "<<n0<<"   n1 = "<<n1<<"    n2 = "<<n2<<"    n3 = "<<n3<<std::endl;
  //  std::cerr<<"old (reordered) = "<<project(old,n0,n1,n2,n3)<<endl;

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
  vector< vector< valarray<double> > > dists1 = distributions(old,P,seq1,n0,n1);
  vector< vector< valarray<double> > > dists23 = distributions23(old,P,seq23,n0,n1);


  /*-------------- Create alignment matrices ---------------*/

  // Cache which states emit which sequences
  vector<int> state_emit(nstates+1);
  for(int S2=0;S2<state_emit.size();S2++) {
    state_emit[S2] = 0;

    if (di(S2)) 
      state_emit[S2] |= (1<<0);

    if (dc(S2)) 
      state_emit[S2] |= (1<<1);
  }

  // Get the distribution which simulates the Start state
  double sum=log_0;
  int count = 0;
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
  std::cerr<<"sum = "<<sum<<std::endl;
  assert(count==27);


  const Matrix Q = createQ(P.IModel);

  // Actually create the Matrices & Chain
  DPmatrixHMM Matrices(state_emit,start_P,Q,
		       P.SModel().distribution(),dists1,dists23,frequency);

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


  /*------------------ Compute the DP matrix ---------------------*/

  if (P.features & (1<<0)) {
    vector<int> path_old = get_path_3way(project(old,n0,n1,n2,n3),0,1,2,3);
    Matrices.forward(path_old,P.constants[0]);
  }
  else {
    // Since we are using M(0,0) instead of S(0,0), we need this hack to get ---+(0,0)
    // We can only use non-silent states at (0,0) to simulate S
    Matrices.forward(0,0);
  
    Matrices.forward(0,0,seq1.size(),seq23.size());
  }

  //------------- Sample a path from the matrix -------------------//

  vector<int> path_g = Matrices.sample_path();
  vector<int> path = Matrices.ungeneralize(path_g);

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
  vector<int> path_new_g = Matrices.generalize(path_new);
  assert(path_new_g == path_g);
  assert(valid(A));

  //-------------- Check relative path probabilities --------------//
  double s1 = substitution::Pr(old,P);
  double s2 = substitution::Pr(A,P);

  double lp1 = prior_branch(project(old,n0,n1,n2,n3),P.IModel,0,1) +
    prior_branch(project(old,n0,n1,n2,n3),P.IModel,0,2) +
    prior_branch(project(old,n0,n1,n2,n3),P.IModel,0,3);

  double lp2 = prior_branch(project(A,n0,n1,n2,n3),P.IModel,0,1) +
    prior_branch(project(A,n0,n1,n2,n3),P.IModel,0,2) +
    prior_branch(project(A,n0,n1,n2,n3),P.IModel,0,3);

  double diff = Matrices.check(path_old,path_new,lp1,s1,lp2,s2);

  if (abs(diff) > 1.0e-9) {
    std::cerr<<prior_HMM_nogiven(old,P) - lp1<<endl;
    std::cerr<<prior_HMM_nogiven(A  ,P) - lp2<<endl;

    std::cerr<<old<<endl;
    std::cerr<<A<<endl;

    std::cerr<<project(old,n0,n1,n2,n3)<<endl;
    std::cerr<<project(A,n0,n1,n2,n3)<<endl;

    assert(0);
  }
#endif

  /*---------------- Adjust for length of n0 changing --------------------*/
  int length_old = old.seqlength(n0);
  int length_new = A.seqlength(n0);

  double log_ratio = 2.0*(P.IModel.lengthp(length_new)-P.IModel.lengthp(length_old));
  if (myrandomf() < exp(log_ratio))
    return A;
  else
    return old;
}
