#include <valarray>
#include <iostream>
#include <cmath>
#include "sample.H"
#include "logsum.H"
#include "choose.H"
#include "bits.H"
#include "util.H"
#include "rng.H"
#include "5way.H"
#include "dpmatrix.H"

// for prior_HMM_nogiven
#include "likelihood.H"

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

using namespace A5;

alignment sample_two_nodes(const alignment& old,const Parameters& P,int b) {
  const tree& T = P.T;

  const vector<double>& pi = P.IModel().pi;

  //  std::cerr<<"old = "<<old<<endl;

  /*---------------- Setup node names ------------------*/
  assert(node >= T.leaves());

  const vector<int> nodes    = get_nodes(old,T,b);
  const vector<int> branches = get_nodes(old,T,b);
  const vector<int> obranches = randomize(branches);
  
  /*------------- Compute sequence properties --------------*/
  vector<int> columns = getorder(old,obranches);

  //  std::cerr<<"n0 = "<<n0<<"   n1 = "<<n1<<"    n2 = "<<n2<<"    n3 = "<<n3<<std::endl;
  //  std::cerr<<"old (reordered) = "<<project(old,n0,n1,n2,n3)<<endl;

  // Find sub-alignments and sequences
  vector<vector<int> > seqs(5);
  for(int i=0;i<columns.size();i++) {
    int column = columns[i];
    for(int i=0;i<4;i++)
      if (not old.gap(column,nodes[2+i]))
	seqs[i].push_back(column);

    if (not old.gap(column,nodes[2]) or 
	not old.gap(column,nodes[3]) or 
	not old.gap(column,nodes[4]) or 
	not old.gap(column,nodes[5]))
      seqs[4].push_back(column);
  }
  const vector<int>& seqall = seqs[4];

  // Map columns with n2 or n3 to single index 'c'
  //  vector< vector<int> > cols(4,vector<int>(seqall.size()+1));
  vector<int> icol(seqall.size()+1);
  vector<int> jcol(seqall.size()+1);
  vector<int> kcol(seqall.size()+1);
  vector<int> lcol(seqall.size()+1);

  icol[0] = 0;
  jcol[0] = 0;
  kcol[0] = 0;
  lcol[0] = 0;
  for(int c=1,i=0,j=0,k=0,l=0;c<seqall.size()+1;c++) {
    if (not old.gap(seqall[c-1],nodes[2]))
      i++;    
    if (not old.gap(seqall[c-1],nodes[3]))
      j++;    
    if (not old.gap(seqall[c-1],nodes[4]))
      k++;
    if (not old.gap(seqall[c-1],nodes[5]))
      l++;
    icol[c] = i;
    jcol[c] = j;
    kcol[c] = k;
    lcol[c] = l;
  }


  /*-------------- Create alignment matrices ---------------*/

  // Cache which states emit which sequences
  vector<int> state_emit(nstates+1);
  for(int S2=0;S2<state_emit.size();S2++) {
    state_emit[S2] = 0;

    if (di(S2) or dj(S2) or dk(S2) or dl(S2)) 
      state_emit[S2] |= (1<<0);
  }

  const Matrix Q = createQ(P.IModel());

  // Actually create the Matrices & Chain
  DParrayConstrained Matrices(seqall.size(),state_emit,get_start_P(pi),Q);

  // Determine state order
  vector<int> state_order(nstates);
  for(int i=0;i<state_order.size();i++)
    state_order[i] = i;
  assert(0);
  // FIXME  - this needs to be internalized into DParray!

  // Determine which states are allowed to match (c2)
  for(int c2=0;c2<Matrices.size();c2++) {
    int i2 = icol[c2];
    int j2 = jcol[c2];
    int k2 = kcol[c2];
    int l2 = lcol[c2];
    for(int i=0;i<state_order.size();i++) {
      int S2 = state_order[i];

      //---------- Get (,j1,k1) ----------
      int i1 = i2;
      if (di(S2)) i1--;

      int j1 = j2;
      if (dj(S2)) j1--;

      int k1 = k2;
      if (dk(S2)) k1--;

      int l1 = l2;
      if (dl(S2)) l1--;
      
      //------ Get c1, check if valid ------
      if (c2==0 
	  or (i1 == i2 and j1 == j2 and k1 == k2 and l1 == l2) 
	  or (i1 == icol[c2-1] and j1 == jcol[c2-1] and k1 == kcol[c2-1] and l1 = lcol[c2-1]) )
	Matrices.states(c2).push_back(S2);
      else
	; // this state not allowed here
    }
  }


  /*------------------ Compute the DP matrix ---------------------*/

  Matrices.forward();

  //------------- Sample a path from the matrix -------------------//

  vector<int> path_g = Matrices.sample_path();
  vector<int> path = Matrices.ungeneralize(path_g);

  alignment A = construct(old,path,n0,n1,n2,n3,T,seq1,seq2,seq3);

#ifndef NDEBUG_DP
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
  double s1 = P.likelihood(old,P);
  double s2 = P.likelihood(A,P);

  double lp1 = prior_HMM_nogiven(old,P);
  double lp2 = prior_HMM_nogiven(A  ,P);

  double diff = Matrices.check(path_old,path_new,lp1,s1,lp2,s2);

  if (abs(diff) > 1.0e-9) {
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

  double log_ratio = 2.0*(P.IModel().lengthp(length_new)-P.IModel().lengthp(length_old));
  if (myrandomf() < exp(log_ratio))
    return A;
  else
    return old;
}
