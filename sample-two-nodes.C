#include <valarray>
#include <iostream>
#include <cmath>
#include <cassert>
#include "sample.H"
#include "logsum.H"
#include "choose.H"
#include "bits.H"
#include "util.H"
#include "rng.H"
#include "5way.H"
#include "dpmatrix.H"
#include "alignment-sums.H"

// for prior_HMM_nogiven
#include "likelihood.H"

// We are sampling from a 5-way alignment (along 5 branches)

// Its a 4-way dynamic programming, though - so the only thing
// that matters is the order of the 4D path. (I think...)

// We want to scramble the sorting method for the branches
// Perhaps that should be the NEXT step?  We can scramble the
// node names, though - we use those to know which leaf node
// is connected to which internal node.

// Branches are labelled 0-3, as are the leaves.  Internal nodes
// are 4,5; internal branch is 5.

using std::abs;
using std::valarray;

using namespace A5;

// IDEA: make a routine which encapsulates this sampling, and passes back
//  the total_sum.  Then we can just call sample_two_nodes w/ each of the 3 trees.
// We can choose between them with the total_sum (I mean, sum_all_paths).
// Then, we can just debug one routine, basically.

DParrayConstrained sample_two_nodes_base(alignment& A,const Parameters& P,const vector<int>& nodes) {

  const tree& T = P.T;
  alignment old = A;

  const vector<double>& pi = P.IModel().pi;

  //  std::cerr<<"old = "<<old<<endl;

  /*------------- Compute sequence properties --------------*/
  vector<int> columns = getorder(old,nodes);

  //  std::cerr<<"n0 = "<<n0<<"   n1 = "<<n1<<"    n2 = "<<n2<<"    n3 = "<<n3<<std::endl;
  //  std::cerr<<"old (reordered) = "<<project(old,n0,n1,n2,n3)<<endl;

  // Find sub-alignments and sequences
  vector<vector<int> > seqs(5);
  for(int i=0;i<columns.size();i++) {
    int column = columns[i];
    for(int i=0;i<4;i++)
      if (not old.gap(column,nodes[i]))
	seqs[i].push_back(column);

    if (not old.gap(column,nodes[0]) or 
	not old.gap(column,nodes[1]) or 
	not old.gap(column,nodes[2]) or 
	not old.gap(column,nodes[3]))
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
    if (not old.gap(seqall[c-1],nodes[0]))
      i++;    
    if (not old.gap(seqall[c-1],nodes[1]))
      j++;    
    if (not old.gap(seqall[c-1],nodes[2]))
      k++;
    if (not old.gap(seqall[c-1],nodes[3]))
      l++;
    icol[c] = i;
    jcol[c] = j;
    kcol[c] = k;
    lcol[c] = l;
  }


  /*-------------- Create alignment matrices ---------------*/

  // Construct the 1D state-emit matrix from the 6D one
  vector<int> state_emit_1D = A5::states_list;
  for(int S2=0;S2<state_emit_1D.size();S2++) {
    int state_emit = state_emit_1D[S2]&leafbitsmask;
    if (state_emit)
      state_emit_1D[S2] = 1;
    else
      state_emit_1D[S2] = 0;
  }
  
  // Create the transition matrix first using just the current, fixed ordering
  const Matrix Q = createQ(P.IModel(),A5::states_list);

  // Actually create the Matrices & Chain
  DParrayConstrained Matrices(seqall.size(), state_emit_1D, get_start_P(pi,A5::states_list), Q);

  // Determine which states are allowed to match (c2)
  for(int c2=0;c2<Matrices.size();c2++) {
    int i2 = icol[c2];
    int j2 = jcol[c2];
    int k2 = kcol[c2];
    int l2 = lcol[c2];
    for(int i=0;i<Matrices.nstates();i++) {
      int S2 = Matrices.order(i);
      int state2 = A5::states_list[S2];

      //---------- Get (,j1,k1) ----------
      int i1 = i2;
      if (bitset(state2,0)) i1--;

      int j1 = j2;
      if (bitset(state2,1)) j1--;

      int k1 = k2;
      if (bitset(state2,2)) k1--;

      int l1 = l2;
      if (bitset(state2,3)) l1--;
      
      //------ Get c1, check if valid ------
      if (c2==0 
	  or (i1 == i2 and j1 == j2 and k1 == k2 and l1 == l2) 
	  or (i1 == icol[c2-1] and j1 == jcol[c2-1] and k1 == kcol[c2-1] and l1 == lcol[c2-1]) )
	Matrices.states(c2).push_back(S2);
      else
	; // this state not allowed here
    }
  }


  /*------------------ Compute the DP matrix ---------------------*/

  //  Matrices.prune(); no effect, really?
  Matrices.forward();

  //------------- Sample a path from the matrix -------------------//

  vector<int> path_g = Matrices.sample_path();
  vector<int> path = Matrices.ungeneralize(path_g);

  A = construct(old,path,nodes,T,seqs,A5::states_list);

#ifndef NDEBUG_DP
  vector<int> newnodes;
  for(int i=0;i<6;i++)
    newnodes.push_back(i);

  vector<int> path_new = get_path(project(A,nodes),newnodes,A5::states_list);
  vector<int> path_new2 = get_path(A,nodes,A5::states_list);
  assert(path_new == path_new2); // <- current implementation probably guarantees this
                                 //    but its not a NECESSARY effect of the routine.

  // get the generalized paths - no sequential silent states that can loop
  vector<int> path_new_g = Matrices.generalize(path_new);
  assert(path_new_g == path_g);
  assert(valid(A));
#endif

  return Matrices;
}

alignment sample_two_nodes(const alignment& old, const Parameters& P,int b) {
  alignment A = old;

  /*---------------- Setup node names ------------------*/
  assert(b >= P.T.leafbranches());

  const vector<int> nodes    = A5::get_nodes_random(P.T,b);

  DParrayConstrained Matrices = sample_two_nodes_base(A,P,nodes);

#ifndef NDEBUG_DP

  // get the paths through the 3way alignment, from the entire alignment
  vector<int> newnodes;
  for(int i=0;i<6;i++)
    newnodes.push_back(i);

  vector<int> path_old = get_path(project(old,nodes),newnodes,A5::states_list);
  vector<int> path_new = get_path(project(A,nodes),newnodes,A5::states_list);
  //-------------- Check relative path probabilities --------------//
  double s1 = P.likelihood(old,P);
  double s2 = P.likelihood(A,P);

  double lp1 = prior_HMM_nogiven(old,P);
  double lp2 = prior_HMM_nogiven(A  ,P);

  double diff = Matrices.check(path_old,path_new,lp1,s1,lp2,s2);

  if (abs(diff) > 1.0e-9) {
    std::cerr<<old<<endl;
    std::cerr<<A<<endl;

    std::cerr<<project(old,nodes)<<endl;
    std::cerr<<project(A,nodes)<<endl;

    std::abort();
  }

#endif

  /*---------------- Adjust for length of n0 changing --------------------*/

  // if we DON'T make the MH move, go back.
  if (myrandomf() < exp(log_acceptance_ratio(old,P,nodes,A,P,nodes)))
    ;
  else
    A = old;

  return A;
}


