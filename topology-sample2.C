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

  // Construct the list of bits and states (hidden, or not) for each sub alignment
  vector<int> states_list = construct_states();

  // Construct the 1D state-emit matrix from the 6D one
  vector<int> state_emit_1D = states_list;
  for(int S2=0;S2<state_emit_1D.size();S2++) {
    int state_emit = state_emit_1D[S2]&leafbitsmask;
    if (state_emit)
      state_emit_1D[S2] = 1;
    else
      state_emit_1D[S2] = 0;
  }
  
  // Create the transition matrix first using just the current, fixed ordering
  const Matrix Q = createQ(P.IModel(),states_list);

  // Actually create the Matrices & Chain
  DParrayConstrained Matrices(seqall.size(), state_emit_1D, get_start_P(pi,states_list), Q);

  // Determine which states are allowed to match (c2)
  for(int c2=0;c2<Matrices.size();c2++) {
    int i2 = icol[c2];
    int j2 = jcol[c2];
    int k2 = kcol[c2];
    int l2 = lcol[c2];
    for(int i=0;i<Matrices.nstates();i++) {
      int S2 = Matrices.order(i);
      int state2 = states_list[S2];

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

  Matrices.forward();

  //------------- Sample a path from the matrix -------------------//

  vector<int> path_g = Matrices.sample_path();
  vector<int> path = Matrices.ungeneralize(path_g);

  A = construct(old,path,nodes,T,seqs,states_list);

#ifndef NDEBUG_DP
  vector<int> newnodes;
  for(int i=0;i<6;i++)
    newnodes.push_back(i);

  vector<int> path_new = get_path(project(A,nodes),newnodes,states_list);
  vector<int> path_new2 = get_path(A,nodes,states_list);
  assert(path_new == path_new2); // <- current implementation probably guarantees this
                                 //    but its not a NECESSARY effect of the routine.

  // get the generalized paths - no sequential silent states that can loop
  vector<int> path_new_g = Matrices.generalize(path_new);
  assert(path_new_g == path_g);
  assert(valid(A));
#endif

  /*---------------- Adjust for length of n0 changing --------------------*/
  int l1_old = old.seqlength(nodes[4]);
  int l1_new = A.seqlength(nodes[4]);

  int l2_old = old.seqlength(nodes[5]);
  int l2_new = A.seqlength(nodes[5]);

  double log_ratio = 2.0*(P.IModel().lengthp(l1_new)-P.IModel().lengthp(l1_old));
  log_ratio += 2.0*(P.IModel().lengthp(l2_new)-P.IModel().lengthp(l2_old));

  // if we DON'T make the MH move, go back.
  if (not(myrandomf() < exp(log_ratio)))
    A = old;

  return Matrices;
}

alignment sample_two_nodes(const alignment& old, const Parameters& P,int b) {
  alignment A = old;

  /*---------------- Setup node names ------------------*/
  assert(b >= P.T.leafbranches());

  const vector<int> nodes    = A5::get_nodes_random(P.T,b);

  DParrayConstrained Matrices = sample_two_nodes_base(A,P,nodes);

#ifndef NDEBUG_DP
  // Construct the list of bits and states (hidden, or not) for each sub alignment
  vector<int> states_list = construct_states();


  //--------------- Check alignment construction ------------------//
    std::cerr<<project(old,nodes)<<endl;
    std::cerr<<project(A,nodes)<<endl;


  // get the paths through the 3way alignment, from the entire alignment
  vector<int> newnodes;
  for(int i=0;i<6;i++)
    newnodes.push_back(i);

  vector<int> path_old = get_path(project(old,nodes),newnodes,states_list);
  vector<int> path_new = get_path(project(A,nodes),newnodes,states_list);

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



  return A;
}


// Do we need the different sample_two_nodes_base routines to use the same
// branch ordering?  We are just 
//  o considering a set of paths (w/ associated topologies)
//  o dividing them up unto groups
//  o choosing between the groups
//  o choosing between the paths in each group
// However, we need the routine to be gibbs.  That is, we need to have each
// input sequence propose the others 

// We could consider a set of moves, each of which has a specified branch ordering
// or something.  Then we would need to have each propose the others condition
// on the branch ordering for the 4 leaf branches.

// We could probably have a set ordering for each topology, as long as each
// proposed the other.... (this would be nice!)

bool topology_sample2(alignment& A,Parameters& P1, const Parameters& P2, 
		      const Parameters& P3,int b) {
  alignment old = A;

  alignment A1 = old;
  alignment A2 = old;
  alignment A3 = old;

  /*---------------- Setup node names ------------------*/
  assert(b >= P1.T.leafbranches());
  const vector<int> nodes1 = A5::get_nodes_random(P1.T,b);
  const vector<int> nodes2 = A5::get_nodes_random(P2.T,b);
  const vector<int> nodes3 = A5::get_nodes_random(P3.T,b);

  // sample the path from each matrix, remember the gap probability of that topology
  DParrayConstrained Matrices1 = sample_two_nodes_base(A1,P1,nodes1);
  DParrayConstrained Matrices2 = sample_two_nodes_base(A2,P2,nodes2);
  DParrayConstrained Matrices3 = sample_two_nodes_base(A3,P3,nodes3);

  // Choose from one of the (Ai,Pi) pairs
  double PS1 = P1.likelihood(A1,P1);
  double PA1 = Matrices1.Pr_sum_all_paths();
  double PP1 = prior(P1);

  double PS2 = P1.likelihood(A2,P2);
  double PA2 = Matrices2.Pr_sum_all_paths();
  double PP2 = prior(P2);

  double PS3 = P1.likelihood(A3,P3);
  double PA3 = Matrices3.Pr_sum_all_paths();
  double PP3 = prior(P3);

  vector<double> P(3);
  P[0] = PS1 + PA1 + PP1;
  P[1] = PS2 + PA2 + PP2;
  P[2] = PS3 + PA3 + PP3;

  int choice = choose(P);

  // Get some pointers to the chosen one.
  DParrayConstrained const* CM = &Matrices1;
  alignment const* CA = &A1;
  Parameters const* CP = &P1;
  const vector<int>* Cnodes = &nodes1;
  if (choice == 1){
    CM = &Matrices2;
    CA = &A2;
    CP = &P2;
    Cnodes = &nodes2;
  }
  else if (choice == 2){
    CM = &Matrices3;
    CA = &A3;
    CP = &P3;
    Cnodes = &nodes3;
  }

#ifndef NDEBUG_DP
  /*------- Get the Probabilities of the new and old states --------*/
  const vector<int>& nodes_old    = nodes1;
  const vector<int> nodes_new     = *Cnodes;

  int l1_old = old.seqlength(nodes_old[4]);
  int l1_new = CA->seqlength(nodes_old[4]);

  int l2_old = old.seqlength(nodes_old[5]);
  int l2_new = CA->seqlength(nodes_old[5]);

  double Pr1 = probability3(old,P1) + 2.0*(P1.IModel().lengthp(l1_old) + P1.IModel().lengthp(l2_old));
  double Pr2 = probability3(*CA,*CP) + 2.0*(P1.IModel().lengthp(l1_new) + P1.IModel().lengthp(l2_new));;

  vector<int> states_list = construct_states();


  /*--------------- Get the new and old paths ---------------*/
  assert(b >= P1.T.leafbranches());
  vector<int> newnodes;
  for(int i=0;i<6;i++)
    newnodes.push_back(i);

  vector<int> path_old = get_path(project(old,nodes1),newnodes,states_list);
  //  vector<int> path_old = A5::get_path(old,nodes_old,states_list);
  vector<int> path_g_old = Matrices1.generalize(path_old);

  vector<int> path_new = get_path(project(*CA,*Cnodes),newnodes,states_list);
  //  vector<int> path_new = A5::get_path(*CA,nodes_new,states_list);
  vector<int> path_g_new = CM->generalize(path_new);

  /*--------------- Compute the sampling probabilities ---------------*/
  double SP1 = choose_P(0,P) + Matrices1.path_P(path_g_old) + Matrices1.generalize_P(path_old);
  double SP2 = choose_P(choice,P) + CM->path_P(path_g_new) + CM->generalize_P(path_new);

  double diff = (Pr2 - Pr1) - (SP2 - SP1);
  std::cerr<<"diff = "<<diff<<std::endl;
  if (abs(diff) > 1.0e-9) {
    std::cerr<<old<<endl;
    std::cerr<<A<<endl;

    std::cerr<<project(old,nodes_old)<<endl;
    std::cerr<<project(A,nodes_new)<<endl;

    std::abort();
  }
#endif

  // Go ahead with the choice
  if (choice != 0) {
    A  = *CA;
    P1 = *CP;
  }

  return (choice != 0);
}

MCMC::result_t sample_topology2(alignment& A,Parameters& P1,int b) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  vector<int> nodes = A5::get_nodes(P1.T,b);

  /****** Generate the Different Topologies *******/
  Parameters P2 = P1;
  Parameters P3 = P1;

  SequenceTree& T2 = P2.T;
  SequenceTree& T3 = P3.T;

  T2.exchange(nodes[1],nodes[2]);
  T3.exchange(nodes[1],nodes[3]);
  
  bool success;
  if (P1.IModel().full_tree)
    success = topology_sample2(A,P1,P2,P3,b);
  else
    success = sample_topology_sgaps(A,P1,P2,P3,b);

  if (success)
    result[1] = 1;

  return result;
}

