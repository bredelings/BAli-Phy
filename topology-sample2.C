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

bool two_way_topology_sample_fgaps(alignment& A,Parameters& P1,const Parameters& P2,int b) {
  alignment old = A;

  alignment A1 = old;
  alignment A2 = old;

  /*---------------- Setup node names ------------------*/
  assert(b >= P1.T.leafbranches());
  const vector<int> nodes1 = A5::get_nodes_random(P1.T,b);
  const vector<int> nodes2 = A5::get_nodes_random(P2.T,b);

  // sample the path from each matrix, remember the gap probability of that topology
  DParrayConstrained Matrices1 = sample_two_nodes_base(A1,P1,nodes1);
  DParrayConstrained Matrices2 = sample_two_nodes_base(A2,P2,nodes2);

  // Choose from one of the (Ai,Pi) pairs
  double PS1 = P1.likelihood(A1,P1);
  double PA1 = Matrices1.Pr_sum_all_paths();
  double PP1 = prior(P1);

  double PS2 = P1.likelihood(A2,P2);
  double PA2 = Matrices2.Pr_sum_all_paths();
  double PP2 = prior(P2);

  vector<double> P(2);
  P[0] = PS1 + PA1 + PP1;
  P[1] = PS2 + PA2 + PP2;

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


//NOTE: in order to compare the trees which we are summing over different branches,
//      we would have to somehow incorporate the differences in the rest of the likelihood.
//      But in the current code we are ignoring that.
//         - FIXME: calculate the full likelihood, subtract the current alignment, and the add
//           in the sum over all alignments.  (alignment = alignments in the HMM here... )


/// This has to be Gibbs, and use the same substitution::Model in each case...
bool three_way_topology_sample_fgaps(alignment& A,Parameters& P1, const Parameters& P2, 
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

///Sample between 3 topologies, ignoring gap priors on each case
bool three_way_topology_sample_sgaps(alignment& A,Parameters& P1,const Parameters& P2, 
			       const Parameters& P3,int b) {
  double Pr1 = P1.probability(A,P1);
  double Pr2 = P1.probability(A,P2);
  double Pr3 = P1.probability(A,P3);

  /*********** Choose A Topology ************/
  int choice = choose(Pr1,Pr2,Pr3);

  bool success = false;
  if (choice == 1) {
    P1 = P2;
    success = true;
  }
  else if (choice == 2) {
    P1 = P3;
    success = true;
  }
  return true;
}

///Sample between 2 topologies, ignoring gap priors on each case
bool two_way_topology_sample_sgaps(alignment& A,Parameters& P1,const Parameters& P2,int b) {
  double Pr1 = P1.probability(A,P1);
  double Pr2 = P1.probability(A,P2);

  /*********** Choose A Topology ************/
  int choice = choose(Pr1,Pr2);

  bool success = false;
  if (choice == 1) {
    P1 = P2;
    success = true;
  }
  return true;
}

///Sample between 2 topologies, ignoring gap priors on each case
bool two_way_topology_sample(alignment& A,Parameters& P1,const Parameters& P2,int b) {
  assert(P1.IModel().full_tree == P2.IModel().full_tree);

  if (P1.IModel().full_tree)
    return two_way_topology_sample_fgaps(A,P1,P2,b);
  else
    return two_way_topology_sample_sgaps(A,P1,P2,b);
}


bool three_way_topology_sample(alignment& A,Parameters& P1,const Parameters& P2,const Parameters& P3,int b) {
  assert(P1.IModel().full_tree == P2.IModel().full_tree);
  assert(P2.IModel().full_tree == P3.IModel().full_tree);

  if (P1.IModel().full_tree)
    return three_way_topology_sample_fgaps(A,P1,P2,P3,b);
  else
    return three_way_topology_sample_sgaps(A,P1,P2,P3,b);
}


//FIXME - go through code and create more exceptions, from asserts... 
MCMC::result_t three_way_topology_sample(alignment& A,Parameters& P1,int b) {
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
  
  bool success = three_way_topology_sample(A,P1,P2,P3,b);



  if (success)
    result[1] = 1;

  return result;
}

alignment swap(const alignment& old,int n1,int n2) {
  alignment A = old;
  for(int column=0;column<A.length();column++)
    std::swap(A(column,n1),A(column,n2));

  return A;
}



MCMC::result_t three_way_topology_and_alignment_sample(alignment& A1,Parameters& P1,
						       const alignment& A2,const Parameters& P2,
						       const alignment& A3,const Parameters& P3,
						       const vector<int>& nodes) {
  
}




MCMC::result_t three_way_topology_and_alignment_sample(alignment& A,Parameters& P1,int b) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  vector<int> nodes = A5::get_nodes_random(P1.T,b);

  /*--------- Generate the Different Topologies -------*/
  Parameters P2 = P1;
  P2.T.exchange(nodes[1],nodes[2]);
  alignment A2 = swap(A,nodes[4],nodes[5]);

  Parameters P3 = P1;
  P3.T.exchange(nodes[1],nodes[3]);
  alignment A3 = swap(A,nodes[4],nodes[5]);

  bool success = three_way_topology_sample(A1,P1,A2,P2,A3,P3,nodes);

  if (success)
    result[1] = 1;

  return result;

}
