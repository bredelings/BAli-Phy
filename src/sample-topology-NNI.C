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
#include "alignment-sums.H"


#include "3way.H"
#include "sample.H"


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

// Do we need the different sample_two_nodes_base routines to use the same
// sub-alignment ordering for different topologies?  No.
//  o Sub-alignment order should affect only which paths are considered
//  o We are essentially considering a set of paths for each topology
//    (So have ALMOST marginalized over the paths: we don't consider some column orders though)
//  o We then divide them up unto groups (topologies)
//  o 1st choose between the groups ,
//  o 2nd choose between the paths in the chosen group.
// The fact that we don't consider some paths should not make this non-reversible
// Each combination of order for each topology is a reversible move, because each path proposes the others.




//FIXME - cleanup in several ways.  Firstly, index alignments, as in NewA[0],NewA[1]..
// This should get rid of the pointers to the chosen onee... (just have NewA[choice])
// Keep track of the old, and go BACK if it doesn't work?

//FIXME - separate HMM out into its own file?

bool sample_two_NNI_two_nodes_MH(alignment& A,Parameters& P1,const Parameters& P2,int b,double rho) {
  alignment old = A;

  alignment A1 = old;
  alignment A2 = old;

  //-------------------- Setup node names ----------------------//
  assert(b >= P1.T.n_leafbranches());
  const vector<int> nodes1 = A5::get_nodes_random(P1.T,b);
  const vector<int> nodes2 = A5::get_nodes_random(P2.T,b);

  // sample the path from each matrix, remember the gap probability of that topology
  DParrayConstrained Matrices1 = sample_two_nodes_base(A1,P1,nodes1);
  DParrayConstrained Matrices2 = sample_two_nodes_base(A2,P2,nodes2);

  // Choose from one of the (Ai,Pi) pairs
  double PS1 = P1.likelihood(A1,P1);
  double PA1 = Matrices1.Pr_sum_all_paths();
  double PP1 = prior(P1)/P1.Temp;

  double PS2 = P1.likelihood(A2,P2);
  double PA2 = Matrices2.Pr_sum_all_paths();
  double PP2 = prior(P2)/P2.Temp;

  vector<double> P(2);
  P[0] = PS1 + PA1 + PP1;
  P[1] = PS2 + PA2 + PP2;


  //------------- Get some pointers to the chosen one -------------//
  vector<int> nodes_old = nodes1;

  int choice = 0;
  DParrayConstrained const* CM = &Matrices1;
  alignment const* CA = &A1;
  Parameters const* CP = &P1;
  vector<int> nodes_new = nodes1;

  double a12 = P[1]-P[0]-log(rho);   // log(a12/a21)
  if (myrandomf() < exp(a12)) {
    choice = 1;
    CM = &Matrices2;
    CA = &A2;
    CP = &P2;
    nodes_new = nodes2;
  }


#ifndef NDEBUG_DP
  //----------------- Get the new and old paths -----------------//
  assert(b >= P1.T.n_leafbranches());
  vector<int> newnodes;
  for(int i=0;i<6;i++)
    newnodes.push_back(i);

  vector<int> path_old = get_path(project(old,nodes_old),newnodes,A5::states_list);
  //  vector<int> path_old = A5::get_path(old,nodes_old,states_list);
  vector<int> path_g_old = Matrices1.generalize(path_old);

  vector<int> path_new = get_path(project(*CA,nodes_new),newnodes,A5::states_list);
  //  vector<int> path_new = A5::get_path(*CA,nodes_new,states_list);
  vector<int> path_g_new = CM->generalize(path_new);

  //------------- Compute the sampling probabilities -------------//
  double Pr1 = probability3(old,P1) + A5::log_correction(old,P1,nodes_old);
  double Pr2 = probability3(*CA,*CP) + A5::log_correction(*CA,*CP,nodes_new);

  double SP1 = choose_P(0,P) + Matrices1.path_P(path_g_old) + Matrices1.generalize_P(path_old);
  double SP2 = choose_P(choice,P) + CM->path_P(path_g_new) + CM->generalize_P(path_new);

  double diff1 = (Pr2 - Pr1) - (SP2 - SP1);
  std::cerr<<"diff1 = "<<diff1<<std::endl;
  std::cerr<<"choice = "<<choice<<std::endl;
  double a_ij = 0;
  if (choice) a_ij = log(rho) + a12;
  std::cerr<<"a_ij = "<<a_ij<<std::endl;

  double P_ij = a_ij +       CM->path_P(path_g_new) +       CM->generalize_P(path_new);
  double P_ji =        Matrices1.path_P(path_g_old) + Matrices1.generalize_P(path_old);

  std::cerr<<"P_ij = "<<P_ij<<std::endl;
  std::cerr<<"P_ji = "<<P_ji<<std::endl;

  double diff2 = (Pr1 + P_ij) - (Pr2 + P_ji);
  std::cerr<<"diff2 = "<<diff2<<std::endl;
  if (abs(diff2) > 1.0e-9) {
    std::cerr<<old<<endl;
    std::cerr<<A<<endl;

    std::cerr<<project(old,nodes_old)<<endl;
    std::cerr<<project(A,nodes_new)<<endl;

    throw myexception()<<__PRETTY_FUNCTION__<<": sampling probabilities were incorrect";
  }
#endif

  //---------------- Adjust for length of n4 and n5 changing --------------------//

  // if we accept the move, then record the changes
  bool success = false;
  if (myrandomf() < exp(log_acceptance_ratio(old,P1,nodes_old,*CA,*CP,nodes_new))) {
    A = *CA;
    if (choice != 0) {
      success = true;
      P1 = *CP;
    }
  }

  return success;
}

bool two_way_topology_sample_fgaps(alignment& A,Parameters& P1,const Parameters& P2,int b) {
  
  vector<Parameters> p(2,P1);
  p[1] = P2;

  vector< vector<int> > nodes(2);
  nodes[0] = A5::get_nodes_random(p[0].T,b);
  nodes[1] = A5::get_nodes_random(p[1].T,b);

  bool success = sample_two_nodes_multi(A,p,nodes,true,false);
  P1 = p[0];

  return success;
}

/// This has to be Gibbs, and use the same substitution::Model in each case...

bool three_way_topology_sample_fgaps(alignment& A,Parameters& P,vector<Parameters>& p,int b) 
{
  vector< vector<int> > nodes(3);
  nodes[0] = A5::get_nodes_random(p[0].T,b);
  nodes[1] = A5::get_nodes_random(p[1].T,b);
  nodes[2] = A5::get_nodes_random(p[2].T,b);

  bool success = sample_two_nodes_multi(A,p,nodes,true,false);
  P = p[0];

  return success;
}

///Sample between 3 topologies, ignoring gap priors on each case
bool three_way_topology_sample_sgaps(alignment& A,Parameters& P,vector<Parameters>& p,int b) 
{
  double Pr1 = p[0].probability(A,p[0]);
  double Pr2 = p[0].probability(A,p[1]);
  double Pr3 = p[0].probability(A,p[2]);

  /*********** Choose A Topology ************/
  int choice = choose3(Pr1,Pr2,Pr3);

  P = p[choice];

  return (choice != 0);
}

///Sample between 2 topologies, ignoring gap priors on each case
bool two_way_topology_sample_sgaps(alignment& A,Parameters& P1,const Parameters& P2,int b) {
  double Pr1 = P1.probability(A,P1);
  double Pr2 = P1.probability(A,P2);

  /*********** Choose A Topology ************/
  int choice = choose2(Pr1,Pr2);

  bool success = false;
  if (choice == 1) {
    P1 = P2;
    success = true;
  }
  return success;
}

///Sample between 2 topologies, ignoring gap priors on each case
bool two_way_topology_sample(alignment& A,Parameters& P1,const Parameters& P2,int b) {
  assert(P1.IModel().full_tree == P2.IModel().full_tree);

  if (P1.IModel().full_tree)
    return two_way_topology_sample_fgaps(A,P1,P2,b);
  else
    return two_way_topology_sample_sgaps(A,P1,P2,b);
}

MCMC::result_t two_way_topology_sample(alignment& A, Parameters& P,int b) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  vector<int>nodes = A5::get_nodes_random(P.T,b);

  select_root(P.T, b, P.LC);

  Parameters P2 = P;
  int b1 = P2.T.directed_branch(nodes[4],nodes[1]);
  int b2 = P2.T.directed_branch(nodes[5],nodes[2]);
  P2.T.exchange_subtrees(b1, b2);

  P2.LC.invalidate_branch(P2.T, b);
  
  bool success = two_way_topology_sample(A,P,P2,b);
  if (success)
    result[1] = 1;

  return result;
}

MCMC::result_t two_way_topology_sample_MH(alignment& A, Parameters& P,int b) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;
  
  vector<int> nodes = A5::get_nodes_random(P.T,b);

  Parameters P2 = P;
  int b1 = P2.T.directed_branch(nodes[4],nodes[1]);
  int b2 = P2.T.directed_branch(nodes[5],nodes[2]);
  P2.T.exchange_subtrees(b1,b2);

  bool success = sample_two_NNI_two_nodes_MH(A,P,P2,b,1.0);
  if (success)
    result[1] = 1;

  return result;
}


bool three_way_topology_sample(alignment& A,Parameters& P,vector<Parameters>& p,int b) {
  assert(   P.IModel().full_tree == p[0].IModel().full_tree);
  assert(p[0].IModel().full_tree == p[1].IModel().full_tree);
  assert(p[1].IModel().full_tree == p[2].IModel().full_tree);

  if (P.IModel().full_tree)
    return three_way_topology_sample_fgaps(A,P,p,b);
  else
    return three_way_topology_sample_sgaps(A,P,p,b);
}


//FIXME - go through code and create more exceptions, from asserts... 
MCMC::result_t three_way_topology_sample(alignment& A,Parameters& P,int b) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  vector<int> nodes = A5::get_nodes(P.T,b);

  /****** Generate the Different Topologies *******/
  select_root(P.T, b, P.LC);
  
  vector<Parameters> p(3,P);

  SequenceTree& T2 = p[1].T;
  SequenceTree& T3 = p[2].T;
  int b1 = P.T.directed_branch(nodes[4],nodes[1]);
  int b2 = P.T.directed_branch(nodes[5],nodes[2]);
  int b3 = P.T.directed_branch(nodes[5],nodes[3]);

  T2.exchange_subtrees(b1,b2);
  p[1].LC.invalidate_branch(T2, b);

  T3.exchange_subtrees(b1,b3);
  p[2].LC.invalidate_branch(T3, b);
  
  bool success = three_way_topology_sample(A,P,p,b);

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

MCMC::result_t three_way_topology_and_alignment_sample(alignment& A,Parameters& P,int b) {
  assert(b >= P.T.n_leafbranches());

  MCMC::result_t result(0.0,2);
  result[0] = 1.0;


  vector<int> two_way_nodes = A5::get_nodes_random(P.T,b);

  //--------- Generate the Different Topologies -------//
  // We ALWAYS resample the connection between two_way_nodes [0] and [4].

  vector<Parameters> p(3,P);
  int b1 = p[0].T.directed_branch(two_way_nodes[4],two_way_nodes[1]);
  int b2 = p[0].T.directed_branch(two_way_nodes[5],two_way_nodes[2]);
  int b3 = p[0].T.directed_branch(two_way_nodes[5],two_way_nodes[3]);
  p[1].T.exchange_subtrees(b1,b2);
  p[2].T.exchange_subtrees(b1,b3);

  vector< vector< int> > nodes;
  for(int i=0;i<p.size();i++)
    nodes.push_back(A3::get_nodes_branch_random(p[i].T, two_way_nodes[4], two_way_nodes[0]) );

  if (sample_tri_multi(A,p,nodes,true,true))
    result[1] = 1;
  P = p[0];

  return result;
}
