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
  vector<efloat_t> Pr(3);
  for(int i=0;i< Pr.size();i++)
    Pr[i] = p[0].probability(A,p[i]);

  /*********** Choose A Topology ************/
  int C = choose(Pr);

  P = p[C];

  return (C != 0);
}

///Sample between 2 topologies, ignoring gap priors on each case
bool two_way_topology_sample_sgaps(alignment& A,Parameters& P1,const Parameters& P2,int b) {
  efloat_t Pr1 = P1.probability(A,P1);
  efloat_t Pr2 = P1.probability(A,P2);

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
