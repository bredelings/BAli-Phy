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
#include "substitution-index.H"

#include "3way.H"
#include "sample.H"


// for prior_HMM_nogiven
#include "likelihood.H"


using MCMC::MoveStats;

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

int two_way_topology_sample_fgaps(vector<alignment>& a,vector<Parameters>& p,const vector<efloat_t>& rho,int b) 
{
  vector< vector<int> > nodes(2);
  nodes[0] = A5::get_nodes_random(p[0].T,b);
  nodes[1] = A5::get_nodes_random(p[1].T,b);

  return sample_two_nodes_multi(a,p,nodes,rho,true,false);
}

/// This has to be Gibbs, and use the same substitution::Model in each case...

int three_way_topology_sample_fgaps(vector<alignment>& a,vector<Parameters>& p,const vector<efloat_t>& rho,int b) 
{
  vector< vector<int> > nodes(3);
  nodes[0] = A5::get_nodes_random(p[0].T,b);
  nodes[1] = A5::get_nodes_random(p[1].T,b);
  nodes[2] = A5::get_nodes_random(p[2].T,b);

  return sample_two_nodes_multi(a,p,nodes,rho,true,false);
}

///Sample between 3 topologies, ignoring gap priors on each case
int three_way_topology_sample_sgaps(vector<alignment>& a,vector<Parameters>& p,const vector<efloat_t>& rho,int b) 
{
  vector<efloat_t> Pr(3);
  for(int i=0;i< Pr.size();i++)
    Pr[i] = rho[i]*p[0].probability(a[i],p[i]);

  return choose(Pr);
}

///Sample between 2 topologies, ignoring gap priors on each case
int two_way_topology_sample_sgaps(vector<alignment>& a, vector<Parameters>& p,const vector<efloat_t>& rho,int b) 
{
  efloat_t Pr1 = rho[0]*p[0].probability(a[0],p[0]);
  efloat_t Pr2 = rho[1]*p[0].probability(a[1],p[1]);

  return choose2(Pr1,Pr2);
}

///Sample between 2 topologies, ignoring gap priors on each case
int two_way_topology_sample(vector<alignment>& a,vector<Parameters>& p,const vector<efloat_t>& rho, int b) 
{
  assert(p[0].IModel().full_tree == p[1].IModel().full_tree);

  if (p[0].IModel().full_tree)
    return two_way_topology_sample_fgaps(a,p,rho,b);
  else
    return two_way_topology_sample_sgaps(a,p,rho,b);
}

void two_way_topology_sample(alignment& A, Parameters& P, MoveStats& Stats, int b) 
{
  vector<int> nodes = A5::get_nodes_random(P.T,b);

  select_root(P.T, b, P.LC);

  vector<alignment> a(2,A);
  vector<Parameters> p(2,P);

  int b1 = p[1].T.directed_branch(nodes[4],nodes[1]);
  int b2 = p[1].T.directed_branch(nodes[5],nodes[2]);

  p[1].T.exchange_subtrees(b1, b2);
  invalidate_subA_index_branch(a[1], p[1].T, b);
  
  const vector<efloat_t> rho(2,1);

  int C = two_way_topology_sample(a,p,rho,b);

  if (C != -1) {
    A = a[C];
    P = p[C];
  }

  Stats.inc("2-way NNI", C>0);
}

vector<int> NNI_branches(const Tree& T, int b) 
{
  vector<const_branchview> branches;
  branches.push_back(T.branch(b));

  append(T.branch(b).branches_after(),branches);
  append(T.branch(b).reverse().branches_after(),branches);

  assert(branches.size() == 5);

  vector<int> branches2;
  for(int i=0;i<branches.size();i++)
    branches2.push_back(branches[i].undirected_name());

  return branches2;
}

void two_way_NNI_and_branches_sample(alignment& A, Parameters& P, MoveStats& Stats, int b) 
{
  vector<int> nodes = A5::get_nodes_random(P.T,b);

  select_root(P.T, b, P.LC);

  vector<alignment> a(2,A);
  vector<Parameters> p(2,P);

  //---------------- Do the NNI operation -------------------//
  Tree& T2 = p[1].T;

  int b1 = T2.directed_branch(nodes[4],nodes[1]);
  int b2 = T2.directed_branch(nodes[5],nodes[2]);

  T2.exchange_subtrees(b1, b2);
  invalidate_subA_index_branch(a[1], T2, b);
  
  //------------- Propose new branch lengths ----------------//
  double ratio = 1.0;
  vector<int> branches = NNI_branches(p[1].T, b);

  for(int i=0;i<branches.size();i++) {

    double factor = exp(gaussian(0,0.05));

    double L = T2.branch( branches[i] ).length() * factor;

    p[1].setlength(branches[i], L);

    ratio *= factor;
  }


  vector<efloat_t> rho(2);
  rho[0] = 1.0;
  rho[1] = ratio;

  int C = two_way_topology_sample(a,p,rho,b);

  if (C != -1) {
    A = a[C];
    P = p[C];
  }

  Stats.inc("2-way NNI+branches", C>0);
}

int three_way_topology_sample(vector<alignment>& a,vector<Parameters>& p, const vector<efloat_t>& rho, int b) 
{
  assert(p[0].IModel().full_tree == p[1].IModel().full_tree);
  assert(p[1].IModel().full_tree == p[2].IModel().full_tree);

  if (p[0].IModel().full_tree)
    return three_way_topology_sample_fgaps(a,p,rho,b);
  else
    return three_way_topology_sample_sgaps(a,p,rho,b);
}


//FIXME - go through code and create more exceptions, from asserts... 
void three_way_topology_sample(alignment& A,Parameters& P, MoveStats& Stats, int b) 
{
  vector<int> nodes = A5::get_nodes(P.T,b);

  //------ Generate Topologies and alter caches ------///
  select_root(P.T, b, P.LC);
  
  vector<Parameters> p(3,P);
  vector<alignment> a(3,A);

  int b1 = P.T.directed_branch(nodes[4],nodes[1]);
  int b2 = P.T.directed_branch(nodes[5],nodes[2]);
  int b3 = P.T.directed_branch(nodes[5],nodes[3]);

  p[1].T.exchange_subtrees(b1,b2);
  p[1].LC.invalidate_branch(p[1].T, b);
  invalidate_subA_index_branch(a[1], p[1].T, b);

  p[2].T.exchange_subtrees(b1,b3);
  p[2].LC.invalidate_branch(p[2].T, b);
  invalidate_subA_index_branch(a[2], p[2].T, b);
  
  const vector<efloat_t> rho(3,1);

  //------ Resample alignments and select topology -----//
  int C = three_way_topology_sample(a,p,rho,b);

  if (C != -1) {
    A = a[C];
    P = p[C];
  }    

  Stats.inc("3-way NNI",C>0);
}

void three_way_topology_and_alignment_sample(alignment& A,Parameters& P, MoveStats& Stats, int b) 
{
  assert(b >= P.T.n_leafbranches());

  vector<int> two_way_nodes = A5::get_nodes_random(P.T,b);

  //--------- Generate the Different Topologies -------//
  // We ALWAYS resample the connection between two_way_nodes [0] and [4].

  vector<alignment>  a(3,A);
  vector<Parameters> p(3,P);
  int b1 = p[0].T.directed_branch(two_way_nodes[4],two_way_nodes[1]);
  int b2 = p[0].T.directed_branch(two_way_nodes[5],two_way_nodes[2]);
  int b3 = p[0].T.directed_branch(two_way_nodes[5],two_way_nodes[3]);

  p[1].T.exchange_subtrees(b1,b2);
  p[1].LC.invalidate_branch(p[1].T, b);
  invalidate_subA_index_branch(a[1], p[1].T, b);
  
  p[2].T.exchange_subtrees(b1,b3);
  p[2].LC.invalidate_branch(p[2].T, b);
  invalidate_subA_index_branch(a[2], p[2].T, b);

  vector< vector< int> > nodes;
  for(int i=0;i<p.size();i++)
    nodes.push_back(A3::get_nodes_branch_random(p[i].T, two_way_nodes[4], two_way_nodes[0]) );

  const vector<efloat_t> rho(3,1);

  int C = sample_tri_multi(a,p,nodes,rho,true,true);

  if (C != -1) {
    A = a[C];
    P = p[C];
  }

  Stats.inc("3-way NNI + A",C>0);
}
