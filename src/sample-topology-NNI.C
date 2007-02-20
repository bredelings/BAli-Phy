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
#include "alignment-util.H"
#include "tree-util.H"
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

int two_way_topology_sample_fgaps(vector<Parameters>& p,const vector<efloat_t>& rho,int b) 
{
  vector< vector<int> > nodes(2);
  nodes[0] = A5::get_nodes_random(p[0].T,b);
  nodes[1] = A5::get_nodes_random(p[1].T,b);

  return sample_two_nodes_multi(p,nodes,rho,true,false);
}

/// This has to be Gibbs, and use the same substitution::Model in each case...

int three_way_topology_sample_fgaps(vector<Parameters>& p,const vector<efloat_t>& rho,int b) 
{
  vector< vector<int> > nodes(3);
  nodes[0] = A5::get_nodes_random(p[0].T,b);
  nodes[1] = A5::get_nodes_random(p[1].T,b);
  nodes[2] = A5::get_nodes_random(p[2].T,b);

  return sample_two_nodes_multi(p,nodes,rho,true,false);
}

///Sample between 3 topologies, ignoring gap priors on each case
int three_way_topology_sample_sgaps(vector<Parameters>& p,const vector<efloat_t>& rho) 
{
  vector<efloat_t> Pr(3);
  for(int i=0;i< Pr.size();i++)
    Pr[i] = rho[i]*p[i].probability();

  return choose(Pr);
}

///Sample between 2 topologies, ignoring gap priors on each case
int two_way_topology_sample_sgaps(vector<Parameters>& p,const vector<efloat_t>& rho) 
{
  efloat_t Pr1 = rho[0]*p[0].probability();
  efloat_t Pr2 = rho[1]*p[1].probability();

  return choose2(Pr1,Pr2);
}

///Sample between 2 topologies, ignoring gap priors on each case
int two_way_topology_sample(vector<Parameters>& p,const vector<efloat_t>& rho, int b) 
{
  assert(p[0].has_IModel() == p[1].has_IModel());

  if (p[0].has_IModel())
    return two_way_topology_sample_fgaps(p,rho,b);
  else
    return two_way_topology_sample_sgaps(p,rho);
}


void two_way_topology_sample(Parameters& P, MoveStats& Stats, int b) 
{
  if (P.has_IModel() and P.branch_HMM_type[b] == 1)
    return;

  vector<int> nodes = A5::get_nodes_random(P.T,b);

  P.select_root(b);

  vector<Parameters> p(2,P);

  int b1 = p[1].T.directed_branch(nodes[4],nodes[1]);
  int b2 = p[1].T.directed_branch(nodes[5],nodes[2]);

  p[1].T.exchange_subtrees(b1, b2);
  p[1].tree_propagate(); 
  p[1].LC_invalidate_branch(b);
  p[1].invalidate_subA_index_branch(b);
  
  if (not extends(p[1].T, P.TC))
    return;

  vector<efloat_t> rho(2,1);

  // because we would select between topologies before selecting
  // internal node states, then reverse distribution cannot depend on 
  // the internal node state of the proposed new topology/alignment
  bool smart_inner_branch = (uniform() < 0.1) and not P.has_IModel();
  if (smart_inner_branch) 
  {
    vector<double> G0 = gamma_approx(p[0],b);
    vector<double> G1 = gamma_approx(p[1],b);

    double a0 = G0[0]+1.0;
    double b0 = -1.0/G0[1];

    double a1 = G1[0]+1.0;
    double b1 = -1.0/G1[1];

    if (a0 < 0 or b0<0) {
      std::cerr<<"a0 = "<<a0<<"  b0 = "<<b0<<std::endl;
      a0 = 1;
      b0 = p[0].branch_mean();
    }

    if (a1 < 0 or b1<0) {
      std::cerr<<"a1 = "<<a0<<"  b1 = "<<b0<<std::endl;
      a1 = 1;
      b1 = p[1].branch_mean();
    }

    p[1].setlength(b,gamma(a1,b1));

    rho[0] = gsl_ran_gamma_pdf(p[1].T.branch(b).length(),a1,b1);
    rho[1] = gsl_ran_gamma_pdf(p[0].T.branch(b).length(),a0,b0);
  }

  int C = two_way_topology_sample(p,rho,b);

  if (C != -1) {
    P = p[C];
  }

  MCMC::Result result(2);

  result.totals[0] = (C>0)?1:0;
  result.totals[1] = p[0].T.branch(b).length();

  if (smart_inner_branch)
    Stats.inc("NNI (2-way,gamma)", result);
  else 
    Stats.inc("NNI (2-way)", result);
}

void two_way_NNI_SPR_sample(Parameters& P, MoveStats& Stats, int b) 
{
  if (P.has_IModel() and P.branch_HMM_type[b] == 1)
    return;

  vector<int> nodes = A5::get_nodes_random(P.T,b);

  P.select_root(b);

  vector<Parameters> p(2,P);

  int b1 = p[1].T.directed_branch(nodes[4],nodes[1]);
  int b2 = p[1].T.directed_branch(nodes[5],nodes[2]);

  p[1].T.exchange_subtrees(b1, b2);
  p[1].tree_propagate(); 
  p[1].LC_invalidate_branch(b);
  p[1].invalidate_subA_index_branch(b);
  
  if (not extends(p[1].T, P.TC))
    return;

  double LA = p[0].T.branch(nodes[4],nodes[0]).length();
  double LB = p[0].T.branch(nodes[4],nodes[5]).length();
  double LC = p[0].T.branch(nodes[5],nodes[3]).length();

  p[1].setlength(p[1].T.branch(nodes[0],nodes[4]),LA + LB);
  p[1].setlength(p[1].T.branch(nodes[4],nodes[5]),LC*uniform());
  p[1].setlength(p[1].T.branch(nodes[5],nodes[3]),LC - p[1].T.branch(nodes[4],nodes[5]).length());

  vector<efloat_t> rho(2,1);
  rho[1] = LC/(LA+LB);

  int C = two_way_topology_sample(p,rho,b);

  if (C != -1) {
    P = p[C];
  }

  Stats.inc("NNI (2-way/SPR)", C>0);
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

void two_way_NNI_and_branches_sample(Parameters& P, MoveStats& Stats, int b) 
{
  if (P.has_IModel() and P.branch_HMM_type[b] == 1)
    return;

  vector<int> nodes = A5::get_nodes_random(P.T,b);

  P.select_root(b);

  vector<Parameters> p(2,P);

  //---------------- Do the NNI operation -------------------//
  int b1 = p[1].T.directed_branch(nodes[4],nodes[1]);
  int b2 = p[1].T.directed_branch(nodes[5],nodes[2]);

  p[1].T.exchange_subtrees(b1, b2);
  p[1].tree_propagate(); 
  p[1].LC_invalidate_branch(b);
  p[1].invalidate_subA_index_branch(b);
  
  if (not extends(p[1].T, P.TC))
    return;

  //------------- Propose new branch lengths ----------------//
  double ratio = 1.0;
  vector<int> branches = NNI_branches(p[1].T, b);

  for(int i=0;i<branches.size();i++) {

    double factor = exp(gaussian(0,0.05));

    double L = p[1].T.branch( branches[i] ).length() * factor;

    p[1].setlength(branches[i], L);

    ratio *= factor;
  }


  vector<efloat_t> rho(2);
  rho[0] = 1.0;
  rho[1] = ratio;

  int C = two_way_topology_sample(p,rho,b);

  if (C != -1) {
    P = p[C];
  }

  Stats.inc("NNI (2-way) + branches", C>0);
}

void two_way_NNI_sample(Parameters& P, MoveStats& Stats, int b) 
{
  if (P.has_IModel() and P.branch_HMM_type[b] == 1)
    return;

  double U = uniform();
  if (U < 0.33333333)
    two_way_topology_sample(P,Stats,b);
  else if (U < 0.66666666)
    two_way_NNI_SPR_sample(P,Stats,b);
  else
    two_way_NNI_and_branches_sample(P,Stats,b);
}

int three_way_topology_sample(vector<Parameters>& p, const vector<efloat_t>& rho, int b) 
{
  assert(p[0].has_IModel() == p[1].has_IModel());
  assert(p[1].has_IModel() == p[2].has_IModel());

  if (p[0].has_IModel())
    return three_way_topology_sample_fgaps(p,rho,b);
  else
    return three_way_topology_sample_sgaps(p,rho);
}


//FIXME - go through code and create more exceptions, from asserts... 
void three_way_topology_sample(Parameters& P, MoveStats& Stats, int b) 
{
  if (P.has_IModel() and P.branch_HMM_type[b] == 1)
    return;

  vector<int> nodes = A5::get_nodes(P.T,b);

  //------ Generate Topologies and alter caches ------///
  P.select_root(b);
  
  vector<Parameters> p(3,P);

  int b1 = P.T.directed_branch(nodes[4],nodes[1]);
  int b2 = P.T.directed_branch(nodes[5],nodes[2]);
  int b3 = P.T.directed_branch(nodes[5],nodes[3]);

  p[1].T.exchange_subtrees(b1,b2);
  p[1].tree_propagate(); 
  p[1].LC_invalidate_branch(b);
  p[1].invalidate_subA_index_branch(b);

  if (not extends(p[1].T, P.TC))
    return;

  p[2].T.exchange_subtrees(b1,b3);
  p[2].tree_propagate(); 
  p[2].LC_invalidate_branch(b);
  p[2].invalidate_subA_index_branch(b);
  
  if (not extends(p[2].T, P.TC))
    return;

  const vector<efloat_t> rho(3,1);

  //------ Resample alignments and select topology -----//
  int C = three_way_topology_sample(p,rho,b);

  if (C != -1) {
    P = p[C];
  }    

  Stats.inc("NNI (3-way)",C>0);
}

void three_way_topology_and_alignment_sample(Parameters& P, MoveStats& Stats, int b) 
{
  assert(b >= P.T.n_leafbranches());

  if (P.has_IModel() and P.branch_HMM_type[b] == 1)
    return;

  vector<int> two_way_nodes = A5::get_nodes_random(P.T,b);

  //--------- Generate the Different Topologies -------//
  // We ALWAYS resample the connection between two_way_nodes [0] and [4].

  vector<Parameters> p(3,P);
  int b1 = p[0].T.directed_branch(two_way_nodes[4],two_way_nodes[1]);
  int b2 = p[0].T.directed_branch(two_way_nodes[5],two_way_nodes[2]);
  int b3 = p[0].T.directed_branch(two_way_nodes[5],two_way_nodes[3]);

  p[1].T.exchange_subtrees(b1,b2);
  p[1].tree_propagate(); 
  p[1].LC_invalidate_branch(b);
  p[1].invalidate_subA_index_branch(b);
  
  if (not extends(p[1].T, P.TC))
    return;

  p[2].T.exchange_subtrees(b1,b3);
  p[2].tree_propagate(); 
  p[2].LC_invalidate_branch(b);
  p[2].invalidate_subA_index_branch(b);

  if (not extends(p[2].T, P.TC))
    return;

  vector< vector< int> > nodes;
  for(int i=0;i<p.size();i++)
    nodes.push_back(A3::get_nodes_branch_random(p[i].T, two_way_nodes[4], two_way_nodes[0]) );

  const vector<efloat_t> rho(3,1);

  int C = sample_tri_multi(p,nodes,rho,true,true);

  if (C != -1) {
    P = p[C];
  }

  Stats.inc("NNI (3-way) + A",C>0);
}
