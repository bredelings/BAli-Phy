/*
   Copyright (C) 2004-2009 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

#include <valarray>
#include <iostream>
#include <cmath>
#include <cassert>
#include "sample.H"
#include "probability/choose.H"
#include "util.H"
#include "rng.H"
#include "dp/5way.H"
#include "dp/alignment-sums.H"
#include "alignment/alignment-util.H"
#include "tree/tree-util.H"
#include "substitution/substitution-index.H"

#include "dp/3way.H"
#include "sample.H"


// for prior_HMM_nogiven
#include "likelihood.H"


using MCMC::MoveStats;
using std::string;
using std::vector;

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

/// Update statistics counters for an NNI move.
void NNI_inc(MoveStats& Stats, const string& name, MCMC::Result result,const Tree& T,int b)
{
  Stats.inc(name, result);

  double L = T.directed_branch(b).length();

  if (L < 0.065)
    Stats.inc(name+"-0.065", result);
  else if (L < 0.125)
    Stats.inc(name+"-0.125", result);
  else if (L < 0.25)
    Stats.inc(name+"-0.25", result);
  else if (L < 0.5)
    Stats.inc(name+"-0.5", result);
  else if (L < 1)
    Stats.inc(name+"-1.0", result);
  else if (L < 2.0)
    Stats.inc(name+"-2.0", result);
  else
    Stats.inc(name+"-2.0+", result);
}

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

///Sample between 2 topologies, ignoring gap priors on each case

int two_way_topology_sample(vector<Parameters>& p,const vector<log_double_t>& rho, int b) 
{
  assert(p[0].variable_alignment() == p[1].variable_alignment());
  for(int j=0;j<p[0].n_data_partitions();j++)
    assert(p[0][j].variable_alignment() == p[1][j].variable_alignment());

  vector< A5::hmm_order > orders(2);
  orders[0] = A5::get_nodes_random(p[0].T(), b);
  orders[1] = A5::get_nodes_random(p[1].T(), b);

  try {
    return sample_two_nodes_multi(p,orders,rho,true,false);
  }
  catch (choose_exception<log_double_t>& c)
  {
    c.prepend(__PRETTY_FUNCTION__);
    throw c;
  }
}


#include "slice-sampling.H"

// Notes: the two-say slice sampler is more likely to accept topologies
//        which have a low probability without branch adjustment.
//        
//        If we look at the distribution for accepted changes of
//        log(P2)-log(P1) without adjusting the branch lengths,
//        then 1% are below -3.23 for MH and -6.3 for slice.

//        Also, 2% are below -5.19 for slice, adjusting for the fact
//        that there are fewer (half?) acceptances for slice.

//        Finally, the bottom 5% for slice make up the bottom .7%
//        (unadjusted) for slice...

//        Experiment #1: load in a posterior sample, to find out what the acceptance
//                       probability *should* be based only on the relative probabilities
//                       of the topologies.
//
//        Experiment #2: for each proposal, do a long MCMC chain where we consider only two
//                       topologies but allow branch lengths to vary, in order to find out
//                       what the acceptance probability should be.

// check the HMM type for a DIRECTED branch without going off the end of the array
int HMM_type_for_branch(const Parameters& P, int b)
{
  const Tree& T = P.T();
  b = T.directed_branch(b).undirected_name();
  assert(0 <= b and b < P.branch_HMM_type.size());
  return P.branch_HMM_type[b];
}

void two_way_topology_slice_sample(owned_ptr<Probability_Model>& P, MoveStats& Stats, int b) 
{
  Parameters& PP = *P.as<Parameters>();
  if (PP.T().directed_branch(b).is_leaf_branch()) return;

  if (PP.variable_alignment() and HMM_type_for_branch(PP,b) == 1) return;

  Tree T0 = PP.T();

  A5::hmm_order order = A5::get_nodes_random(PP.T(), b);
  const auto& nodes = order.nodes;

  PP.select_root(b);
  // P.likelihood();  Why does this not make a difference in speed?

  vector<Parameters> p(2,PP);

  int b1 = p[1].T().directed_branch(nodes[4],nodes[1]);
  int b2 = p[1].T().directed_branch(nodes[5],nodes[2]);

  // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
  p[1].exchange_subtrees(b1, b2);
  p[1].LC_invalidate_branch(b);
  p[1].invalidate_subA_index_branch(b);
  
  if (not extends(p[1].T(), *PP.TC))
    return;

  double L = PP.T().directed_branch(b).length();

  //  We cannot evaluate Pr2 here unless -t: internal node states could be inconsistent!
  //  double Pr1 = log(p[0].probability());
  //  double Pr2 = log(p[1].probability());

  branch_length_slice_function logp1(p[0],b);
  branch_length_slice_function logp2(p[1],b);

  vector<slice_function*> logp;
  logp.push_back(&logp1);
  logp.push_back(&logp2);

  double w = PP.branch_mean();

  //  std::pair<int,double> choice = two_way_slice_sample(L,logp1,logp2,w,-1,true,0,false,0);
  std::pair<int,double> choice = slice_sample_multi(L,logp,w,-1);

  int C = choice.first;
  if (C == -1) return;

  if (choice.first == 0)
    PP = p[0];
  else
    PP = p[1];

  MCMC::Result result(3);

  result.totals[0] = (C>0)?1:0;
  // This gives us the average length of branches prior to successful swaps
  if (C>0)
    result.totals[1] = L;
  else
    result.counts[1] = 0;
  result.totals[2] = std::abs(PP.T().directed_branch(b).length() - L);

  //  if (C == 1) std::cerr<<"slice-diff = "<<Pr2 - Pr1<<"\n";

  NNI_inc(Stats,"NNI (2-way,slice)", result, T0, b);
}

void two_way_topology_sample(owned_ptr<Probability_Model>& P, MoveStats& Stats, int b) 
{
  Parameters& PP = *P.as<Parameters>();
  if (PP.T().directed_branch(b).is_leaf_branch()) return;

  if (PP.variable_alignment() and HMM_type_for_branch(PP,b) == 1) return;

  double slice_fraction = PP.load_value("NNI_slice_fraction",-0.25);

  if (not PP.variable_alignment() and uniform() < slice_fraction) {
    two_way_topology_slice_sample(P,Stats,b);
    return;
  }

  A5::hmm_order order = A5::get_nodes_random(PP.T(), b);
  const auto& nodes = order.nodes;

  PP.select_root(b);
  // PP.likelihood();  Why does this not make a difference in speed?

  vector<Parameters> p(2,PP);

  int b1 = p[1].T().directed_branch(nodes[4],nodes[1]);
  int b2 = p[1].T().directed_branch(nodes[5],nodes[2]);

  // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
  p[1].exchange_subtrees(b1, b2);
  p[1].LC_invalidate_branch(b);
  p[1].invalidate_subA_index_branch(b);
  
  if (not extends(p[1].T(), *PP.TC))
    return;

  //  We cannot evaluate Pr2 here unless -t: internal node states could be inconsistent!
  //  double Pr1 = log(p[0].probability());
  //  double Pr2 = log(p[1].probability());

  vector<log_double_t> rho(2,1);

  // Because we would select between topologies before selecting
  // internal node states, the reverse distribution cannot depend on 
  // the internal node state of the proposed new topology/alignment

  int C = two_way_topology_sample(p,rho,b);

  if (C != -1) {
    PP = p[C];
  }

  //  if (C == 1) std::cerr<<"MH-diff = "<<Pr2 - Pr1<<"\n";

  MCMC::Result result(2);

  result.totals[0] = (C>0)?1:0;
  // This gives us the average length of branches prior to successful swaps
  if (C>0)
    result.totals[1] = p[0].T().directed_branch(b).length();
  else
    result.counts[1] = 0;

  NNI_inc(Stats,"NNI (2-way)", result,p[0].T(),b);
}

void two_way_NNI_SPR_sample(owned_ptr<Probability_Model>& P, MoveStats& Stats, int b) 
{
  Parameters& PP = *P.as<Parameters>();
  if (PP.T().directed_branch(b).is_leaf_branch()) return;

  if (PP.variable_alignment() and HMM_type_for_branch(PP,b) == 1) return;

  A5::hmm_order order = A5::get_nodes_random(PP.T(), b);
  const auto& nodes = order.nodes;

  PP.select_root(b);
  // PP.likelihood();  Why does this not make a difference in speed?

  vector<Parameters> p(2,PP);

  int b1 = p[1].T().directed_branch(nodes[4],nodes[1]);
  int b2 = p[1].T().directed_branch(nodes[5],nodes[2]);

  // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
  p[1].exchange_subtrees(b1, b2);
  p[1].LC_invalidate_branch(b);
  p[1].invalidate_subA_index_branch(b);
  
  if (not extends(p[1].T(), *PP.TC))
    return;

  double LA = p[0].T().branch(nodes[4],nodes[0]).length();
  double LB = p[0].T().branch(nodes[4],nodes[5]).length();
  double LC = p[0].T().branch(nodes[5],nodes[3]).length();

  p[1].setlength(p[1].T().branch(nodes[0],nodes[4]),LA + LB);
  p[1].setlength(p[1].T().branch(nodes[4],nodes[5]),LC*uniform());
  p[1].setlength(p[1].T().branch(nodes[5],nodes[3]),LC - p[1].T().branch(nodes[4],nodes[5]).length());

  vector<log_double_t> rho(2,1);
  rho[1] = LC/(LA+LB);

  int C = two_way_topology_sample(p,rho,b);

  if (C != -1) {
    PP = p[C];
  }


  MCMC::Result result(2);

  result.totals[0] = (C>0)?1:0;
  // This gives us the average length of branches prior to successful swaps
  if (C>0)
    result.totals[1] = p[0].T().directed_branch(b).length();
  else
    result.counts[1] = 0;
  NNI_inc(Stats,"NNI (2-way/SPR)", result, p[0].T(), b);
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

void two_way_NNI_and_branches_sample(owned_ptr<Probability_Model>& P, MoveStats& Stats, int b) 
{
  Parameters& PP = *P.as<Parameters>();
  if (PP.T().directed_branch(b).is_leaf_branch()) return;

  if (PP.variable_alignment() and HMM_type_for_branch(PP,b) == 1) return;

  A5::hmm_order order = A5::get_nodes_random(PP.T(),b);
  const auto& nodes = order.nodes;

  PP.select_root(b);
  // PP.likelihood();  Why does this not make a difference in speed?

  vector<Parameters> p(2,PP);

  //---------------- Do the NNI operation -------------------//
  int b1 = p[1].T().directed_branch(nodes[4],nodes[1]);
  int b2 = p[1].T().directed_branch(nodes[5],nodes[2]);

  // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
  p[1].exchange_subtrees(b1, b2);
  p[1].LC_invalidate_branch(b);
  p[1].invalidate_subA_index_branch(b);
  
  if (not extends(p[1].T(), *PP.TC))
    return;

  //------------- Propose new branch lengths ----------------//
  double ratio = 1.0;
  vector<int> branches = NNI_branches(p[1].T(), b);

  for(int i=0;i<branches.size();i++) {

    double factor = exp(gaussian(0,0.05));

    double L = p[1].T().directed_branch( branches[i] ).length() * factor;

    p[1].setlength(branches[i], L);

    ratio *= factor;
  }


  vector<log_double_t> rho(2);
  rho[0] = 1.0;
  rho[1] = ratio;

  int C = two_way_topology_sample(p,rho,b);

  if (C != -1) {
    PP = p[C];
  }

  MCMC::Result result(2);

  result.totals[0] = (C>0)?1:0;
  // This gives us the average length of branches prior to successful swaps
  if (C>0)
    result.totals[1] = p[0].T().directed_branch(b).length();
  else
    result.counts[1] = 0;

  NNI_inc(Stats,"NNI (2-way) + branches", result, p[0].T(), b);
}

void two_way_NNI_sample(owned_ptr<Probability_Model>& P, MoveStats& Stats, int b) 
{
  Parameters& PP = *P.as<Parameters>();
  if (PP.T().directed_branch(b).is_leaf_branch()) return;

  if (PP.variable_alignment() and HMM_type_for_branch(PP,b) == 1) return;

  double U = uniform();
  if (U < 0.33333333) {
    two_way_topology_sample(P,Stats,b);
  }
  else if (U < 0.66666666)
    two_way_NNI_SPR_sample(P,Stats,b);
  else
    two_way_NNI_and_branches_sample(P,Stats,b);
}

/// This has to be Gibbs, and use the same substitution::Model in each case...

int three_way_topology_sample(vector<Parameters>& p, const vector<log_double_t>& rho, int b) 
{
  assert(p[0].variable_alignment() == p[1].variable_alignment());
  assert(p[1].variable_alignment() == p[2].variable_alignment());

  vector< A5::hmm_order > orders(3);
  orders[0] = A5::get_nodes_random(p[0].T(), b);
  orders[1] = A5::get_nodes_random(p[1].T(), b);
  orders[2] = A5::get_nodes_random(p[2].T(), b);

  try {
    return sample_two_nodes_multi(p,orders,rho,true,false);
  }
  catch (choose_exception<log_double_t>& c)
  {
    c.prepend(__PRETTY_FUNCTION__);
    throw c;
  }
}


void three_way_topology_sample_slice(owned_ptr<Probability_Model>& P, MoveStats& Stats, int b) 
{
  Parameters& PP = *P.as<Parameters>();
  if (PP.T().directed_branch(b).is_leaf_branch()) return;

  if (PP.variable_alignment()) return;

  Tree T0 = PP.T();

  A5::hmm_order order = A5::get_nodes_random(PP.T(),b);
  const auto& nodes = order.nodes;

  //------ Generate Topologies and alter caches ------///
  PP.select_root(b);
  // PP.likelihood();  Why does this not make a difference in speed?
  
  vector<Parameters> p(3,PP);

  int b1 = PP.T().directed_branch(nodes[4],nodes[1]);
  int b2 = PP.T().directed_branch(nodes[5],nodes[2]);
  int b3 = PP.T().directed_branch(nodes[5],nodes[3]);

  // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
  p[1].exchange_subtrees(b1, b2);
  p[1].LC_invalidate_branch(b);
  p[1].invalidate_subA_index_branch(b);

  if (not extends(p[1].T(), *PP.TC))
    return;

  // Internal node states may be inconsistent after this: p[2].alignment_prior() undefined!
  p[2].exchange_subtrees(b1, b3);
  p[2].LC_invalidate_branch(b);
  p[2].invalidate_subA_index_branch(b);
  
  if (not extends(p[2].T(), *PP.TC))
    return;

  const vector<log_double_t> rho(3,1);

  double L = PP.T().directed_branch(b).length();

#ifndef NDEBUG
  //  We cannot evaluate Pr2 here unless -t: internal node states could be inconsistent!
  log_double_t Pr1 = p[0].heated_probability();
  log_double_t Pr2 = p[1].heated_probability();
  log_double_t Pr3 = p[2].heated_probability();
#endif

  branch_length_slice_function logp1(p[0],b);
  branch_length_slice_function logp2(p[1],b);
  branch_length_slice_function logp3(p[2],b);

#ifndef NDEBUG
  assert(std::abs(Pr1.log() - logp1(L)) < 1.0e-9);
  assert(std::abs(Pr2.log() - logp2(L)) < 1.0e-9);
  assert(std::abs(Pr3.log() - logp3(L)) < 1.0e-9);
#endif

  vector<slice_function*> logp;
  logp.push_back(&logp1);
  logp.push_back(&logp2);
  logp.push_back(&logp3);

  double w = PP.branch_mean();

  std::pair<int,double> choice = slice_sample_multi(L,logp,w,-1);

  int C = choice.first;
  if (C != -1)
    PP = p[C];

  MCMC::Result result(4);

  result.totals[0] = (C>0)?1:0;
  // This gives us the average length of branches prior to successful swaps
  if (C>0)
    result.totals[1] = L;
  else
    result.counts[1] = 0;
  result.totals[2] = std::abs(PP.T().directed_branch(b).length() - L);
  result.totals[3] = logp1.count + logp2.count + logp3.count;

  //  if (C == 1) std::cerr<<"slice-diff3 = "<<Pr2 - Pr1<<"\n";
  //  if (C == 2) std::cerr<<"slice-diff3 = "<<Pr3 - Pr1<<"\n";

  // stats are here mis-reported!
  NNI_inc(Stats,"NNI (3-way,slice)", result, T0, b);
}

void three_way_topology_sample(owned_ptr<Probability_Model>& P, MoveStats& Stats, int b) 
{
  Parameters& PP = *P.as<Parameters>();
  if (PP.T().directed_branch(b).is_leaf_branch()) return;

  if (PP.variable_alignment() and HMM_type_for_branch(PP,b) == 1)
    return;

  double slice_fraction = PP.load_value("NNI_slice_fraction",-0.25);

  if (not PP.variable_alignment() and uniform() < slice_fraction) {
    three_way_topology_sample_slice(P,Stats,b);
    return;
  }

  A5::hmm_order order = A5::get_nodes_random(PP.T(),b);
  const auto& nodes = order.nodes;

  //------ Generate Topologies and alter caches ------///
  PP.select_root(b);
  // PP.likelihood();  Why does this not make a difference in speed?

  vector<Parameters> p(3,PP);

  int b1 = PP.T().directed_branch(nodes[4],nodes[1]);
  int b2 = PP.T().directed_branch(nodes[5],nodes[2]);
  int b3 = PP.T().directed_branch(nodes[5],nodes[3]);

  // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
  p[1].exchange_subtrees(b1, b2);
  p[1].LC_invalidate_branch(b);
  p[1].invalidate_subA_index_branch(b);

  if (not extends(p[1].T(), *PP.TC))
    return;

  // Internal node states may be inconsistent after this: p[2].alignment_prior() undefined!
  p[2].exchange_subtrees(b1, b3);
  p[2].LC_invalidate_branch(b);
  p[2].invalidate_subA_index_branch(b);
  
  if (not extends(p[2].T(), *PP.TC))
    return;

  const vector<log_double_t> rho(3,1);

  //------ Resample alignments and select topology -----//
  int C = three_way_topology_sample(p,rho,b);

  if (C != -1) {
    PP = p[C];
  }    

  MCMC::Result result(2);

  result.totals[0] = (C>0)?1:0;
  // This gives us the average length of branches prior to successful swaps
  if (C>0)
    result.totals[1] = p[0].T().directed_branch(b).length();
  else
    result.counts[1] = 0;

  NNI_inc(Stats,"NNI (3-way)", result, p[0].T(), b);
}

void three_way_topology_and_alignment_sample(owned_ptr<Probability_Model>& P, MoveStats& Stats, int b) 
{
  Parameters& PP = *P.as<Parameters>();
  if (PP.T().directed_branch(b).is_leaf_branch()) return;

  if (PP.variable_alignment() and HMM_type_for_branch(PP,b) == 1)
    return;

  A5::hmm_order order = A5::get_nodes_random(PP.T(), b);
  const auto& two_way_nodes = order.nodes;

  //--------- Generate the Different Topologies -------//
  // We ALWAYS resample the connection between two_way_nodes [0] and [4].

  vector<Parameters> p(3,PP);
  int b1 = p[0].T().directed_branch(two_way_nodes[4],two_way_nodes[1]);
  int b2 = p[0].T().directed_branch(two_way_nodes[5],two_way_nodes[2]);
  int b3 = p[0].T().directed_branch(two_way_nodes[5],two_way_nodes[3]);

  // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
  p[1].exchange_subtrees(b1, b2);
  p[1].LC_invalidate_branch(b);
  p[1].invalidate_subA_index_branch(b);
 
  p[1].recompute_pairwise_alignment(b1);
  p[1].note_alignment_changed_on_branch(b2);

  if (not extends(p[1].T(), *PP.TC))
    return;

  // Internal node states may be inconsistent after this: p[2].alignment_prior() undefined!
  p[2].exchange_subtrees(b1, b3);
  p[2].LC_invalidate_branch(b);
  p[2].invalidate_subA_index_branch(b);

  p[2].recompute_pairwise_alignment(b1);
  p[2].note_alignment_changed_on_branch(b3);

  if (not extends(p[2].T(), *PP.TC))
    return;

  vector< vector< int> > nodes;
  for(int i=0;i<p.size();i++)
    nodes.push_back(A3::get_nodes_branch_random(p[i].T(), two_way_nodes[4], two_way_nodes[0]) );

  const vector<log_double_t> rho(3,1);

  int C = -1;
  try {
    C = sample_tri_multi(p,nodes,rho,true,true);
  }
  catch (choose_exception<log_double_t>& c)
  {
    c.prepend(__PRETTY_FUNCTION__);
    throw c;
  }

  if (C != -1) {
    PP = p[C];
  }
    
  MCMC::Result result(2);
    
  result.totals[0] = (C>0)?1:0;
  // This gives us the average length of branches prior to successful swaps
  if (C>0)
    result.totals[1] = p[0].T().directed_branch(b).length();
  else
    result.counts[1] = 0;
  
  NNI_inc(Stats,"NNI (3-way) + A", result, p[0].T(), b);
}
