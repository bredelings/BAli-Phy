/*
   Copyright (C) 2004-2007,2009-2012 Benjamin Redelings

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

///
/// \file sample-node.C
///
/// \brief Contains routines for resampling the sequence at an internal node (3way).
///

#include <iostream>
#include <cmath>
#include "sample.H"
#include "probability/choose.H"
#include "util.H"
#include "rng.H"
#include "dp/3way.H"
#include "dp/alignment-sums.H"
#include "alignment/alignment-util.H"
#include "alignment/alignment-constraint.H"
#include "likelihood.H"    // for prior()
#include "substitution/substitution-index.H"
#include <boost/shared_ptr.hpp>
#include "dp/dp-array.H"

//TODO - 1. calculate the probability of 
//  a) the path we came in with
//  b) the path we chose
//  c) the most probable path?

// 2. Calculate the likelihood of the reassembled matrix and the original matrix
//     - see if the difference is the same as the difference between the path probabilities

using std::abs;
using std::vector;
using std::endl;

using boost::dynamic_bitset;
using boost::shared_ptr;

shared_ptr<DParrayConstrained> sample_node_base(data_partition& P,const vector<int>& nodes)
{
  const Tree& T = P.T();

  assert(P.variable_alignment());

  int b1 = T.directed_branch(nodes[1],nodes[0]);
  int b2 = T.directed_branch(nodes[0],nodes[2]);
  int b3 = T.directed_branch(nodes[0],nodes[3]);

  HMM m1 = P.get_branch_HMM(b1);
  m1.remap_bits({0,3});
  HMM m2 = P.get_branch_HMM(b2);
  m2.remap_bits({3,1});
  HMM m3 = P.get_branch_HMM(b3);
  m3.remap_bits({3,2});

  HMM m123 = Glue(m1,Glue(m2,m3));
  m123.hidden_bits.set(3);
  m123.B = P.get_beta();

  vector<HMM::bitmask_t> a1 = convert_to_bits(P.get_pairwise_alignment(b1),0,3);
  vector<HMM::bitmask_t> a2 = convert_to_bits(P.get_pairwise_alignment(b2),3,1);
  vector<HMM::bitmask_t> a3 = convert_to_bits(P.get_pairwise_alignment(b3),3,2);

  vector<HMM::bitmask_t> a123 = Glue_A(a1, Glue_A(a2, a3));
  vector<HMM::bitmask_t> a123_emit = remove_silent(a123, m123.all_bits() & ~m123.hidden_bits);

  shared_ptr<DParrayConstrained> Matrices ( new DParrayConstrained(a123_emit.size(), m123) );
  
  // collect the silent-or-correct-emissions for each type columns
  vector< vector<int> > allowed_states_for_mask(8);
  for(auto& m: allowed_states_for_mask)
    m.reserve(Matrices->n_dp_states());
  
  // Construct the states that are allowed for each emission pattern.
  for(int S2:Matrices->dp_order()) 
  {
    unsigned int mask = (m123.state_emit[S2] & ~m123.hidden_bits).to_ulong();
    
    // Hidden states never contradict an emission pattern.
    if (not mask)
      for(int j=0;j<8;j++)
	allowed_states_for_mask[j].push_back(S2);
    else
      allowed_states_for_mask[mask].push_back(S2);
  }
  
  // All states are allowed to match column 0
  Matrices->states(0) = Matrices->dp_order();
  
  // Determine which states are allowed to match other columns (c2)
  for(int c2=0;c2<a123_emit.size();c2++) 
  {
    unsigned int mask=(a123_emit[c2]&~m123.hidden_bits).to_ulong();
    assert(mask);
    
    Matrices->states(c2+1) = allowed_states_for_mask[mask];
  }
  
  //------------------ Compute the DP matrix ----------------------//
  Matrices->forward();
  
  //------------- Sample a path from the matrix -------------------//

  // If the DP matrix ended up having probability 0, don't try to sample a path through it!
  if (Matrices->Pr_sum_all_paths() <= 0.0)
  {
    std::cerr<<"sample_node_base( ): All paths have probability 0!"<<std::endl;
    return Matrices;
  }

  vector<int> path_g = Matrices->sample_path();
  vector<int> path = Matrices->ungeneralize(path_g);

  for(int i=0;i<3;i++) {
    int b = T.directed_branch(nodes[0],nodes[i+1]);
    P.set_pairwise_alignment(b, get_pairwise_alignment_from_path(path, *Matrices, 3, i), false);
  }

  P.recompute_alignment_matrix_from_pairwise_alignments();

  assert(valid(P.A()));

  return Matrices;
}

int sample_node_multi(vector<Parameters>& p,const vector< vector<int> >& nodes_,
		      const vector<log_double_t>& rho_, bool do_OS,bool do_OP) 
{
  vector<vector<int> > nodes = nodes_;
  vector<log_double_t> rho = rho_; 
  assert(p.size() == nodes.size());
 
  //------------ Check the alignment branch constraints ------------//
  for(int i=0;i<p.size();i++) {
    vector<int> branches;
    branches.push_back(p[i].T().branch(nodes[i][0],nodes[i][1]));
    branches.push_back(p[i].T().branch(nodes[i][0],nodes[i][2]));
    branches.push_back(p[i].T().branch(nodes[i][0],nodes[i][3]));

    if (any_branches_constrained(branches, p[i].T(), p[i].PC->TC, p[i].PC->AC))
      return -1;
  }

  //----------- Generate the different states and Matrices ---------//
  log_double_t C1 = A3::correction(p[0],nodes[0]);
#if !defined(NDEBUG_DP) || !defined(NDEBUG)
  const Parameters P0 = p[0];
#endif

  vector< vector< shared_ptr<DParrayConstrained> > > Matrices(p.size());
  for(int i=0;i<p.size();i++) {
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (p[i][j].variable_alignment())
	Matrices[i].push_back( sample_node_base(p[i][j],nodes[i]) );
      else
	Matrices[i].push_back( shared_ptr<DParrayConstrained>() );
  }

  //-------- Calculate corrections to path probabilities ---------//

  vector< vector<log_double_t> > OS(p.size());
  vector< vector<log_double_t> > OP(p.size());

  for(int i=0; i<p.size(); i++) 
  {
    if (do_OS)
      for(int j=0;j<p[i].n_data_partitions();j++)
	if (p[i][j].variable_alignment())
	  OS[i].push_back( p[i][j].likelihood() );
	else
	  OS[i].push_back( 1 );
    else
      OS[i] = vector<log_double_t>(p[i].n_data_partitions(),log_double_t(1));
    
    if (do_OP)
      for(int j=0;j<p[i].n_data_partitions();j++)
	OP[i].push_back( other_prior(p[i][j],nodes[i]) );
    else
      OP[i] = vector<log_double_t>(p[i].n_data_partitions(),log_double_t(1));
  }

  //---------------- Calculate choice probabilities --------------//
  vector<log_double_t> Pr(p.size());

  for(int i=0;i<Pr.size();i++) 
  {
    Pr[i] = rho[i] * p[i].prior_no_alignment();

    // sum of substitution and alignment probability over all paths
    for(int j=0;j<p[i].n_data_partitions();j++)
      if (p[i][j].variable_alignment())
      {
	Pr[i] *= Matrices[i][j]->Pr_sum_all_paths();
	Pr[i] *= pow(OS[i][j], p[i][j].get_beta());
	Pr[i] *= OP[i][j];
      }
      else
	Pr[i] *= p[i][j].heated_likelihood();
  }

  assert(Pr[0] > 0.0);

  // Fail if Pr[0] is 0
  if (Pr[0] <= 0.0) return -1;

  int C = -1;
  try {
    C = choose_MH(0,Pr);
  }
  catch (choose_exception<log_double_t>& c)
  {
    c.prepend(std::string(__PRETTY_FUNCTION__)+"\n");

    c<<show_parameters(p[0]);
    c<<p[0].probability()<<" = "<<p[0].likelihood()<<" + "<<p[0].prior()<<"\n";

    throw c;
  }

  assert(Pr[C] > 0.0);

#ifndef NDEBUG_DP
  std::cerr<<"choice = "<<C<<endl;

  // One mask for all p[i] assumes that only ignored nodes can be renamed
  dynamic_bitset<> ignore(p[0].t().n_nodes());
  ignore[ nodes[0][0] ] = true;

  // Check that our constraints are met
  for(int i=0;i<p.size();i++) 
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (not A_constant(P0[j].A(), p[i][j].A(), ignore)) {
	std::cerr<<P0[j].A()<<endl;
	std::cerr<<p[i][j].A()<<endl;
	assert(A_constant(P0[j].A(), p[i][j].A(), ignore));
      }

  // Add another entry for the incoming configuration
  p.push_back( P0 );
  nodes.push_back(nodes[0]);
  rho.push_back( rho[0] );
  Matrices.push_back( Matrices[0] );
  OS.push_back( OS[0] );
  OP.push_back( OP[0] );

  vector< vector< vector<int> > > paths(p.size());

  //------------------- Check offsets from path_Q -> P -----------------//
  for(int i=0;i<p.size();i++) 
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (p[i][j].variable_alignment())
      {
	paths[i].push_back( get_path_unique(A3::get_bitpath(p[i][j], nodes[i]), *Matrices[i][j] ) );
	
	OS[i][j] = p[i][j].likelihood();
	OP[i][j] = other_prior(p[i][j],nodes[i]);
	
	log_double_t OP_i = OP[i][j] / A3::correction(p[i][j],nodes[i]);
	
	check_match_P(p[i][j], OS[i][j], OP_i, paths[i][j], *Matrices[i][j]);
      }
      else
	paths[i].push_back( vector<int>() );

  //--------- Compute path probabilities and sampling probabilities ---------//
  vector< vector<log_double_t> > PR(p.size());

  for(int i=0;i<p.size();i++) 
  {
    log_double_t choice_ratio = 1;
    if (i<Pr.size())
      choice_ratio = choose_MH_P(0,i,Pr)/choose_MH_P(i,0,Pr);
    else
      choice_ratio = 1;
    
    
    //    sample_P(p[i], choice_ratio, rho[i], paths[i], Matrices[i]) );
    //    PR[i][j][0] *= A3::correction(p[i][j],nodes[i]);
    PR[i] = vector<log_double_t>(4,1);
    PR[i][0] = p[i].heated_probability();
    PR[i][2] = rho[i];
    PR[i][3] = choice_ratio;
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (p[i][j].variable_alignment())
      {
	vector<int> path_g = Matrices[i][j]->generalize(paths[i][j]);
	PR[i][0] *= A3::correction(p[i][j],nodes[i]);
	PR[i][1] *= Matrices[i][j]->path_P(path_g)* Matrices[i][j]->generalize_P(paths[i][j]);
      }
  }

  //--------- Check that each choice is sampled w/ the correct Probability ---------//
  check_sampling_probabilities(PR);
#endif

  //---------------- Adjust for length of n4 and n5 changing --------------------//

  // if we reject the move, then don't do anything
  //FIXME - PARTITION - compute and cache P0 part before changing p[0], then we can
  //                     throw P0 away if we want to.
  log_double_t C2 = A3::correction(p[C],nodes[C]);
  if (uniform() > double(C1/C2))
    return -1;

  return C;
}





void sample_node(Parameters& P,int node) 
{
  const Tree& T = P.T();

  vector<Parameters> p(1,P);

  vector< vector<int> > nodes(1);
  nodes[0] = A3::get_nodes_random(T,node);

  vector<log_double_t> rho(1,1);

  int C = sample_node_multi(p,nodes,rho,false,false);

  if (C != -1) {
    P = p[C];
  }
}
