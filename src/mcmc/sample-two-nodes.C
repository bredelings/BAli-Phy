/*
   Copyright (C) 2004-2007,2009-2014 Benjamin Redelings

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
/// \file sample-two-nodes.C
///
/// \brief Contains routines for resampling the sequence at two adjacent internal nodes (5way).
///

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
#include "alignment/alignment-util2.H"
#include "alignment/alignment-constraint.H"
#include "dp/dp-array.H"

using std::vector;
using std::abs;
using std::endl;

using boost::dynamic_bitset;
using boost::shared_ptr;

shared_ptr<DParrayConstrained>
sample_two_nodes_base(data_partition P, const data_partition& P0, const A5::hmm_order& order, const A5::hmm_order& order0)
{
  assert(P.variable_alignment());
  HMM m12345 = A5::get_HMM(P,order);

  /*------- Get column order from (A0,T0,nodes) --------*/

  /*  OK, so, what does it mean to get the column order from the first alignment?
      I guess it means that we used (A0,T0,nodes0) to get an order.  We then keep that order
      for use with (A[i],T[i],nodes[i]).  */

  vector<HMM::bitmask_t> a123456 = A5::get_bitpath(P0, order0);
  a123456 = remap_bitpath(a123456, order0.nodes, order.nodes);
  vector<HMM::bitmask_t> a1234 = remove_silent(a123456, m12345.all_bits() & ~m12345.hidden_bits);

  /*-------------- Create DP matrices ---------------*/

  shared_ptr<DParrayConstrained> Matrices ( new DParrayConstrained(a1234.size(), m12345) );

  // collect the silent-or-correct-emissions for each type columns
  vector< vector<int> > allowed_states_for_mask(16);
  for(auto& m: allowed_states_for_mask)
    m.reserve(Matrices->n_dp_states());

  for(int S2: Matrices->dp_order())
  {
    unsigned int mask = (m12345.state_emit[S2] & ~m12345.hidden_bits).to_ulong();

    // Hidden states never contradict an emission pattern.
    if (not mask)
      for(int j=0;j<16;j++)
	allowed_states_for_mask[j].push_back(S2);
    else
      allowed_states_for_mask[mask].push_back(S2);
  }

  Matrices->states(0) = Matrices->dp_order();

  // Determine which states are allowed to match (c2)
  for(int c2=0;c2<Matrices->size()-1;c2++) 
  {
    unsigned mask=a1234[c2].to_ulong();

    Matrices->states(c2+1) = allowed_states_for_mask[mask];
  }

  //------------------ Compute the DP matrix ---------------------//

  Matrices->forward();

  // If the DP matrix ended up having probability 0, don't try to sample a path through it!
  if (Matrices->Pr_sum_all_paths() <= 0.0) 
  {
    std::cerr<<"sample_two_nodes_base( ): All paths have probability 0!"<<std::endl;
    return Matrices;
  }

  //------------- Sample a path from the matrix -------------------//

  vector<int> path_g = Matrices->sample_path();
  vector<int> path = Matrices->ungeneralize(path_g);

  const auto& nodes = order.nodes;

  P.set_pairwise_alignment(P.t().find_branch(nodes[0],nodes[4]), get_pairwise_alignment_from_path(path, *Matrices, 0, 4));
  P.set_pairwise_alignment(P.t().find_branch(nodes[1],nodes[4]), get_pairwise_alignment_from_path(path, *Matrices, 1, 4));
  P.set_pairwise_alignment(P.t().find_branch(nodes[2],nodes[5]), get_pairwise_alignment_from_path(path, *Matrices, 2, 5));
  P.set_pairwise_alignment(P.t().find_branch(nodes[3],nodes[5]), get_pairwise_alignment_from_path(path, *Matrices, 3, 5));
  P.set_pairwise_alignment(P.t().find_branch(nodes[4],nodes[5]), get_pairwise_alignment_from_path(path, *Matrices, 4, 5));

#ifndef NDEBUG_DP
  check_alignment(P.A(), P.t(), "sample_two_nodes_base:out");
#endif

  return Matrices;
}

///(a[0],p[0]) is the point from which the proposal originates, and must be valid.
int sample_two_nodes_multi(vector<Parameters>& p,const vector<A5::hmm_order>& order_,
			   const vector<log_double_t>& rho_,bool do_OS,bool do_OP) 
{
  for(int i=1;i<p.size();i++)
    for(int j=0;j<p[0].n_data_partitions();j++)
      assert(p[0][j].variable_alignment() == p[i][j].variable_alignment());

  vector<A5::hmm_order> order = order_;
  vector<log_double_t> rho = rho_;
  assert(p.size() == order.size());
  
/*
  //------------ Check the alignment branch constraints ------------//
  for(int i=0;i<p.size();i++) {
    vector<int> branches;

    branches.push_back(p[i].t().branch(order[i].nodes[0], order[i].nodes[4]));
    branches.push_back(p[i].t().branch(order[i].nodes[1], order[i].nodes[4]));
    branches.push_back(p[i].t().branch(order[i].nodes[2], order[i].nodes[5]));
    branches.push_back(p[i].t().branch(order[i].nodes[3], order[i].nodes[5]));
    branches.push_back(p[i].t().branch(order[i].nodes[4], order[i].nodes[5]));

    if (any_branches_constrained(branches, p[i].t(), p[i].PC->TC, p[i].PC->AC))
      return -1;
  }
*/
  
  //----------- Generate the different states and Matrices ---------//
  log_double_t C1 = A5::correction(p[0],order[0]);
#if !defined(NDEBUG_DP) || !defined(NDEBUG)
  const Parameters P0 = p[0];
#endif

  vector< vector< shared_ptr<DParrayConstrained> > > Matrices(p.size());
  for(int i=0;i<p.size();i++) 
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (p[i][j].variable_alignment())
      {
	Matrices[i].push_back(sample_two_nodes_base(p[i][j], p[0][j], order[i], order[0]));
	//    p[i][j].LC.invalidate_node(p[i].T,order[i][4]);
	//    p[i][j].LC.invalidate_node(p[i].T,order[i][5]);
#ifndef NDEBUG
	p[i][j].likelihood();  // check the likelihood calculation
#endif
      }
      else
	Matrices[i].push_back(NULL);
	


  //-------- Calculate corrections to path probabilities ---------//

  vector< vector<log_double_t> > OS(p.size());
  vector< vector<log_double_t> > OP(p.size());

  for(int i=0; i<p.size(); i++) 
  {
    if (do_OS)
      for(int j=0;j<p[i].n_data_partitions();j++)
	OS[i].push_back( p[i][j].likelihood() );
    else
      OS[i] = vector<log_double_t>(p[i].n_data_partitions(),log_double_t(1));
    
    if (do_OP)
      for(int j=0;j<p[i].n_data_partitions();j++)
	OP[i].push_back( other_prior(p[i][j],order[i].nodes) );
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

#ifndef NDEBUG_DP
  std::cerr<<"choice = "<<C<<endl;

  // One mask for all p[i] assumes that only ignored nodes can be renamed
  dynamic_bitset<> ignore(p[0].t().n_nodes());
  ignore[ order[0].nodes[4] ] = true;
  ignore[ order[0].nodes[5] ] = true;

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
  order.push_back(order[0]);
  rho.push_back( rho[0] );
  Matrices.push_back( Matrices[0] );
  OS.push_back( OS[0] );
  OP.push_back( OP[0] );

  vector< vector< vector<int> > >paths(p.size());

  //------------------- Check offsets from path_Q -> P -----------------//
  for(int i=0;i<p.size();i++) 
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (p[i][j].variable_alignment())
      {
	paths[i].push_back( get_path_unique(A5::get_bitpath(p[i][j], order[i]), *Matrices[i][j]) );
    
	OS[i][j] = p[i][j].likelihood();
	OP[i][j] = other_prior(p[i][j],order[i].nodes);
	
	log_double_t OP_i = OP[i][j] / A5::correction(p[i][j],order[i]);
	
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
      
    // sample_P(p[i][j], choice_ratio, rho[i], paths[i][j], *Matrices[i][j]);
    // PR[i][j][0] *= A5::correction(p[i][j],nodes[i]);
    PR[i] = vector<log_double_t>(4,1);
    PR[i][0] = p[i].heated_probability(); // p[i].prior_no_alignment() * p[i].prior_alignment() * p[i].likelihood();
    PR[i][2] = rho[i];
    PR[i][3] = choice_ratio;
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (p[i][j].variable_alignment()) {
	vector<int> path_g = Matrices[i][j]->generalize(paths[i][j]);
	PR[i][0] *= A5::correction(p[i][j],order[i]);
	PR[i][1] *= Matrices[i][j]->path_P(path_g)* Matrices[i][j]->generalize_P(paths[i][j]);
      } 
  }

  //--------- Check that each choice is sampled w/ the correct Probability ---------//
  check_sampling_probabilities(PR);
#endif

  //---------------- Adjust for length of n4 and n5 changing --------------------//

  // if we reject the move, then don't do anything
  log_double_t C2 = A5::correction(p[C],order[C]);
  if (uniform() > double(C1/C2))
    return -1;

  return C;
}


void sample_two_nodes(Parameters& P,int b) 
{
  vector<Parameters> p(1,P);

  vector< A5::hmm_order > order(1);
  order[0] = A5::get_nodes_random(P.t(), b);

  vector<log_double_t> rho(1,1);

  int C = sample_two_nodes_multi(p,order,rho,false,false);

  if (C != -1) {
    P = p[C];
  }
}
