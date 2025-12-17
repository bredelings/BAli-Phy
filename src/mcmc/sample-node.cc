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
#include "util/rng.H"
#include "util/log-level.H"                         // for log_verbose
#include "dp/2way.H"
#include "dp/3way.H"
#include "dp/alignment-sums.H"
#include "alignment/alignment-util.H"
#include "alignment/alignment-constraint.H"
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
using std::pair;

using boost::dynamic_bitset;
using std::shared_ptr;

pair<shared_ptr<DParrayConstrained>, log_double_t>
sample_node_base(mutable_data_partition P,const vector<int>& nodes)
{
    assert(P.variable_alignment());

    int b1 = P.t().find_branch(nodes[1],nodes[0]);
    int b2 = P.t().find_branch(nodes[0],nodes[2]);
    int b3 = P.t().find_branch(nodes[0],nodes[3]);

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
	auto mask = (m123.state_emit[S2] & ~m123.hidden_bits).raw();
    
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
	auto mask=(a123_emit[c2]&~m123.hidden_bits).raw();
	assert(mask);
    
	Matrices->states(c2+1) = allowed_states_for_mask[mask];
    }
  
    //------------------ Compute the DP matrix ----------------------//
    Matrices->forward();
  
    //------------- Sample a path from the matrix -------------------//

    auto path_g = Matrices->sample_path();
    if (not path_g)
    {
	if (log_verbose > 0) std::cerr<<"sample_node_base( ): path probabilities sum to "<<Matrices->Pr_sum_all_paths()<<"!"<<std::endl;
	return {Matrices, 0};
    }

    vector<int> path = Matrices->ungeneralize(*path_g);

    for(int i=0;i<3;i++) {
	int b = P.t().find_branch(nodes[0],nodes[i+1]);
	P.set_pairwise_alignment(b, get_pairwise_alignment_from_path(path, *Matrices, 3, i));
    }

    // What is the probability that we choose the specific alignment that we did?
    auto sampling_pr = Matrices->path_P(*path_g)* Matrices->generalize_P(path);

    return {Matrices,sampling_pr};
}

int sample_node_multi(vector<Parameters>& p,const vector< vector<int> >& nodes_,
		      const vector<log_double_t>& rho_)
{
    vector<vector<int> > nodes = nodes_;
    vector<log_double_t> rho = rho_; 
    assert(p.size() == nodes.size());
 
    /*
    //------------ Check the alignment branch constraints ------------//
    for(int i=0;i<p.size();i++) {
    vector<int> branches;
    branches.push_back(p[i].t().find_branch(nodes[i][0],nodes[i][1]));
    branches.push_back(p[i].t().find_branch(nodes[i][0],nodes[i][2]));
    branches.push_back(p[i].t().find_branch(nodes[i][0],nodes[i][3]));

    if (any_branches_constrained(branches, p[i].t(), p[i].PC->TC, p[i].PC->AC))
    return -1;
    }
    */
  
    //----------- Generate the different states and Matrices ---------//
    log_double_t C1 = A3::correction(p[0],nodes[0]);
#if !defined(NDEBUG_DP) || !defined(NDEBUG)
    const Parameters P0 = p[0];
#endif

    vector< vector< shared_ptr<DParrayConstrained> > > Matrices(p.size());

    vector<log_double_t> Pr(p.size());
    for(int i=0;i<p.size();i++)
    {
        Pr[i] = rho[i];

#ifndef NDEBUG_DP
        Matrices[i].resize( p[i].n_data_partitions() );
#endif
	for(int j=0;j<p[i].n_data_partitions();j++)
        {
	    if (p[i][j].variable_alignment())
            {
                auto [M, sampling_pr] = sample_node_base(p[i][j],nodes[i]);
                Pr[i] /= sampling_pr;
                Pr[i] *= A3::correction(p[i][j], nodes[i]);
#ifndef NDEBUG_DP
		Matrices[i][j] = M;
#endif
            }
        }

        Pr[i] *= p[i].heated_probability();
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
    if (log_verbose >= 4) std::cerr<<"choice = "<<C<<endl;

    // FIXME: check that alignment of sequences in each group is unchanged.

    // Add another entry for the incoming configuration
    p.push_back( P0 );
    nodes.push_back(nodes[0]);
    rho.push_back( rho[0] );
    Matrices.push_back( Matrices[0] );

    vector< vector< vector<int> > > paths(p.size());

    //------------------- Check offsets from path_Q -> P -----------------//
    for(int i=0;i<p.size();i++) 
	for(int j=0;j<p[i].n_data_partitions();j++) 
	    if (p[i][j].variable_alignment())
	    {
		paths[i].push_back( get_path_unique(A3::get_bitpath(p[i][j], nodes[i]), *Matrices[i][j] ) );
	
		auto OS = p[i][j].likelihood();
		auto OP = other_prior(p[i][j],nodes[i]) / A3::correction(p[i][j],nodes[i]);
	
		check_match_P(p[i][j], OS, OP, paths[i][j], *Matrices[i][j]);
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
    vector<Parameters> p(1,P);

    vector< vector<int> > nodes(1);
    nodes[0] = A3::get_nodes_random(P.t(),node);

    vector<log_double_t> rho(1,1);

    int C = sample_node_multi(p,nodes,rho);

    if (C != -1) {
	P = p[C];
    }
}
