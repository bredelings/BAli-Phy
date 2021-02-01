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
#include "util/assert.hh"
#include "sample.H"
#include "probability/choose.H"
#include "util/mapping.H"
#include "util/rng.H"
#include "dp/2way.H"
#include "dp/5way.H"
#include "dp/alignment-sums.H"
#include "alignment/alignment-util.H"
#include "alignment/alignment-util2.H"
#include "alignment/alignment-constraint.H"
#include "dp/dp-array.H"

using std::vector;
using std::optional;
using std::abs;
using std::endl;
using std::shared_ptr;

using boost::dynamic_bitset;

shared_ptr<DParrayConstrained>
sample_two_nodes_base(mutable_data_partition P, const vector<HMM::bitmask_t>& a123456, const A5::hmm_order& order, const A5::hmm_order& order0)
{
    assert(P.variable_alignment());
    HMM m12345 = A5::get_HMM(P,order);

    /*------- Get column order from (A0,T0,nodes) --------*/

    /*  OK, so, what does it mean to get the column order from the first alignment?
	I guess it means that we used (A0,T0,nodes0) to get an order.  We then keep that order
	for use with (A[i],T[i],nodes[i]).  */

    auto a123456_remapped = remap_bitpath(a123456, compute_mapping(order0.nodes, order.nodes));
    vector<HMM::bitmask_t> a1234 = remove_silent(a123456_remapped, m12345.all_bits() & ~m12345.hidden_bits);

    /*-------------- Create DP matrices ---------------*/

    shared_ptr<DParrayConstrained> Matrices ( new DParrayConstrained(a1234.size(), m12345) );

    // collect the silent-or-correct-emissions for each type columns
    vector< vector<int> > allowed_states_for_mask(16);
    for(auto& m: allowed_states_for_mask)
	m.reserve(Matrices->n_dp_states());

    for(int S2: Matrices->dp_order())
    {
	auto mask = (m12345.state_emit[S2] & ~m12345.hidden_bits).raw();

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
	auto mask=a1234[c2].raw();

	Matrices->states(c2+1) = allowed_states_for_mask[mask];
    }

    //------------------ Compute the DP matrix ---------------------//

    Matrices->forward();

    //------------- Sample a path from the matrix -------------------//

    const auto& nodes = order.nodes;

    int b04 = P.t().find_branch(nodes[0],nodes[4]);
    int b14 = P.t().find_branch(nodes[1],nodes[4]);
    int b25 = P.t().find_branch(nodes[2],nodes[5]);
    int b35 = P.t().find_branch(nodes[3],nodes[5]);
    int b45 = P.t().find_branch(nodes[4],nodes[5]);

    // If the DP matrix ended up having probability 0, don't try to sample a path through it!
    if (Matrices->Pr_sum_all_paths() <= 0.0) 
    {
	std::cerr<<"sample_two_nodes_base( ): All paths have probability 0!"<<std::endl;

        // Compute likelihood when pairwise alignments disagree about internal sequence length
        // we can get memory access errors.
        P.unset_pairwise_alignment(b04);
        P.unset_pairwise_alignment(b14);
        P.unset_pairwise_alignment(b25);
        P.unset_pairwise_alignment(b35);
        P.unset_pairwise_alignment(b45);
    }
    else
    {
        vector<int> path_g = Matrices->sample_path();
        vector<int> path = Matrices->ungeneralize(path_g);

        P.set_pairwise_alignment(b04, get_pairwise_alignment_from_path(path, *Matrices, 0, 4));
        P.set_pairwise_alignment(b14, get_pairwise_alignment_from_path(path, *Matrices, 1, 4));
        P.set_pairwise_alignment(b25, get_pairwise_alignment_from_path(path, *Matrices, 2, 5));
        P.set_pairwise_alignment(b35, get_pairwise_alignment_from_path(path, *Matrices, 3, 5));
        P.set_pairwise_alignment(b45, get_pairwise_alignment_from_path(path, *Matrices, 4, 5));
    }

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

    double heat_beta = p[0].get_beta();
    vector<optional<vector<HMM::bitmask_t>>> a123456(p[0].n_data_partitions());

    for(int j=0;j<p[0].n_data_partitions();j++)
        if (p[0][j].has_pairwise_alignments())
            a123456[j] = A5::get_bitpath(p[0][j], order[0]);
  
    vector< vector<log_double_t> > OS(p.size());
    vector< vector<log_double_t> > OP(p.size());
    vector<log_double_t> Pr(p.size());

    //----------- Generate the different states and Matrices ---------//
    log_double_t C1 = A5::correction(p[0],order[0]);
#if !defined(NDEBUG_DP) || !defined(NDEBUG)
    const Parameters P0 = p[0];
#endif

    vector< vector< shared_ptr<DParrayConstrained> > > Matrices(p.size());
    for(int i=0;i<p.size();i++)
    {
	for(int j=0;j<p[i].n_data_partitions();j++)
        {
	    if (p[i][j].variable_alignment())
	    {
		Matrices[i].push_back(sample_two_nodes_base(p[i][j], *a123456[j], order[i], order[0]));
		if (Matrices[i].back()->Pr_sum_all_paths() <= 0.0)
                {
		    std::cerr<<"Pr = 0   i = "<<i<<"   j="<<j<<" \n";
                    // Avoid calculating likelihoods and priors if the alignment is unset.
                    return -1;
                }
#ifndef NDEBUG
		p[i][j].likelihood();  // check the likelihood calculation
#endif
	    }
	    else
		Matrices[i].push_back(NULL);
        }

        //-------- Calculate corrections to path probabilities ---------//
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


        //---------------- Calculate choice probabilities --------------//
        Pr[i] = rho[i] * p[i].prior_no_alignment();

	// sum of substitution and alignment probability over all paths
	for(int j=0;j<p[i].n_data_partitions();j++)
	{
	    Pr[i] *= pow(OS[i][j], heat_beta);
	    if (p[i][j].variable_alignment())
	    {
		Pr[i] *= Matrices[i][j]->Pr_sum_all_paths();
		Pr[i] *= OP[i][j];
	    }
	}
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

    // FIXME: check that alignment of sequences besides the middle 2 is the same between P0[j] and p[i][j]

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
    {
	// check whether this arrangement has probability 0 for some reason.
	bool ok = true;
	for(int j=0;j<p[i].n_data_partitions();j++) 
	    if (p[i][j].variable_alignment() and Matrices[i][j]->Pr_sum_all_paths() <= 0.0) 
		ok = false;

	if (not ok)
	    assert(i != 0 and i != p.size()-1);

	for(int j=0;j<p[i].n_data_partitions();j++)
	    if (p[i][j].variable_alignment() and ok)
	    {
		paths[i].push_back( get_path_unique(A5::get_bitpath(p[i][j], order[i]), *Matrices[i][j]) );
    
		OS[i][j] = p[i][j].likelihood();
		OP[i][j] = other_prior(p[i][j],order[i].nodes);
	
		log_double_t OP_i = OP[i][j] / A5::correction(p[i][j],order[i]);
	
		check_match_P(p[i][j], OS[i][j], OP_i, paths[i][j], *Matrices[i][j]);
	    }
	    else
		paths[i].push_back( vector<int>() );
    }

    //--------- Compute path probabilities and sampling probabilities ---------//
    vector< vector<log_double_t> > PR(p.size(), vector<log_double_t>(4,1));

    for(int i=0;i<p.size();i++) 
    {
	// check whether this arrangement violates a constraint in any partition
	bool ok = true;
	for(int j=0;j<p[i].n_data_partitions();j++)
	    if (p[i][j].variable_alignment() and Matrices[i][j]->Pr_sum_all_paths() <= 0.0) 
		ok = false;

	if (not ok) {
	    PR[i][0] = 0;
	    assert(i != 0 and i != p.size()-1);
	    continue;
	}

	log_double_t choice_ratio = 1;
	if (i<Pr.size())
	    choice_ratio = choose_MH_P(0,i,Pr)/choose_MH_P(i,0,Pr);
	else
	    choice_ratio = 1;
      
	// sample_P(p[i][j], choice_ratio, rho[i], paths[i][j], *Matrices[i][j]);
	// PR[i][j][0] *= A5::correction(p[i][j],nodes[i]);
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
