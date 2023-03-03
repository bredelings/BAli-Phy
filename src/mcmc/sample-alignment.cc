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

#include "sample-alignment.H"

#include <valarray>
#include <iostream>
#include <cmath>
#include "sample.H"
#include "dp/2way.H"
#include "dp/alignment-sums.H"
#include "alignment/alignment-constraint.H"
#include "alignment/alignment-util.H"
#include "alignment/alignment-util2.H"
#include "substitution/likelihood.H"
#include "dp/dp-matrix.H"
#include "util/log-level.H"                         // for log_verbose

// SYMMETRY: Because we are only sampling from alignments with the same fixed length
// for both sequences, this process is symmetric

using std::abs;
using std::pair;
using std::vector;
using std::shared_ptr;
using std::optional;
using boost::dynamic_bitset;
using namespace A2;

shared_ptr<DPmatrixSimple> sample_alignment_forward(data_partition P, const TreeInterface& t, const indel::PairHMM& hmm, int b, optional<int> bandwidth)
{
    assert(P.variable_alignment());

    int bb = t.reverse(b);

    // We SHOULD be able to avoid constructing an index here.
    // There should be no question of what order the columns go.
    // `get_indices_n( )` just constructs a 1-row matrix from 0 to n-1.
    auto dists1 = substitution::get_column_likelihoods(P, {b}, get_indices_n(P.seqlength(t.source(b))), 2);

    /*
     * OK, so here we need to handle (i) multifurcating nodes and (ii) non-reversible models.
     *
     * (i) For multifurcating nodes, this should be the same as the peeling routine, except that
     *     we do not peel along the branch.
     *     So we shouldn't need an index, and the order of columns should be unambiguous.
     *
     * (ii) For non-reversible models, we need to do just what the peeling routine does,
     *      including apply root frequencies, and using the transposed P matrix if necessary,
     *      but just not peel.
     *
     * Now, we could try and expose the type of calculator that is being used.  But it would probably
     * be simpler to set this up as a property when we run the annotated likelihood routine.  Then we
     * can simply compute the property without the C++ code having to know how the likelihood calculator works.
     */

    DPmatrixEmit::EmissionProbs dists2;
    if (t.n_nodes() == 2)
    {
	dists2 = substitution::get_leaf_seq_likelihoods(P, t.target(b), 2);
    }
    else
    {
	vector<int> prev = t.branches_before(bb);
	assert(prev.size() == 2);
	auto a0 = convert_to_bits(P.get_pairwise_alignment(prev[0]),0,2);
	auto a1 = convert_to_bits(P.get_pairwise_alignment(prev[1]),1,2);
	auto a012 = Glue_A(a0,a1);
	dists2 = substitution::get_column_likelihoods(P, prev, get_indices_from_bitpath_w(a012,{0,1},(1<<2)), 2);
    }
    //  To handle a 2-node tree, we can do something things for dists2:
    //      dists2 = substitution::get_leaf_seq_likelihoods(P, root, 2);

    vector<HMM::bitmask_t> state_emit(4,0);
    state_emit[0] |= (1<<1)|(1<<0);
    state_emit[1] |= (1<<1);
    state_emit[2] |= (1<<0);
    state_emit[3] |= 0;

    MatrixShape matrix_shape{dists1.n_columns(), dists2.n_columns()};

    auto Matrices = std::make_shared<DPmatrixSimple>
                    (
                         std::move(matrix_shape),
                         HMM(state_emit, hmm.start_pi(), hmm, P.get_beta()),
                         std::move(dists1),
                         std::move(dists2),
                         *P.WeightedFrequencyMatrix()
                    );

    //-------------- Compute ymin and ymax for each x --------------//
    auto yboundaries = yboundaries_everything(*Matrices);

    if (bandwidth)
        yboundaries = yboundaries_simple_band(*Matrices, *bandwidth);

    //------------------ Compute the DP matrix ---------------------//
    Matrices->forward_band(yboundaries);

    return Matrices;
}


pair<shared_ptr<DPmatrixSimple>,log_double_t> sample_alignment_base(mutable_data_partition P, const indel::PairHMM& hmm, int b, optional<int> bandwidth) 
{
    auto Matrices = sample_alignment_forward(P, P.t(), hmm, b, bandwidth);

    // If the DP matrix ended up having probability 0, don't try to sample a path through it!
    if (Matrices->Pr_sum_all_paths() <= 0.0)
    {
	if (log_verbose > 0)
	    std::cerr<<"sample_alignment_base( ): All paths have probability 0!"<<std::endl;
	return {Matrices,{}};
    }

    vector<int> path = Matrices->sample_path();

    P.set_pairwise_alignment(b, A2::get_pairwise_alignment_from_path(path));

    return {Matrices, Matrices->Pr_sum_all_paths() / Matrices->path_Q(path)};
}

pair<shared_ptr<DPmatrixSimple>,log_double_t> sample_alignment_base(mutable_data_partition P, int b, optional<int> bandwidth)
{
    return sample_alignment_base(P, P.get_branch_HMM(b), b, bandwidth);
}

log_double_t sample_alignment(Parameters& P,int b)
{
    //  if (any_branches_constrained(vector<int>(1,b), P.t(), P.PC->TC, P.PC->AC))
    //    return;

    P.select_root(b);

    auto t = P.t();

    if (t.is_leaf_node(t.target(b)))
	b = t.reverse(b);
  
    optional<int> bandwidth;
    if (P.contains_key("simple_bandwidth"))
        bandwidth  = (int)P.lookup_key("simple_bandwidth");

#if !defined(NDEBUG_DP) || !defined(NDEBUG)
    const Parameters P0 = P;
#endif
    vector<Parameters> p;
    p.push_back(P);

    vector< vector< shared_ptr<DPmatrixSimple> > > Matrices(1);
    log_double_t total_ratio = 1.0;
    for(int i=0;i<p.size();i++) 
    {
	for(int j=0;j<p[i].n_data_partitions();j++) 
	    if (p[i][j].variable_alignment()) 
	    {
                auto [M, ratio] = sample_alignment_base(p[i][j], b, bandwidth);
                total_ratio *= ratio;
		Matrices[i].push_back(M);
		// If Pr_sum_all_paths() == 0, then the alignment for this partition will be unchanged.
#ifndef NDEBUG
		p[i][j].likelihood();  // check the likelihood calculation
#endif
	    }
	    else
		Matrices[i].push_back(shared_ptr<DPmatrixSimple>());
    }

#ifndef NDEBUG_DP
    if (log_verbose >=4) std::cerr<<"\n\n----------------------------------------------\n";

    int node1 = P.t().source(b);
    int node2 = P.t().target(b);

    vector<int> nodes;
    nodes.push_back(node1);
    nodes.push_back(node2);

    p.push_back(P0);
    Matrices.push_back(Matrices[0]);

    vector< vector< log_double_t > > OS(p.size());
    vector< vector< log_double_t > > OP(p.size());
    vector< vector< vector<int> > > paths(p.size());

    //------------------- Check offsets from path_Q -> P -----------------//
    for(int i=0;i<p.size();i++) 
	for(int j=0;j<p[i].n_data_partitions();j++) 
	    if (p[i][j].variable_alignment())
	    {
		auto a = p[i][j].get_pairwise_alignment(p[i].t().find_branch(node1,node2));
		paths[i].push_back( get_path_from_pairwise_alignment(a) );
    
		OS[i].push_back( other_subst(p[i][j],nodes) );
		OP[i].push_back( other_prior(p[i][j],nodes) );
	
		check_match_P(p[i][j], OS[i][j], OP[i][j], paths[i][j], *Matrices[i][j]);
	    }
	    else {
		paths[i].push_back( vector<int>() );
		OS[i].push_back( 1 );
		OP[i].push_back( 1 );
	    }
  
    //--------- Compute path probabilities and sampling probabilities ---------//
    vector< vector<log_double_t> > PR(p.size());

    for(int i=0;i<p.size();i++)
    {
	// sample_P(p[i][j], 1, 1, paths[i][j], *Matrices[j]);
	PR[i] = vector<log_double_t>(4,1);
	PR[i][0] = p[i].heated_probability();
	for(int j=0;j<p[i].n_data_partitions();j++) 
	    if (p[i][j].variable_alignment())
	    {
		vector<int> path_g = Matrices[i][j]->generalize(paths[i][j]);
		PR[i][1] *= Matrices[i][j]->path_P(path_g)* Matrices[i][j]->generalize_P(paths[i][j]);
	    }
    }

    if (log_verbose >= 4)
    {
	for(int i=0;i<p[1].n_data_partitions();i++) 
	{
	    if (paths[0][i].size() > paths[1][i].size() and log_verbose > 0)
		std::cerr<<"path "<<i+1<<" got longer by "<<paths[0][0].size() - paths[1][0].size()<<"!\n";
	    if (paths[0][i].size() < paths[1][i].size() and log_verbose > 0)
		std::cerr<<"path "<<i+1<<" got shorter by "<<paths[1][0].size() - paths[0][0].size()<<"!\n";
	}
    }

    //--------- Check that each choice is sampled w/ the correct Probability ---------//
    check_sampling_probabilities(PR);

    //--------- Check construction of A  ---------//
    if (log_verbose >= 4)
    {
	for(int j=0;j<P.n_data_partitions();j++) 
	{
	    double diff = log(PR[0][0]) - log(PR[1][0]);
	    std::cerr<<"before = "<<PR[1][0]<<"       after = "<<PR[0][0]<<
		" diff = "<<diff<<std::endl;

	    if (diff < -10) {
		log_double_t L1 = p[1].likelihood();
		log_double_t L2 = p[0].likelihood();
      
		log_double_t prior1 = p[1].prior();
		log_double_t prior2 = p[0].prior();
      
		std::cerr<<"Yelp!\n";
      
		std::cerr<<std::endl;
		std::cerr<<"DELTA Likelihood = "<<L2/L1<<std::endl;
		std::cerr<<"DELTA prior = "<<prior2/prior1<<std::endl;
		std::cerr<<std::endl;
      
		std::cerr<<"Sampling probability of good path is: "<<PR[1][1]<<std::endl;
	    }
	}
    }

#endif

    P = p[0];

    return total_ratio;
}

