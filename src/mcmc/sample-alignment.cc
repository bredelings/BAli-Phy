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
#include "util/settings.H"                          // for get_setting_or( )

// SYMMETRY: Because we are only sampling from alignments with the same fixed length
// for both sequences, this process is symmetric

using std::abs;
using std::pair;
using std::vector;
using std::shared_ptr;
using std::optional;
using boost::dynamic_bitset;
using namespace A2;

/*
 * So we are are doing calc_prob_at_root( ) or calc_prob_not_at_root( ).
 *
 * We only include F in dist1*dist2*F (for match states) if we are at_root.
 *
 * When we divide out by the I and D probabilities, we multiply by F only if the root
 * frequencies are NOT included.
 *
 * When merging columns in get_column_likelihoods( ), we apply root frequecies whenever the
 * branch leading to the root is a "-".  If the root isn't behind either of the branches, then
 * we don't apply any frequencies.
 *
 * We need to copy the away_from_root_frequencies in the shift( ) function.
 *
 * We need to handle the case where n2 has observed sequences in sample_alignment.
 * So perhaps we should allow passing in node_CLVs.
 * This would remove the special case for the tree having 2 nodes.
 * 
 * But if there is an observed sequence at n0 in sample_tri, then we need to complain.
 */


shared_ptr<DPmatrixSimple> sample_alignment_forward(data_partition P, const TreeInterface& t, const indel::PairHMM& hmm, int b, optional<int> bandwidth)
{
    assert(P.variable_alignment());

    int bb = t.reverse(b);

    int n2 = t.target(b);

    auto F = P.WeightedFrequencyMatrix(n2);

    auto dists1 = substitution::shift(*P.cache(b), 2);

    EVector LCN;
    if (auto lcn = P.get_node_CLV(n2))
    {
        if (auto sparse = lcn.to<SparseLikelihoods>())
            LCN.push_back(sparse->DenseLikelihoods());
        else
            LCN.push_back(lcn);
    }

    EVector LCB;
    EVector A;
    for(int b: t.branches_before(bb))
    {
	LCB.push_back(P.cache(b));
	A.push_back(P.get_pairwise_alignment_(b));
    }

    // Unlike with sample-tri, the order of characters at n2 is determined by the pairwise alignments.
    auto dists2 = substitution::shift(*substitution::merge_branches(LCN, LCB, A), 2);
    
    vector<HMM::bitmask_t> state_emit(4,0);
    state_emit[0] |= (1<<1)|(1<<0);
    state_emit[1] |= (1<<1);
    state_emit[2] |= (1<<0);
    state_emit[3] |= 0;

    // This includes the 2 columns of padding that we asked for above.
    MatrixSize matrix_size{dists1.n_columns(), dists2.n_columns()};

    //-------------- Compute ymin and ymax for each x --------------//
    auto yboundaries = yboundaries_everything(dists1.n_columns()-2, dists2.n_columns()-2);

    if (bandwidth)
        yboundaries = yboundaries_simple_band(dists1.n_columns()-2, dists2.n_columns()-2, *bandwidth);
    MatrixShape matrix_shape(matrix_size, std::move(yboundaries));

    //------------------ Compute the DP matrix ---------------------//
    auto Matrices = std::make_shared<DPmatrixSimple>
                    (
                         std::move(matrix_shape),
                         HMM(state_emit, hmm.start_pi(), hmm, P.get_beta()),
                         std::move(dists1),
                         std::move(dists2),
                         *F
                    );

    Matrices->forward_band();

    return Matrices;
}


pair<shared_ptr<DPmatrixSimple>,log_double_t> sample_alignment_base(mutable_data_partition P, const indel::PairHMM& hmm, int b, optional<int> bandwidth) 
{
    auto Matrices = sample_alignment_forward(P, P.t(), hmm, b, bandwidth);

    auto path = Matrices->sample_path();
    if (not path)
    {
	if (log_verbose > 0) std::cerr<<"sample_alignment_base( ): path probabilities sum to "<<Matrices->Pr_sum_all_paths()<<"!"<<std::endl;
	return {Matrices, 0};
    }

    P.set_pairwise_alignment(b, A2::get_pairwise_alignment_from_path(*path));

    return {Matrices, Matrices->Pr_sum_all_paths() / Matrices->path_Q(*path)};
}

pair<shared_ptr<DPmatrixSimple>,log_double_t> sample_alignment_base(mutable_data_partition P, int b, optional<int> bandwidth)
{
    return sample_alignment_base(P, P.get_branch_HMM(b), b, bandwidth);
}

log_double_t sample_alignment(Parameters& P, int b, bool initial_state_valid)
{
    if (log_verbose >= 3)
        std::cerr<<"[sample_alignment]\n";

    //  if (any_branches_constrained(vector<int>(1,b), P.t(), P.PC->TC, P.PC->AC))
    //    return;

    P.select_root(b);

    auto t = P.t();

    if (t.is_leaf_node(t.target(b)))
	b = t.reverse(b);
  
    optional<int> bandwidth;
    if (setting_exists("simple_bandwidth"))
        bandwidth  = get_setting("simple_bandwidth").as_int64();

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

    // OS and OP never used!!

    //--------- Compute path probabilities and sampling probabilities ---------//
    if (initial_state_valid)
    {
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
    }
#endif

    P = p[0];

    // ensure that the program is executed.
    P.evaluate_program();

    return total_ratio;
}

