/*
  Copyright (C) 2024 Benjamin Redelings

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
/// \file sample-A5-2D.C
///
/// \brief Contains routines for resampling the sequence at two adjacent internal nodes (5way).
///

#include "substitution/likelihood.H"                // for get_column_likeli...
#include <iostream>
#include <cmath>
#include "util/assert.hh"
#include "sample.H"
#include "probability/choose.H"
#include "util/mapping.H"
#include "util/rng.H"
#include "util/log-level.H"
#include "dp/2way.H"
#include "dp/5way.H"
#include "dp/hmm.H"                                 // for HMM::bitmask_t, HMM
#include "dp/alignment-sums.H"
#include "dp/dp-engine.H"                           // for DPengine
#include "dp/dp-matrix.H"                           // for DPmatrixConstrained
#include "alignment/alignment-util.H"
#include "alignment/alignment-util2.H"
#include "alignment/alignment-constraint.H"

using std::vector;
using std::optional;
using std::abs;
using std::endl;
using std::shared_ptr;
using std::pair;

using boost::dynamic_bitset;

pair<shared_ptr<DPmatrixConstrained>, log_double_t>
sample_A5_2D_base(mutable_data_partition P, const vector<HMM::bitmask_t>& a123456, const A5::hmm_order& order, const A5::hmm_order& order0, optional<int> bandwidth)
{
    assert(P.variable_alignment());
    HMM m12345 = A5::get_HMM(P,order);

    /*------- Get column order from (A0,T0,nodes) --------*/

    /*  OK, so, what does it mean to get the column order from the first alignment?
        I guess it means that we used (A0,T0,nodes0) to get an order.  We then keep that order
        for use with (A[i],T[i],nodes[i]).  */

    auto a123456_remapped = remap_bitpath(a123456, compute_mapping(order0.nodes, order.nodes));

    HMM::bitmask_t bits234;
    bits234.set(2);
    bits234.set(3);
    bits234.set(1);

    // Only keep columns with bits234 set, and also clear other bits.
    vector<HMM::bitmask_t> a234 = remove_silent(a123456_remapped, bits234);
    // Then reset bits 4 and 5
    for(auto& mask: a234)
    {
	// 5 = 2 | 4
	if (mask.test(2) or mask.test(3))
	    mask.set(5);
	// 4 = 1 | 2 | 3
	if (mask.test(1) or mask.test(5))
	    mask.set(4);
    }

    /*---------- Compute sequence properties -----------*/
    const auto t = P.t();
    auto& nodes = order.nodes;
    int b04 = t.find_branch(nodes[0],nodes[4]);
    int b14 = t.find_branch(nodes[1],nodes[4]);
    int b25 = t.find_branch(nodes[2],nodes[5]);
    int b35 = t.find_branch(nodes[3],nodes[5]);
    int b45 = t.find_branch(nodes[4],nodes[5]);
    int b54 = t.reverse(b45);

    auto F = P.WeightedFrequencyMatrix(nodes[0]);
    auto dists1 = substitution::shift(*P.cache(b04), 2);

    // We need to combine dists3+dists4 -> dists34, then combine dists2+dists34 -> dists234.
    // But we need to get the emission probabilities at node 4 WITHOUT relying on alignments on the internal branches.

    // Combining dists3 + dists4 -> dists34 needs to (a) propagate across branch b54 and (b) include root frequencies if the root is behind b54.
    P.set_pairwise_alignment(b25, get_pairwise_alignment_from_bits(a234, 2, 5));
    P.set_pairwise_alignment(b35, get_pairwise_alignment_from_bits(a234, 3, 5));
    auto dists34 = P.cache(b54);

    // Q: How do we specify that dists34 corresponds to both bits 2 and 3?
    // A: We set bit 5 when bit 2 or 3 are set.
    auto dists2 = P.cache(b14);
    auto dists234 = substitution::get_column_likelihoods({dists2, dists34}, get_indices_from_bitpath_w(a234, {1,5}, bits234), *F, 2);
    assert(dists234.n_columns() == a234.size()+2);

    /*------------- Create matrix shape ----------------*/
    auto yboundaries = yboundaries_everything(dists1.n_columns()-2, dists234.n_columns()-2);

    if (bandwidth)
        yboundaries = yboundaries_simple_band(dists1.n_columns()-2, dists234.n_columns()-2, *bandwidth);

    // This includes the 2 columns of padding that we asked for above.
    MatrixSize matrix_size{dists1.n_columns(), dists234.n_columns()};

    MatrixShape matrix_shape(matrix_size, std::move(yboundaries));

    /*-------------- Create DP matrices ---------------*/

    auto Matrices = std::make_shared<DPmatrixConstrained>(
        std::move(matrix_shape),
        m12345,
        std::move(dists1),
        std::move(dists234),
        *F
        );
    Matrices->emit1 = 1;
    Matrices->emit2 = bits234;

    // collect the silent-or-correct-emissions for each type columns
    vector< vector<int> > allowed_states_for_mask(8);
    for(auto& m: allowed_states_for_mask)
        m.reserve(Matrices->n_dp_states());

    for(int S2: Matrices->dp_order())
    {
        auto mask = (m12345.state_emit[S2] & Matrices->emit2).raw();

        // Hidden states never contradict an emission pattern.
        if (not mask)
            for(int j=0;j<8;j++)
                allowed_states_for_mask[j].push_back(S2);
        else
        {
            mask >>= 1; // remap bits 123 to bits 012
            allowed_states_for_mask[mask].push_back(S2);
        }
    }

    Matrices->states(1) = Matrices->dp_order();

    // Determine which states are allowed to match (c2)
    for(int c2=1;c2<Matrices->dists2.n_columns()-1;c2++)
    {
        auto mask=(a234[c2-1]&Matrices->emit2).raw();
        mask >>= 1;
        assert(mask);

        Matrices->states(c2+1) = allowed_states_for_mask[mask];
    }

    //------------------ Compute the DP matrix ---------------------//

    Matrices->forward_band();

    // If the DP matrix ended up having probability 0, don't try to sample a path through it!
    auto path_g = Matrices->sample_path();
    if (not path_g)
    {
	if (log_verbose > 0) std::cerr<<"sample_A5_2D_base( ): path probabilities sum to "<<Matrices->Pr_sum_all_paths()<<"!"<<std::endl;
	return {Matrices, 0};
    }

    //------------- Sample a path from the matrix -------------------//
    vector<int> path = Matrices->ungeneralize(*path_g);

    P.set_pairwise_alignment(b04, get_pairwise_alignment_from_path(path, *Matrices, 0, 4));
    P.set_pairwise_alignment(b14, get_pairwise_alignment_from_path(path, *Matrices, 1, 4));
    P.set_pairwise_alignment(b25, get_pairwise_alignment_from_path(path, *Matrices, 2, 5));
    P.set_pairwise_alignment(b35, get_pairwise_alignment_from_path(path, *Matrices, 3, 5));
    P.set_pairwise_alignment(b45, get_pairwise_alignment_from_path(path, *Matrices, 4, 5));

    // What is the probability that we choose the specific alignment that we did?
    auto sampling_pr = Matrices->path_P(*path_g)* Matrices->generalize_P(path);

    return {Matrices, sampling_pr};
}

struct IntegrationPrs
{
    log_double_t sampling = 1;
    log_double_t correction = 1;
    log_double_t sum_all_paths = 1;
    log_double_t heated_prob = 1;
    log_double_t proposal = 1;
};

///(a[0],p[0]) is the point from which the proposal originates, and must be valid.
vector<optional<IntegrationPrs>>
sample_A5_2D_multi2(vector<Parameters>& p,const vector<A5::hmm_order>& order_,
                    const vector<log_double_t>& rho_, optional<int> bandwidth)
{
    for(int i=1;i<p.size();i++)
        for(int j=0;j<p[0].n_data_partitions();j++)
            assert(p[0][j].variable_alignment() == p[i][j].variable_alignment());

    vector<A5::hmm_order> order = order_;
    vector<log_double_t> rho = rho_;
    assert(p.size() == order.size());

    vector<optional<vector<HMM::bitmask_t>>> a123456(p[0].n_data_partitions());

    for(int j=0;j<p[0].n_data_partitions();j++)
        if (p[0][j].has_pairwise_alignments())
        {
            if (not p[0][j].alignment_is_random())
                throw myexception()<<"Partition "<<j+1<<": can't change the tree topology because the tree-alignment is fixed!\n  Consider adding --imodel=none or --fix=tree or removing --fix=alignment.";

            a123456[j] = A5::get_bitpath(p[0][j], order[0]);
        }

    IntegrationPrs Pr0;
    Pr0.heated_prob = p[0].heated_probability();
    Pr0.correction = A5::correction(p[0],order[0]);
    Pr0.proposal = rho[0];

    vector<optional<IntegrationPrs>> Pr(p.size(), IntegrationPrs());

    //----------- Generate the different states and Matrices ---------//
    vector< vector< shared_ptr<DPmatrixConstrained> > > Matrices(p.size());
    for(int i=0;i<p.size();i++)
    {

#ifndef NDEBUG_DP
        Matrices[i].resize(p[i].n_data_partitions());
#endif

        for(int j=0;j<p[i].n_data_partitions();j++)
        {
            if (p[i][j].variable_alignment())
            {
                auto [M, sampling_pr] = sample_A5_2D_base(p[i][j], *a123456[j], order[i], order[0], bandwidth);

#ifndef NDEBUG_DP
                Matrices[i][j] = M;
#endif
                if (M->Pr_sum_all_paths() <= 0.0)
                {
                    if (log_verbose > 0) std::cerr<<"Pr = 0: option "<<i<<", partition "<<j<<" \n";
                    Pr[i] = {};

                    // Make sure to set all the Matrices[i][j] to something non-NULL.
                    continue;
                }

                if (Pr[i])
                {
                    Pr[i]->sampling *= sampling_pr;
                    Pr[i]->correction *= A5::correction(p[i][j], order[i]);
                    Pr[i]->sum_all_paths *= M->Pr_sum_all_paths();
                }

                if (i==0)
                {
                    auto path = get_path_unique(*a123456[j], *M);
                    auto path_g = M->generalize(path);
                    auto sampling_pr0 = M->path_P(path_g) * M->generalize_P(path);

                    Pr0.sampling *= sampling_pr0;
                    Pr0.sum_all_paths *= M->Pr_sum_all_paths();
                }

#ifndef NDEBUG_DP
                p[i][j].likelihood();  // check the likelihood calculation
#endif
            }
        }

        // Don't compute the probability if the alignment wasn't resampled!
        // Should we treat i=0 differently, since the old alignment is consistent?
        if (Pr[i])
        {
            Pr[i]->heated_prob = p[i].heated_probability();
            Pr[i]->proposal = rho[i];
        }
    }

    Pr.push_back(Pr0);

    return Pr;
}

optional<log_double_t> sample_A5_2D_ratio(vector<Parameters>& p, const vector<A5::hmm_order>& order,
					  const vector<log_double_t>& rho, optional<int> bandwidth)
{
    if (p.size() != 2)
        throw myexception()<<"sample_A5_2D_ratio only takes two Parameters objects!";

    auto Prs = sample_A5_2D_multi2(p, order, rho, bandwidth);

    if (Prs[0] and Prs[1])
    {
        auto sample_reverse = Prs[2]->sampling;
        auto sample_forward = Prs[1]->sampling;

        return (sample_reverse/sample_forward);
    }
    else
        return {};
}


///(a[0],p[0]) is the point from which the proposal originates, and must be valid.
int sample_A5_2D_multi(vector<Parameters>& p,const vector<A5::hmm_order>& order_,
                       const vector<log_double_t>& rho_, optional<int> bandwidth)
{
    for(int i=1;i<p.size();i++)
        for(int j=0;j<p[0].n_data_partitions();j++)
            assert(p[0][j].variable_alignment() == p[i][j].variable_alignment());

    vector<A5::hmm_order> order = order_;
    vector<log_double_t> rho = rho_;
    assert(p.size() == order.size());

    vector<optional<vector<HMM::bitmask_t>>> a123456(p[0].n_data_partitions());

    for(int j=0;j<p[0].n_data_partitions();j++)
        if (p[0][j].has_pairwise_alignments())
        {
            if (not p[0][j].alignment_is_random())
                throw myexception()<<"Partition "<<j+1<<": can't change the tree topology because the tree-alignment is fixed!\n  Consider adding --imodel=none or --fix=tree or removing --fix=alignment.";

            a123456[j] = A5::get_bitpath(p[0][j], order[0]);
        }

    vector<log_double_t> Pr(p.size());

    //----------- Generate the different states and Matrices ---------//
    log_double_t C1 = A5::correction(p[0],order[0]);
#if !defined(NDEBUG_DP) || !defined(NDEBUG)
    const Parameters P0 = p[0];
#endif

    vector< vector< shared_ptr<DPmatrixConstrained> > > Matrices(p.size());
    for(int i=0;i<p.size();i++)
    {
        Pr[i] = rho[i];

#ifndef NDEBUG_DP
        Matrices[i].resize(p[i].n_data_partitions());
#endif

        bool ok = true;
        for(int j=0;j<p[i].n_data_partitions();j++)
        {
            if (p[i][j].variable_alignment())
            {
                auto [M, sampling_pr] = sample_A5_2D_base(p[i][j], *a123456[j], order[i], order[0], bandwidth);

#ifndef NDEBUG_DP
                Matrices[i][j] = M;
#endif
                if (M->Pr_sum_all_paths() <= 0.0)
                {
                    if (log_verbose > 0) std::cerr<<"Pr = 0: option "<<i<<", partition "<<j<<" \n";
                    ok = false;

                    // Make sure to set all the Matrices[i][j] to something non-NULL.
                    continue;
                }

                Pr[i] /= sampling_pr;
                // Wait, why do we include the correction here?
                // We shouldn't need it to get the true distribution...
                Pr[i] *= A5::correction(p[i][j], order[i]);

#ifndef NDEBUG_DP
                p[i][j].likelihood();  // check the likelihood calculation
#endif
            }
        }

        // Don't compute the probability if the alignment wasn't resampled!
        // Should we treat i=0 differently, since the old alignment is consistent?
        if (ok)
            Pr[i] *= p[i].heated_probability();
        else
            Pr[i] = 0;
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
    if (log_verbose >= 4) std::cerr<<"choice = "<<C<<endl;

    // FIXME: check that alignment of sequences besides the middle 2 is the same between P0[j] and p[i][j]

    // Add another entry for the incoming configuration
    p.push_back( P0 );
    order.push_back(order[0]);
    rho.push_back( rho[0] );
    Matrices.push_back( Matrices[0] );

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

                auto OS = other_subst(p[i][j],order[i].nodes);
                auto OP = other_prior(p[i][j],order[i].nodes) / A5::correction(p[i][j],order[i]);

                check_match_P(p[i][j], OS, OP, paths[i][j], *Matrices[i][j]);
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
        PR[i][0] = p[i].heated_probability();
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


void sample_A5_2D(Parameters& P,int b, std::optional<int> bandwidth)
{
    vector<Parameters> p(1,P);

    vector< A5::hmm_order > order(1);
    order[0] = A5::get_nodes_random(P.t(), b);

    vector<log_double_t> rho(1,1);

    int C = sample_A5_2D_multi(p,order,rho, bandwidth);

    if (C != -1) {
        P = p[C];
    }
}
