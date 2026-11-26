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

#include <algorithm>                                // for sort, max
#include "util/bitmask.hh"                           // for bitmask
#include <boost/dynamic_bitset/dynamic_bitset.hpp>  // for dynamic_bitset
#include <iostream>                                 // for operator<<, endl
#include <memory>                                   // for shared_ptr, __sha...
#include <new>                                      // for bad_alloc
#include <optional>                                 // for optional
#include <string>                                   // for operator+, string
#include <utility>                                  // for pair, move
#include <vector>                                   // for vector, vector::s...
#include "alignment/alignment-constraint.hh"         // for boundaries_inters...
#include "computation/object.hh"                     // for intrusive_ptr_rel...
#include "dp/1way.hh"                                // for convert_to_bits
#include "dp/3way.hh"                                // for get_nodes_branch_...
#include "dp/alignment-sums.hh"                      // for sample_star_multi_calculation
#include "dp/dp-engine.hh"                           // for DPengine
#include "dp/dp-matrix.hh"                           // for DPmatrixConstrained
#include "dp/hmm.hh"                                 // for HMM::bitmask_t, HMM
#include "models/TreeInterface.hh"                   // for TreeInterface
#include "models/parameters.hh"                      // for Parameters, mutab...
#include "probability/choose.hh"                     // for choose_MH_P, choo...
#include "substitution/likelihood.hh"                // for get_column_likeli...
#include "util/assert.hh"                           // for assert
#include "util/math/log-double.hh"                   // for log_double_t, ope...
#include "util/log-level.hh"                         // for log_verbose
#include "util/myexception.hh"                       // for myexception
#include "util/settings.hh"                          // for get_setting_or( )
#include "util/rng.hh"                               // for uniform

//Assumptions:
//  a) we assume that the internal node is the parent sequence in each of the sub-alignments

using std::abs;
using std::vector;
using std::pair;
using std::endl;
using std::optional;
using std::shared_ptr;
using boost::dynamic_bitset;

pair<shared_ptr<DPmatrixConstrained>,Availability<log_double_t>>
tri_sample_alignment_base(mutable_data_partition P, const vector<int>& nodes, const vector<HMM::bitmask_t>& a23,
			  optional<int> bandwidth)
{

    const auto t = P.t();
  
    assert(P.variable_alignment());

    assert(t.is_connected(nodes[0],nodes[1]));
    assert(t.is_connected(nodes[0],nodes[2]));
    assert(t.is_connected(nodes[0],nodes[3]));

    // std::cerr<<"A = "<<A<<endl;

    int b1 = t.find_branch(nodes[1],nodes[0]);
    int b2 = t.find_branch(nodes[2],nodes[0]);
    int b3 = t.find_branch(nodes[3],nodes[0]);

    HMM m1 = P.get_branch_HMM(b1);
    m1.remap_bits({0,3});
    HMM m2 = P.get_branch_HMM(t.reverse(b2));
    m2.remap_bits({3,1});
    HMM m3 = P.get_branch_HMM(t.reverse(b3));
    m3.remap_bits({3,2});

    HMM m123 = Glue(m1,Glue(m2,m3));
    m123.hidden_bits.set(3);
    m123.B = P.get_beta();

    /*---------- Compute sequence properties -----------*/
    HMM::bitmask_t m23; m23.set(1); m23.set(2);

    auto F = P.WeightedFrequencyMatrix(nodes[0]);
    auto dists1 = substitution::shift(*P.cache(b1), 2);
    auto dists2 = P.cache(b2);
    auto dists3 = P.cache(b3);
    auto dists23 = substitution::get_column_likelihoods({dists2, dists3}, get_indices_from_bitpath_w(a23, {1,2}, m23), *F, 2);

    //-------------- Create matrix shape -----------------//

    auto yboundaries = yboundaries_everything(dists1.n_columns()-2, dists23.n_columns()-2);

    if (bandwidth)
        yboundaries = yboundaries_simple_band(dists1.n_columns()-2, dists23.n_columns()-2, *bandwidth);

    // This includes the 2 columns of padding that we asked for above.
    MatrixSize matrix_size{dists1.n_columns(), dists23.n_columns()};

    MatrixShape matrix_shape(matrix_size, std::move(yboundaries));

    //-------------- Create alignment matrices ---------------//
    auto Matrices = std::make_shared<DPmatrixConstrained>
                    (
                        std::move(matrix_shape),
                        m123,
                        std::move(dists1),
                        std::move(dists23),
                        *F
                    );
    Matrices->emit1 = 1;
    Matrices->emit2 = 2|4;

    // collect the silent-or-correct-emissions for each type columns
    vector< vector<int> > allowed_states_for_mask(4);
    for(auto& m: allowed_states_for_mask)
	m.reserve(Matrices->n_dp_states());

    // Construct the states that are allowed for each emission pattern.
    for(int S2: Matrices->dp_order())
    {
	auto mask = (m123.state_emit[S2] & Matrices->emit2).raw();

	// Hidden states never contradict an emission pattern.
	if (not mask) // m123.silent(S2))
	    for(int j=0;j<allowed_states_for_mask.size();j++)
		allowed_states_for_mask[j].push_back(S2);
	else
	{
	    mask >>= 1;
	    allowed_states_for_mask[mask].push_back(S2);
	}
    }

    Matrices->states(1) = Matrices->dp_order();

    // Determine which states are allowed to match (,c2)
    for(int c2=1;c2<Matrices->dists2.n_columns()-1;c2++) 
    {
	auto mask = (a23[c2-1]&Matrices->emit2).raw();
	mask >>= 1;
	assert(mask);

	Matrices->states(c2+1) = allowed_states_for_mask[mask];
    }

    //------------------ Compute the DP matrix ---------------------//
    Matrices->forward_band();

/*
    //  vector<vector<int> > pins = get_pins(P.alignment_constraint,A,group1,group2 | group3,seq1,seq23);
    vector<vector<int> > pins(2);

    //  Note: we don't even HAVE an a123 unless tree_changed == false!
    //  vector< pair<int,int> > yboundaries = get_y_ranges_for_band(bandwidth, seq23, seq1, seq123);
    //  vector<pair<int,int>> yboundaries(seq1.size()+1,pair<int,int>(0,seq23.size()));
  
    // if the constraints are currently met but cannot be met
    if (pins.size() == 1 and pins[0][0] == -1)
	; //std::cerr<<"Constraints cannot be expressed in terms of DP matrix paths!"<<std::endl;
    else 
    {
	const int I = a1+1;
	const int J = a23.size()+1;
	yboundaries = boundaries_intersection(yboundaries, get_yboundaries_from_pins(I, J, pins));

	Matrices->forward_band(yboundaries);
	if (Matrices->Pr_sum_all_paths() <= 0.0) 
	    std::cerr<<"Constraints give this choice probability 0"<<std::endl;
    }
*/

    auto path_g = Matrices->sample_path();
    if (not path_g)
    {
	if (log_verbose > 0) std::cerr<<"tri_sample_alignment_base( ): path probabilities sum to "<<Matrices->Pr_sum_all_paths()<<"!"<<std::endl;
	return {Matrices, unavailable};
    }

    vector<int> path = Matrices->ungeneralize(*path_g);

    // This is the probability of choosing the concrete path after generalization.
    auto sampling_pr = Matrices->path_P(*path_g) * Matrices->generalize_P(path);
    if (not std::isfinite(sampling_pr.log()))
    {
        if (log_verbose > 0)
            std::cerr<<"tri_sample_alignment_base( ): sampling probability is "<<sampling_pr<<"!"<<std::endl;
        return {Matrices, unavailable};
    }

    for(int i=0;i<3;i++) {
	int b = t.find_branch(nodes[0],nodes[i+1]);
	P.set_pairwise_alignment(b, get_pairwise_alignment_from_path(path, *Matrices, 3, i));
    }

    return {Matrices, available(sampling_pr)};
}

// If there is an original 3way alignment, then we need to construct a 3way path and project to 2way
// in order for the constraint to be consistent with the original 3-way path.
//
// (In other cases, there maybe not be any original path to be consistent with.)
vector<HMM::bitmask_t> A23_constraint(data_partition P, const vector<int>& nodes, bool consistent_with_original)
{
    if (consistent_with_original)
    {
	HMM::bitmask_t m23; m23.set(1); m23.set(2);
	vector<HMM::bitmask_t> a123 = A3::get_bitpath(P, nodes);
	return remove_silent(a123, m23);
    }
    else
    {
	int b4 = P.t().find_branch(nodes[2],nodes[3]);
	return convert_to_bits(P.get_pairwise_alignment(b4),1,2);
    }
}

vector<optional<vector<HMM::bitmask_t>>> A23_constraints(const Parameters& P, const vector<int>& nodes, bool consistent_with_original)
{
    vector<optional<vector<HMM::bitmask_t>>> a23(P.n_data_partitions());
    for(int i=0; i<P.n_data_partitions();i++)
	if (P[i].variable_alignment())
	    a23[i] = A23_constraint(P[i], nodes, consistent_with_original);
    return a23;
}

pair<shared_ptr<DPmatrixConstrained>,Availability<log_double_t>>
tri_sample_alignment_base(mutable_data_partition P, const data_partition& P0,
			  const vector<int>& nodes, const vector<int>& nodes0,
			  optional<int> bandwidth)
{
    const auto t0 = P0.t();

    assert(t0.is_connected(nodes[0],nodes[1]));
    assert(nodes0[0] == nodes[0]);
    bool tree_changed = not t0.is_connected(nodes[0],nodes[2]) or not t0.is_connected(nodes[0],nodes[3]);

    // If the tree changed, assert that previously nodes 2 and 3 were connected.
    if (tree_changed)
    {
	assert(t0.is_connected(nodes[2],nodes[3]));
    }
    else
    {
	assert(t0.is_connected(nodes[0],nodes[2]));
	assert(t0.is_connected(nodes[0],nodes[3]));
    }

    vector<HMM::bitmask_t> a23;

    if (tree_changed)
    {
	assert(P.t().is_connected(nodes0[2], nodes0[3]));  // The old attachment point should not be split in the new      tree
	assert(t0.is_connected(nodes0[2],nodes0[0]));  // The old attachment point should     be split in the original tree
	assert(t0.is_connected(nodes0[3],nodes0[0]));

	a23 = A23_constraint(P0, nodes, false);
    }
    else
	a23 = A23_constraint(P, nodes, true);

    return tri_sample_alignment_base(P, nodes, a23, bandwidth);
}

Availability<log_double_t> tri_sample_alignment_ratio(mutable_data_partition P, int node1, int node2,
                                                      optional<int> bandwidth = {})
{
    auto nodes = A3::get_nodes_branch_random(P.t(),node1,node2);
    auto a23 = A23_constraint(P, nodes, true);

    auto bitpath0 = A3::get_bitpath(P,nodes);

    auto [M, forward_sample_pr] = tri_sample_alignment_base(P, nodes, a23, bandwidth);

    if (not forward_sample_pr)
        return unavailable;

    auto path0 = get_path_unique(bitpath0, *M);
    auto path0_g = M->generalize(path0);
    auto reverse_sample_pr = M->path_P(path0_g) * M->generalize_P(path0);

    auto ratio = reverse_sample_pr / *forward_sample_pr;
    if (std::isfinite(ratio.log()))
        return available(ratio);
    return unavailable;
}

Availability<log_double_t> tri_sample_alignment_ratio(Parameters& P, int node1, int node2, optional<int> bandwidth)
{
    auto ratio = available(log_double_t(1));
    for(int j=0;j<P.n_data_partitions();j++)
    {
	if (P[j].has_pairwise_alignments())
	{
	    if (not P[j].alignment_is_random())
		throw myexception()<<"Partition "<<j+1<<": can't change the tree topology because the tree-alignment is fixed!\n  Consider adding --imodel=none or --fix=tree or removing --fix=alignment.";

	    auto partition_ratio = tri_sample_alignment_ratio(P[j], node1, node2, bandwidth);
	    if (not partition_ratio)
		return unavailable;
	    ratio *= partition_ratio;
	}
    }

    if (not std::isfinite(ratio->log()))
        return unavailable;
    return ratio;
}


Availability<log_double_t> pr_sum_out_A_tri(Parameters& P,
                                            const vector<optional<vector<HMM::bitmask_t>>>& a23,
                                            const vector<int>& nodes)
{
    log_double_t Pr = 1.0;

    // sum of substitution and alignment probability over all paths
    for(int j=0;j<P.n_data_partitions();j++)
    {
	if (P[j].variable_alignment())
	{
            // This computes the sampling probability of the CHOSEN path, not the INPUT path!
	    auto [M, sampling_pr] = tri_sample_alignment_base(P[j], nodes, *a23[j], {});
            if (not sampling_pr)
            {
                std::cerr<<"pr_sum_out_A_tri: sampling is unavailable for partition "<<j<<"\n";
                return unavailable;
            }

	    Pr *= *sampling_pr;
            Pr /= node_length_correction(P[j], nodes[0]);
            // FIXME! These sums still need to be accepted/rejected!
        }
    }

    if (not std::isfinite(Pr.log()))
        return unavailable;
    return available(Pr);
}

pair<shared_ptr<DPengine>,Availability<log_double_t>> sample_tri_multi_calculation::compute_matrix(int i, int j)
{
    return tri_sample_alignment_base(p[i][j], p[0][j], nodes[i], nodes[0], bandwidth);
}

sample_tri_multi_calculation::sample_tri_multi_calculation(vector<Parameters>& pp,const vector< vector<int> >& nodes_,
							   optional<int> b)
    :sample_star_multi_calculation(pp, nodes_, b)
{ }

// Sample on the two-dimensional grid, preserving the ordered alignment of neighbors 2 and 3.
optional<int> sample_tri_2D_multi(vector<Parameters>& p, const vector<vector<int>>& nodes,
                                  const vector<log_double_t>& rho, optional<int> bandwidth)
{
    if (not bandwidth and setting_exists("simple_bandwidth"))
        bandwidth = get_setting("simple_bandwidth").as_int64();

    try {
        sample_tri_multi_calculation tri(p, nodes, bandwidth);
        tri.run_dp();
        if (not tri.Pr[0]) return {};
        tri.set_proposal_probabilities(rho);
        return tri.choose();
    }
    catch (std::bad_alloc& e) {
        std::cerr<<"Allocation failed in sample_tri_2D_multi: "<<e.what()<<"!  Proceeding."<<std::endl;
        return {};
    }
}

// Other star moves can also select the full cube using the configured mixture.
optional<int> sample_tri_multi(vector<Parameters>& p, const vector<vector<int>>& nodes,
                               const vector<log_double_t>& rho)
{
    if (uniform() < get_setting_or("cube_fraction",0.0))
        return sample_cube_multi(p, nodes, rho);

    return sample_tri_2D_multi(p, nodes, rho, {});
}

/*
 * NOTE: This version of the bandwidth uses a bandwidth around the CURRENT PATH,
 *       instead of the around the DIAGONAL.
 *       It is broken, but maintains some logic for reverse proposals, since we
 *       need to find the probability of proposing the original path given the
 *       bandwidth around the old path.
 */

optional<int> sample_tri_multi(vector<Parameters>& p,const vector< vector<int> >& nodes,
		               const vector<log_double_t>& rho, int bandwidth)
{
    assert(bandwidth >= 0);
    try {
	vector<Parameters> p2 = p;

	//----------------- Part 1: Forward -----------------//
	sample_tri_multi_calculation tri1(p, nodes, bandwidth);
	tri1.run_dp();

	// The DP matrix construction didn't work.
	if (not tri1.Pr[0]) return {};

	tri1.set_proposal_probabilities(rho);

	auto choice1 = tri1.choose(false);
	if (not choice1) return {};
	int C1 = *choice1;

	//----------------- Part 2: Backward -----------------//

	// Set the initial alignment, so that our bandwidth is relative to this one.
	//
	// This just selects another alignment w/in the range of possible alignments, so the previous
	//   cache invalidations should work for this alignment as well.
	//

	// FIXME: direct access to A_?
	//    for(int i=0;i<p2.size();i++)
	//      for(int j=0;j<p2[i].n_data_partitions();j++)
	//	p2[i][j].A_ = p[C1][j].A_;
	std::abort();

	sample_tri_multi_calculation tri2(p2, nodes, bandwidth);
	tri2.run_dp();

	// The DP matrix construction didn't work.
	if (not tri2.Pr[0]) return {};

	tri2.set_proposal_probabilities(rho);

    auto forward_choice = choose_MH_P(0, C1, tri1.Pr);
    auto reverse_choice = choose_MH_P(C1, 0, tri2.Pr);
    if (not tri1.Pr[C1] or not tri2.Pr[0] or not forward_choice or not reverse_choice)
        return {};
    log_double_t ratio = (*tri1.Pr[C1] / *tri2.Pr[0]) * *forward_choice / *reverse_choice;

	ratio *= tri1.C1 / tri2.C1;

	if (uniform() < double(ratio))
	    return C1;
	else
	    return {};
    }
    catch (std::bad_alloc& e) {
	std::cerr<<"Allocation failed in sample_tri_multi: "<<e.what()<<"!  Proceeding."<<std::endl;
	return {};
    }
}


void tri_sample_alignment(Parameters& P,int node1,int node2) 
{
    P.set_root(node1);

    {
        //------------(Gibbs) sample from proposal distribution ------------------//
        vector<Parameters> p(1,P);

        vector< vector<int> > nodes(1);
        nodes[0] = A3::get_nodes_branch_random(P.t(),node1,node2);

        vector<log_double_t> rho(1,1);

        auto choice = sample_tri_multi(p,nodes,rho);

        if (choice)
            P = p[*choice];

        // If there is no choice, then p[0] may still hold new alignments for the partitions
        // that were handled before the DP matrix of a later partition failed.  Destroy the
        // proposals here: P cannot be executed while a modified copy of it exists.
    }

    // Ensure that the program is executed.
    P.evaluate_program();
}

/// Resample branch alignment, internal nodes, and branch length

/// Assumptions:
///  We assume that the probability of the other branch alignments is unaffected...

bool tri_sample_alignment_branch(Parameters& P,
				 int node1,int node2,int b,
				 double rho_,double length_or_duration2)
{
    //----------- Generate the Different Matrices ---------//
    vector<Parameters> p(2,P);
    p[1].t().set_branch_length_or_duration(b,length_or_duration2);

    vector< vector<int> > nodes (2, A3::get_nodes_branch_random(P.t(),node1,node2) );

    vector<log_double_t> rho(2);
    rho[0] = 1;
    rho[1] = rho_;

    auto choice = sample_tri_multi(p,nodes,rho);

    if (choice)
	P = p[*choice];

    return choice and *choice > 0;
}

bool tri_sample_alignment_and_parameter(Parameters& P, int node1,int node2, const Proposal& propose)
{
    //----------- Generate the Different Matrices ---------//
    vector<Parameters> p(2,P);

    vector< vector<int> > nodes (2, A3::get_nodes_branch_random(P.t(),node1,node2) );

    auto rho = propose(p[1]);

    auto choice = sample_tri_multi(p, nodes, {1.0, rho});

    if (choice)
	P = p[*choice];

    return choice and *choice > 0;
}


bool tri_sample_alignment_branch_model(Parameters& P,int node1,int node2)
{
    //----------- Generate the Different Matrices ---------//
    vector<Parameters> p(2,P);

    //  int b = P.t().find_branch(node1,node2);
    // need to make this into a parameters if we are sampling it
    //  p[1].branch_HMM_type[b] = 1 - p[1].branch_HMM_type[b];

    vector< vector<int> > nodes (2, A3::get_nodes_branch_random(P.t(), node1,node2) );

    vector<log_double_t> rho(2,1.0);

    auto choice = sample_tri_multi(p,nodes,rho);

    if (choice)
	P = p[*choice];

    return choice and *choice > 0;
}
