/*
  Copyright (C) 2004-2007,2009-2012,2017 Benjamin Redelings

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

#include <algorithm>                                // for sort
#include <boost/dynamic_bitset/dynamic_bitset.hpp>  // for dynamic_bitset
#include <iostream>                                 // for operator<<, endl
#include <memory>                                   // for shared_ptr, __sha...
#include <new>                                      // for bad_alloc
#include <utility>                                  // for move, pair
#include <vector>                                   // for vector
#include "computation/object.H"                     // for intrusive_ptr_rel...
#include "dp/2way.H"                                // for convert_to_bits
#include "dp/3way.H"                                // for get_nodes_branch_...
#include "dp/alignment-sums.H"                      // for sample_cube_multi...
#include "dp/dp-cube.H"                             // for DPcubeSimple
#include "dp/hmm.H"                                 // for HMM, HMM::bitmask_t
#include "models/TreeInterface.H"                   // for TreeInterface
#include "models/parameters.H"                      // for Parameters, mutab...
#include "probability/choose.H"                     // for choose_MH_P
#include "substitution/likelihood.H"                // for get_column_likeli...
#include "util/assert.hh"                           // for assert
#include "util/math/log-double.H"                   // for log_double_t, ope...
#include "util/settings.H"                          // for get_setting_or( )
#include "util/log-level.H"                         // for log_verbose
#include "util/rng.H"                               // for uniform
class DPengine;

//Assumptions:
//  a) we assume that the internal node is the parent sequence in each of the sub-alignments

using std::abs;
using std::vector;
using std::optional;
using std::pair;
using std::endl;
using std::shared_ptr;
using boost::dynamic_bitset;

pair<shared_ptr<DPcubeSimple>,log_double_t>
cube_sample_alignment_base(mutable_data_partition P, const data_partition& P0, 
                           const vector<int>& nodes, const vector<int>& nodes0,
                           optional<int> /*bandwidth*/)
{
    const auto t = P.t();
    const auto t0 = P0.t();
  
    assert(P.variable_alignment());

    assert(t.is_connected(nodes[0],nodes[1]));
    assert(t.is_connected(nodes[0],nodes[2]));
    assert(t.is_connected(nodes[0],nodes[3]));

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

    vector<HMM::bitmask_t> a23;

    HMM::bitmask_t m23; m23.set(1); m23.set(2);

    if (tree_changed)
    {
	int b4 = t0.find_branch(nodes[2],nodes[3]);
	// Does this give the right order so that the move is reversible?
	// FIXME: Check that when we project a123_new to a12_new at the end, this does not change!!!
	a23 = convert_to_bits(P0.get_pairwise_alignment(b4),1,2);

	// The branch that the subtree was pruned from.
//	int b5 = t.find_branch(nodes0[2],nodes0[3]);
	assert(t0.is_connected(nodes0[2],nodes0[0]));
	assert(t0.is_connected(nodes0[3],nodes0[0]));

	// Make sure the column order on the pruned branch matches the projected column order from the original alignment.
	//    vector<HMM::bitmask_t> b123 = A3::get_bitpath(P0, nodes0);
	//    P.set_pairwise_alignment(b5, get_pairwise_alignment_from_bits(b123,1,2));
    }
    else
    {
	vector<HMM::bitmask_t> a123 = A3::get_bitpath(P, nodes);
	a23 = remove_silent(a123, m23);
    }

    auto F = P.WeightedFrequencyMatrix(nodes[0]);
    auto dists1 = substitution::shift(*P.cache(b1), 2);
    auto dists2 = substitution::shift(*P.cache(b2), 2);
    auto dists3 = substitution::shift(*P.cache(b3), 2);

    //-------------- Create alignment matrices ---------------//

    vector<int> branches(3);
    for(int i=0;i<3;i++)
	branches[i] = t.find_branch(nodes[0],nodes[i+1]);

    shared_ptr<DPcubeSimple>
	Matrices(new DPcubeSimple(m123, std::move(dists1), std::move(dists2), std::move(dists3), *F));
    Matrices->forward_cube();

    if (Matrices->Pr_sum_all_paths() <= 0.0) 
	std::cerr<<"Constraints give this choice probability 0"<<std::endl;

    auto path_g = Matrices->sample_path();
    if (not path_g)
    {
	if (log_verbose > 0) std::cerr<<"sample_node_base( ): path probabilities sum to "<<Matrices->Pr_sum_all_paths()<<"!"<<std::endl;

#ifndef NDEBUG_DP
	Matrices->clear();
#endif
	return {Matrices, 0};
    }

    vector<int> path = Matrices->ungeneralize(*path_g);

    for(int i=0;i<3;i++) {
	int b = t.find_branch(nodes[0],nodes[i+1]);
	P.set_pairwise_alignment(b, get_pairwise_alignment_from_path(path, *Matrices, 3, i));
    }

#ifdef NDEBUG_DP
    Matrices->clear();
#endif

    // What is the probability that we choose the specific alignment that we did?
    auto sampling_pr = Matrices->path_P(*path_g)* Matrices->generalize_P(path);

    return {Matrices,sampling_pr};
}

pair<shared_ptr<DPengine>,log_double_t> sample_cube_multi_calculation::compute_matrix(int i, int j)
{
    return cube_sample_alignment_base(p[i][j], p[0][j], nodes[i], nodes[0], bandwidth);
}

sample_cube_multi_calculation::sample_cube_multi_calculation(vector<Parameters>& pp,const vector< vector<int> >& nodes_,
    optional<int> bw)
    :sample_A3_multi_calculation(pp, nodes_, bw)
{ }

// Consider making into object! That would make it easier to mix
// and match parts of the routine, while saving state.

int sample_cube_multi(vector<Parameters>& p,const vector< vector<int> >& nodes,
		     const vector<log_double_t>& rho) 
{
    optional<int> bandwidth;
    if (setting_exists("simple_bandwidth"))
        bandwidth  = get_setting("simple_bandwidth").as_int64();

    try {
	sample_cube_multi_calculation tri(p, nodes, bandwidth);

	// The DP matrix construction didn't work.
	if (tri.Pr[0] <= 0.0) return -1;

	tri.set_proposal_probabilities(rho);

	return tri.choose();
    }
    catch (std::bad_alloc&) {
	std::cerr<<"Allocation failed in sample_cube_multi!  Proceeding."<<std::endl;
	return -1;
    }
}

int sample_cube_multi(vector<Parameters>& p,const vector< vector<int> >& nodes,
		     const vector<log_double_t>& rho, optional<int> bandwidth)
{
    try {
	vector<Parameters> p2 = p;

	//----------------- Part 1: Forward -----------------//
	sample_cube_multi_calculation tri1(p, nodes, bandwidth);

	// The DP matrix construction didn't work.
	if (tri1.Pr[0] <= 0.0) return -1;

	tri1.set_proposal_probabilities(rho);

	int C1 = tri1.choose(false);
	assert(C1 != -1);

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

	sample_cube_multi_calculation tri2(p2, nodes, bandwidth);

	// The DP matrix construction didn't work.
	if (tri2.Pr[0] <= 0.0) return -1;

	tri2.set_proposal_probabilities(rho);

	log_double_t ratio = tri1.Pr[C1]*choose_MH_P(0,C1,tri1.Pr)/(tri2.Pr[0]*choose_MH_P(C1,0,tri2.Pr));

	ratio *= tri1.C1 / tri2.C1;

	if (uniform() < double(ratio))
	    return C1;
	else
	    return -1;
    }
    catch (std::bad_alloc&) {
	std::cerr<<"Allocation failed in sample_cube_multi!  Proceeding."<<std::endl;
	return -1;
    }
}


void cube_sample_alignment(Parameters& P,int node1,int node2) 
{
    int bandwidth = get_setting_or("bandwidth", -1);

    P.set_root(node1);

    //------------(Gibbs) sample from proposal distribution ------------------//
    vector<Parameters> p(1,P);

    vector< vector<int> > nodes(1);
    nodes[0] = A3::get_nodes_branch_random(P.t(),node1,node2);

    vector<log_double_t> rho(1,1);

    int C = -1;
    if (bandwidth >= 0)
	C = sample_cube_multi(p,nodes,rho, bandwidth);
    else
	C = sample_cube_multi(p,nodes,rho);

    if (C != -1) {
	P = p[C];
    }
}

/// Resample branch alignment, internal nodes, and branch length

/// Assumptions:
///  We assume that the probability of the other branch alignments is unaffected...

bool cube_sample_alignment_branch(Parameters& P,
				 int node1,int node2,int b,
				 double rho_,double length2)
{
    //----------- Generate the Different Matrices ---------//
    vector<Parameters> p(2,P);
    p[1].setlength(b,length2);

    vector< vector<int> > nodes (2, A3::get_nodes_branch_random(P.t(),node1,node2) );

    vector<log_double_t> rho(2);
    rho[0] = 1;
    rho[1] = rho_;

    int C = sample_cube_multi(p,nodes,rho);

    if (C != -1) {
	P = p[C];
    }

    return (C > 0);
}

bool cube_sample_alignment_and_parameter(Parameters& P, int node1,int node2, const Proposal& propose)
{
    //----------- Generate the Different Matrices ---------//
    vector<Parameters> p(2,P);

    vector< vector<int> > nodes (2, A3::get_nodes_branch_random(P.t(),node1,node2) );

    auto rho = propose(p[1]);

    int C = sample_cube_multi(p, nodes, {1.0, rho});

    if (C != -1) {
	P = p[C];
    }

    return (C > 0);
}


bool cube_sample_alignment_branch_model(Parameters& P,int node1,int node2)
{
    //----------- Generate the Different Matrices ---------//
    vector<Parameters> p(2,P);

    //  int b = P.t().find_branch(node1,node2);
    // need to make this into a parameters if we are sampling it
    //  p[1].branch_HMM_type[b] = 1 - p[1].branch_HMM_type[b];

    vector< vector<int> > nodes (2, A3::get_nodes_branch_random(P.t(), node1,node2) );

    vector<log_double_t> rho(2,1.0);

    int C = sample_cube_multi(p,nodes,rho);

    if (C != -1) {
	P = p[C];
    }

    return (C > 0);
}
