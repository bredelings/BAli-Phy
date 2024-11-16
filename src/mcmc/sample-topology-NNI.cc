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
#include "util/assert.hh"
#include "util/log-level.H"
#include "util/settings.H"
#include "sample.H"
#include "probability/choose.H"
#include "util/rng.H"
#include "dp/4way.H"
#include "dp/5way.H"
#include "dp/alignment-sums.H"
#include "alignment/alignment-util.H"
#include "tree/tree-util.H"

#include "dp/3way.H"
#include "sample.H"

using MCMC::MoveStats;
using std::string;
using std::vector;
using std::optional;

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


/// Update statistics counters for an NNI move.
void NNI_inc(MoveStats& Stats, const string& name, MCMC::Result result,double L)
{
    Stats.inc(name, result);

    if (L < 0.0325)
	Stats.inc(name+"-0.0325", result);
    else if (L < 0.065)
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

// Do we need the different sample_A5_base routines to use the same
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
    vector< A5::hmm_order > orders(2);
    orders[0] = A5::get_nodes_random(p[0].t(), b);
    orders[1] = A5::get_nodes_random(p[1].t(), b);

    try {
	return sample_A5_multi(p,orders,rho);
    }
    catch (choose_exception<log_double_t>& c)
    {
	c.prepend(__PRETTY_FUNCTION__);
	throw c;
    }
}


#include "slice-sampling.H"

/*
 * BROKEN: Tries to integrate out the alignment length along the internal branch on TWO topologies.
           But never actually creates an internal node states for p[1].tree.
 */

void two_way_topology_slice_sample(owned_ptr<context>& P, MoveStats& Stats, int b) 
{
    std::abort();

    Parameters& PP = *P.as<Parameters>();
    if (PP.t().is_leaf_branch(b)) return;

    if (log_verbose >= 3) std::cerr<<"[two_way_topology_slice_sample]\n";

    A5::hmm_order order = A5::get_nodes_random(PP.t(), b);
    const auto& nodes = order.nodes;

    PP.select_root(b);
    PP.cache_likelihood_branches();

    int b1 = PP.t().find_branch(nodes[4],nodes[1]);
    int b2 = PP.t().find_branch(nodes[5],nodes[2]);

    vector<Parameters> p(2,PP);

    // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
    p[1].NNI_discard_alignment(b1, b2);
  
    //  if (not extends(p[1].t(), PP.PC->TC))
    //    return;

    double L = PP.t().branch_length(b);

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
    result.totals[2] = std::abs(PP.t().branch_length(b) - L);

    //  if (C == 1) std::cerr<<"slice-diff = "<<Pr2 - Pr1<<"\n";

    NNI_inc(Stats,"NNI (2-way,slice)", result, L);
}

void two_way_topology_sample(owned_ptr<context>& P, MoveStats& Stats, int b) 
{
    Parameters& PP = *P.as<Parameters>();
    if (PP.t().is_leaf_branch(b)) return;

    if (log_verbose >= 3) std::cerr<<"[two_way_topology_sample]\n";

    double slice_fraction = get_setting_or("NNI_slice_fraction",-0.25);

    if (not PP.variable_alignment() and uniform() < slice_fraction) {
	two_way_topology_slice_sample(P,Stats,b);
	return;
    }

    A5::hmm_order order = A5::get_nodes_random(PP.t(), b);
    const auto& nodes = order.nodes;

    PP.select_root(b);
    PP.cache_likelihood_branches();

    int b1 = PP.t().find_branch(nodes[4],nodes[1]);
    int b2 = PP.t().find_branch(nodes[5],nodes[2]);

    vector<Parameters> p(2,PP);

    // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
    p[1].NNI_discard_alignment(b1, b2);
  
    //  if (not extends(p[1].t(), PP.PC->TC))
    //    return;

    //  We cannot evaluate Pr2 here unless -t: internal node states could be inconsistent!
    //  double Pr1 = log(p[0].probability());
    //  double Pr2 = log(p[1].probability());

    vector<log_double_t> rho(2,1);

    // Because we would select between topologies before selecting
    // internal node states, the reverse distribution cannot depend on 
    // the internal node state of the proposed new topology/alignment

    vector< A5::hmm_order > orders(2);
    orders[0] = A5::get_nodes_random(p[0].t(), b);
    orders[1] = A5::get_nodes_random(p[1].t(), b);

    bool smart = (uniform() < 0.5);
    if (smart)
    {
	//    std::cerr<<"order = "<<order.nodes<<"\n";
	orders[0] = order;
	orders[0].topology = 0;
	std::swap(orders[0].nodes[2], orders[0].nodes[3]);
	vector<int> v1 = {orders[0].nodes[0],
			  orders[0].nodes[1],
			  orders[0].nodes[2],
			  orders[0].nodes[3]};
    
	//    std::cerr<<"v1 = "<<v1<<"\n";
	orders[1] = order;
	std::swap(orders[1].nodes[1], orders[1].nodes[2]);
	orders[1].topology = 1;
	vector<int> v2 = {orders[1].nodes[0],
			  orders[1].nodes[2],
			  orders[1].nodes[3],
			  orders[1].nodes[1]};

	//    std::cerr<<"v2 = "<<v2<<"\n";
	assert(v1 == v2);
    }

    int C = -1;
    try {
	C = sample_A5_multi(p,orders,rho);
    }
    catch (choose_exception<log_double_t>& c)
    {
	c.prepend(__PRETTY_FUNCTION__);
	throw c;
    }

    if (C != -1) {
	PP = p[C];
    }

    //  if (C == 1) std::cerr<<"MH-diff = "<<Pr2 - Pr1<<"\n";

    MCMC::Result result(2);

    double L0 = 1;
    if (PP.t().has_branch_lengths())
        L0 = PP.t().branch_length(b);

    result.totals[0] = (C>0)?1:0;
    // This gives us the average length of branches prior to successful swaps
    if (C>0)
	result.totals[1] = L0;
    else
	result.counts[1] = 0;

    if (smart)
	NNI_inc(Stats,"NNI (2-way smart)", result, L0);
    else
	NNI_inc(Stats,"NNI (2-way stupid)", result, L0);
}

void two_way_NNI_SPR_sample(owned_ptr<context>& P, MoveStats& Stats, int b) 
{
    Parameters& PP = *P.as<Parameters>();
    if (PP.t().is_leaf_branch(b)) return;
    if (not PP.t().has_branch_lengths()) return;

    if (log_verbose >= 3) std::cerr<<"[two_way_NNI_SPR_sample]\n";

    A5::hmm_order order = A5::get_nodes_random(PP.t(), b);
    const auto& nodes = order.nodes;

    PP.select_root(b);
    PP.cache_likelihood_branches();

    int b1 = PP.t().find_branch(nodes[4],nodes[1]);
    int b2 = PP.t().find_branch(nodes[5],nodes[2]);

    vector<Parameters> p(2,PP);

    // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
    p[1].NNI_discard_alignment(b1, b2);
  
    //  if (not extends(p[1].t(), PP.PC->TC))
    //    return;

    double LA = p[0].t().branch_length(p[0].t().find_branch(nodes[4],nodes[0]));
    double LB = p[0].t().branch_length(p[0].t().find_branch(nodes[4],nodes[5]));
    double LC = p[0].t().branch_length(p[0].t().find_branch(nodes[5],nodes[3]));

    p[1].setlength(p[1].t().find_branch(nodes[0],nodes[4]),LA + LB);
    p[1].setlength(p[1].t().find_branch(nodes[4],nodes[5]),LC*uniform());
    p[1].setlength(p[1].t().find_branch(nodes[5],nodes[3]),LC - p[1].t().branch_length(p[0].t().find_branch(nodes[4],nodes[5])));

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
	result.totals[1] = p[0].t().branch_length(b);
    else
	result.counts[1] = 0;
    NNI_inc(Stats,"NNI (2-way/SPR)", result, p[0].t().branch_length(b));
}

vector<int> NNI_branches(const TreeInterface& t, int b) 
{
    vector<int> branches;
    branches.push_back(b);

    t.append_branches_after(b, branches);
    t.append_branches_before(b, branches);

    assert(branches.size() == 5);

    return branches;
}

void two_way_NNI_and_branches_sample(owned_ptr<context>& P, MoveStats& Stats, int b) 
{
    Parameters& PP = *P.as<Parameters>();
    if (PP.t().is_leaf_branch(b)) return;
    if (not PP.t().has_branch_lengths()) return;

    if (log_verbose >= 3) std::cerr<<"[two_way_NNI_and_branches_sample]\n";

    A5::hmm_order order = A5::get_nodes_random(PP.t(), b);
    const auto& nodes = order.nodes;

    PP.select_root(b);
    PP.cache_likelihood_branches();

    int b1 = PP.t().find_branch(nodes[4],nodes[1]);
    int b2 = PP.t().find_branch(nodes[5],nodes[2]);

    vector<Parameters> p(2,PP);

    //---------------- Do the NNI operation -------------------//
    // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
    p[1].NNI_discard_alignment(b1, b2);
  
    //  if (not extends(p[1].t(), PP.PC->TC))
    //    return;

    //------------- Propose new branch lengths ----------------//
    log_double_t ratio = 1.0;
    vector<int> branches = NNI_branches(p[1].t(), b);

    for(int i=0;i<branches.size();i++) {

	auto factor = exp_to<log_double_t>(gaussian(0,0.05));

	double L = p[1].t().branch_length( branches[i] ) * double(factor);

        if (not p[1].t().can_set_branch_length(branches[i])) return;
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
	result.totals[1] = p[0].t().branch_length(b);
    else
	result.counts[1] = 0;

    NNI_inc(Stats,"NNI (2-way) + branches", result, p[0].t().branch_length(b));
}

void two_way_NNI_sample(owned_ptr<context>& P, MoveStats& Stats, int b) 
{
    Parameters& PP = *P.as<Parameters>();
    if (PP.t().is_leaf_branch(b)) return;

    if (log_verbose >= 3) std::cerr<<"[two_way_NNI_sample]\n";

    double U = uniform();
    if (U < 0.33333333) {
	two_way_topology_sample(P,Stats,b);
    }
    else if (U < 0.66666666)
	two_way_NNI_SPR_sample(P,Stats,b);
    else
	two_way_NNI_and_branches_sample(P,Stats,b);
}

void three_way_topology_sample_slice(owned_ptr<context>& P, MoveStats& Stats, int b) 
{
    Parameters& PP = *P.as<Parameters>();
    if (PP.t().is_leaf_branch(b)) return;

    if (PP.variable_alignment()) return;

    if (log_verbose >= 3) std::cerr<<"[three_way_topology_sample_slice]\n";

    A5::hmm_order order = A5::get_nodes_random(PP.t(), b);
    const auto& nodes = order.nodes;

    //------ Generate Topologies and alter caches ------///
    PP.select_root(b);
    PP.cache_likelihood_branches();

    int b1 = PP.t().find_branch(nodes[4],nodes[1]);
    int b2 = PP.t().find_branch(nodes[5],nodes[2]);
    int b3 = PP.t().find_branch(nodes[5],nodes[3]);

    vector<Parameters> p(3,PP);

    // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
    p[1].NNI_discard_alignment(b1, b2);

    // Internal node states may be inconsistent after this: p[2].alignment_prior() undefined!
    p[2].NNI_discard_alignment(b1, b3);

    const vector<log_double_t> rho(3,1);

    double L = PP.t().branch_length(b);

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
    result.totals[2] = std::abs(PP.t().branch_length(b) - L);
    result.totals[3] = logp1.count + logp2.count + logp3.count;

    //  if (C == 1) std::cerr<<"slice-diff3 = "<<Pr2 - Pr1<<"\n";
    //  if (C == 2) std::cerr<<"slice-diff3 = "<<Pr3 - Pr1<<"\n";

    // stats are here mis-reported!
    NNI_inc(Stats,"NNI (3-way,slice)", result, L);
}

optional<log_double_t>& operator*=(optional<log_double_t>& pr1, const optional<log_double_t>& pr2)
{
    if (pr1)
    {
	if (pr2)
	    pr1.value() *= pr2.value();
	else
	    pr1 = {};
    }

    return pr1;
}

optional<log_double_t> optimize(Parameters& P, const vector<int>& nodes)
{
    optional<log_double_t> ratio = 1;
    if (uniform() < 0.5)
    {
	ratio *= tri_sample_alignment_ratio(P, nodes[4], nodes[0]);
	if (not ratio) return {};
	ratio *= tri_sample_alignment_ratio(P, nodes[4], nodes[1]);
	if (not ratio) return {};
	ratio *= tri_sample_alignment_ratio(P, nodes[5], nodes[2]);
	if (not ratio) return {};
	ratio *= tri_sample_alignment_ratio(P, nodes[5], nodes[3]);
	if (not ratio) return {};
	ratio *= tri_sample_alignment_ratio(P, nodes[5], nodes[4]);
	if (not ratio) return {};
	ratio *= tri_sample_alignment_ratio(P, nodes[4], nodes[5]);
    }
    else
    {
	ratio *= tri_sample_alignment_ratio(P, nodes[4], nodes[5]);
	if (not ratio) return {};
	ratio *= tri_sample_alignment_ratio(P, nodes[5], nodes[4]);
	if (not ratio) return {};
	ratio *= tri_sample_alignment_ratio(P, nodes[5], nodes[3]);
	if (not ratio) return {};
	ratio *= tri_sample_alignment_ratio(P, nodes[5], nodes[2]);
	if (not ratio) return {};
	ratio *= tri_sample_alignment_ratio(P, nodes[4], nodes[1]);
	if (not ratio) return {};
	ratio *= tri_sample_alignment_ratio(P, nodes[4], nodes[0]);
    }
    return ratio;
}

void two_way_topology_5A_sample(owned_ptr<context>& P, MoveStats& /*Stats*/, int b)
{
    Parameters& PP = *P.as<Parameters>();
    if (PP.t().is_leaf_branch(b)) return;

    if (log_verbose >= 3) std::cerr<<"[two_way_topology_5A_sample]\n";

    A5::hmm_order order = A5::get_nodes_random(PP.t(), b);
    const auto& nodes = order.nodes;
    PP.select_root(b);
    PP.cache_likelihood_branches();

    int b1 = PP.t().find_branch(nodes[4],nodes[1]);
    int b2 = PP.t().find_branch(nodes[5],nodes[2]);

    //------ Generate Topologies and alter caches ------///

    // 1. Resample alignments on original topology and compute reverse_sampling_pr/forward_sampling_pr
    vector< A5::hmm_order > orders(2);
    auto P0 = PP;
    auto Like0A = P0.likelihood();
    auto Prob0A = P0.probability();
    auto PrAl0A = P0.prior_alignment();

    orders[0] = A5::get_nodes_random(P0.t(), b);
    optional<log_double_t> ratio = 1;
    ratio *= optimize(P0, orders[0].nodes);
    if (not ratio) return;

    auto Like0B = P0.likelihood();
    auto Prob0B = P0.probability();
    auto PrAl0B = P0.prior_alignment();

    // 2. Resample alignment on new topology and compute reverse_sampling_pr/forward_sampling_pr
    auto P1 = P0;
    P1.NNI_discard_alignment(b1, b2);
    // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
    orders[1] = A5::get_nodes_random(P1.t(), b);

    vector<Parameters> p({P0,P1});
    auto ratio_tree = sample_A5_ratio(p, orders, {1,1});
    ratio *= ratio_tree;
    if (not ratio) return;

    P1 = p[1];

    bool accept1 = accept_MH(P0, P1, *ratio_tree);

    auto Like1A = P1.likelihood();
    auto Prob1A = P1.probability();
    auto PrAl1A = P1.prior_alignment();

    // 3. Resampling alignments on new topology and compute reverse_sampling_pr/forward_sampling_pr
    ratio *= optimize(P1, orders[1].nodes);
    if (not ratio) return;

    auto Like1B = P1.likelihood();
    auto Prob1B = P1.probability();
    auto PrAl1B = P1.prior_alignment();

    // 4. Accept/reject the new topology with resampled alignments given the proposal ratio.
    bool accept2 = perform_MH(PP, P1, *ratio);
    std::cerr<<std::boolalpha<<"[two_way_topology_5A_sample] accept1 = "<<accept1<<"        accept2 = "<<accept2<<"\n";
    if (accept2 and not accept1)
    {
	std::cerr<<"Prob0A = "<<Prob0A <<" Like0A = "<<Like0A<<" PrAl0A = "<<PrAl0A<<"\n";
	std::cerr<<"Prob0B = "<<Prob0B <<" Like0B = "<<Like0B<<" PrAl0B = "<<PrAl0B<<"\n";
	std::cerr<<"Prob1A = "<<Prob1A <<" Like1A = "<<Like1A<<" PrAl1A = "<<PrAl1A<<"\n";
	std::cerr<<"Prob1B = "<<Prob1B <<" Like1B = "<<Like1B<<" PrAl1B = "<<PrAl1B<<"\n";
	std::cerr<<"\n\n";
    }
}

void three_way_NNI_sample(Parameters& PP, MoveStats& Stats, int b, int b1, int b2, int b3)
{
    PP.select_root(b);
    PP.cache_likelihood_branches();

    //------ Generate Topologies and alter caches ------///
    vector<Parameters> p(3,PP);

    double L0 = 1;
    if (PP.t().has_branch_lengths())
        L0 = PP.t().branch_length(b);

    vector< A5::hmm_order > orders(3);
    orders[0] = A5::get_nodes_random(p[0].t(), b);

    // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
    p[1].NNI_discard_alignment(b1, b2);
    orders[1] = A5::get_nodes_random(p[1].t(), b);

    // Internal node states may be inconsistent after this: p[2].alignment_prior() undefined!
    p[2].NNI_discard_alignment(b1, b3);
    orders[2] = A5::get_nodes_random(p[2].t(), b);
  
    const vector<log_double_t> rho(3,1);

    //------ Resample alignments and select topology -----//

    try {
	int C = sample_A5_multi(p,orders,rho);

        if (C != -1) {
            PP = p[C];
        }

        MCMC::Result result(2);

        result.totals[0] = (C>0)?1:0;
        // This gives us the average length of branches prior to successful swaps
        if (C>0)
            result.totals[1] = L0;
        else
            result.counts[1] = 0;

        NNI_inc(Stats,"NNI (3-way)", result, L0);
    }
    catch (choose_exception<log_double_t>& c)
    {
	c.prepend(__PRETTY_FUNCTION__);
	throw c;
    }
}

void three_way_NNI_A4_sample(Parameters& PP, MoveStats& Stats, int b, int b1, int b2, int b3)
{
    PP.select_root(b);
    PP.cache_likelihood_branches();

    //------ Generate Topologies and alter caches ------///
    vector<Parameters> p(3,PP);

    double L0 = 1;
    if (PP.t().has_branch_lengths())
        L0 = PP.t().branch_length(b);

    vector< A4::hmm_order > orders(3);
    orders[0] = A4::get_nodes_random(p[0].t(), b);

    // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
    p[1].NNI_discard_alignment(b1, b2);
    orders[1] = A4::get_nodes_random(p[1].t(), b);

    // Internal node states may be inconsistent after this: p[2].alignment_prior() undefined!
    p[2].NNI_discard_alignment(b1, b3);
    orders[2] = A4::get_nodes_random(p[2].t(), b);

    const vector<log_double_t> rho(3,1);

    //------ Resample alignments and select topology -----//

    try {
        int C = sample_A4_multi(p,orders,rho);

        if (C != -1) {
            PP = p[C];
        }

        MCMC::Result result(2);

        result.totals[0] = (C>0)?1:0;
        // This gives us the average length of branches prior to successful swaps
        if (C>0)
            result.totals[1] = L0;
        else
            result.counts[1] = 0;

        NNI_inc(Stats,"NNI (3-way)", result, L0);
    }
    catch (choose_exception<log_double_t>& c)
    {
        c.prepend(__PRETTY_FUNCTION__);
        throw c;
    }
}

void three_way_topology_sample(owned_ptr<context>& P, MoveStats& Stats, int b) 
{
    Parameters& PP = *P.as<Parameters>();
    if (PP.t().is_leaf_branch(b)) return;

    if (log_verbose >= 3) std::cerr<<"[three_way_topology_sample]\n";

    double slice_fraction = get_setting_or("NNI_slice_fraction",-0.25);

    if (not PP.variable_alignment() and uniform() < slice_fraction)
    {
	three_way_topology_sample_slice(P, Stats, b);
    }
    else
    {
	A5::hmm_order order = A5::get_nodes_random(PP.t(), b);
	const auto& nodes = order.nodes;

	int b1 = PP.t().find_branch(nodes[4],nodes[1]);
	int b2 = PP.t().find_branch(nodes[5],nodes[2]);
	int b3 = PP.t().find_branch(nodes[5],nodes[3]);

        three_way_NNI_sample(PP, Stats, b, b1, b2, b3);
    }
}

/* Like NNI, except
 * + we can only swap branches pointing away from the root
 * + we need to avoid children being older than their parents.
 * + there may be a degree 2 node.
 *
 * Specificially, we need u to be younger than y.
 *
 *                       v
 *                       |
 *                    ---x---           ---x---
 *                    |     |           |     |
 *                  --y--   |    OR   --y--   |
 *                  |   |   |         |   |   |
 *                  z   w   u         z   w   u
 *
 * Here, tipward_branches[0] = (y,z)
 *       tipward_branches[1] = (y,w)
 *       other_branches[0]   = (x,u)
 *       other_branches[1]   = (x,v) if x not the root
 *       b           = (x,y)
 */


void three_way_time_tree_NNI_sample(owned_ptr<context>& P, MoveStats& Stats, int b)
{
    if (log_verbose >= 3) std::cerr<<"[three_way_time_tree_NNI_sample]\n";

    Parameters& PP = *P.as<Parameters>();
    auto T = PP.t();

    // 1. Skip if this is not an internal branch
    if (T.is_leaf_branch(b)) return;

    // 2. Point branch away b = (x,y) away from root.
    if (not T.away_from_root(b)) b = T.reverse(b);

    int x = T.source(b);
    int y = T.target(b);

    assert(T.degree(x) == 2 or T.degree(x) == 3);
    // We shouldn't be seeing degree-2 nodes except on the root branch.
    assert(T.degree(y) == 3);

    // 3. Get branches to child nodes z and w
    auto tipward_branches = T.branches_after(b);
    assert(tipward_branches.size() == 2);
    int b2 = tipward_branches[0]; // (y,z)
    int b3 = tipward_branches[1]; // (y,w)

    // 4. Get branch to sibling node u
    auto other_branches = T.branches_after(T.reverse(b));
    assert(other_branches.size() == 1 or other_branches.size() == 2);
    if (other_branches.size() == 2)
    {
	if (not T.away_from_root(other_branches[0]))
	    std::swap(other_branches[0],other_branches[1]);
	assert(not T.away_from_root(other_branches[1]));
    }

    int b1 = other_branches[0]; // (x,u)
    int u = T.target(other_branches[0]);
    assert(T.away_from_root(b1));

    // 5. Check that node u is YOUNGER THAN node y
    if (T.node_time(u) > T.node_time(y)) return;

    if (other_branches.size() == 2)
    {
	// 6a. x is NOT the root.

	// We consider interchanging b1 <-> b2 and b1 <-> b3
	three_way_NNI_sample(PP, Stats, b, b1, b2, b3);
    }
    else
    {
	// 6b. x is the root.
	three_way_NNI_A4_sample(PP, Stats, b, b1, b2, b3);

	// We could also create moves to resample the alignment on 2 adjacent branches.
    }
}

void three_way_topology_and_A5_2D_sample(owned_ptr<context>& P, MoveStats& Stats, int b, std::optional<int> bandwidth)
{
    Parameters& PP = *P.as<Parameters>();
    auto t = PP.t();
    if (t.is_leaf_branch(b)) return;

    if (log_verbose >= 3) std::cerr<<"[three_way_topology_and_A5_2D_sample]\n";

    //--------- Generate the Different Topologies -------//
    // We do NOT always resample the connection between two_way_nodes [0] and [4].
    // Should we?
    PP.select_root(b);
    PP.cache_likelihood_branches();

    //------ Generate Topologies and alter caches ------///
    vector<Parameters> p(3,PP);

    double L0 = 1;
    if (PP.t().has_branch_lengths())
	L0 = PP.t().branch_length(b);

    vector< A5::hmm_order > orders(3);
    orders[0] = A5::get_nodes_random(PP.t(), b);

    auto& nodes = orders[0].nodes;
    int b1 = t.find_branch(nodes[4], nodes[1]);
    int b2 = t.find_branch(nodes[5], nodes[2]);
    int b3 = t.find_branch(nodes[5], nodes[3]);

    // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
    p[1].NNI_discard_alignment(b1, b2);
    orders[1] = A5::get_nodes_random(p[1].t(), b);

    // Internal node states may be inconsistent after this: p[2].alignment_prior() undefined!
    p[2].NNI_discard_alignment(b1, b3);
    orders[2] = A5::get_nodes_random(p[2].t(), b);

    const vector<log_double_t> rho(3,1);

    int C = -1;
    try {
	C = sample_A5_2D_multi(p, orders, rho, bandwidth);
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
	result.totals[1] = L0;
    else
	result.counts[1] = 0;

    NNI_inc(Stats,"NNI (5-way) + A", result, L0);
}

void three_way_topology_and_A3_2D_sample(owned_ptr<context>& P, MoveStats& Stats, int b, std::optional<int> bandwidth) 
{
    Parameters& PP = *P.as<Parameters>();
    if (PP.t().is_leaf_branch(b)) return;

    if (log_verbose >= 3) std::cerr<<"[three_way_topology_and_A3_2D_sample]\n";

    A5::hmm_order order = A5::get_nodes_random(PP.t(), b);
    const auto& two_way_nodes = order.nodes;

    //--------- Generate the Different Topologies -------//
    // We ALWAYS resample the connection between two_way_nodes [0] and [4].

    double L0 = PP.t().branch_length(b);

    PP.select_root(b);
    PP.cache_likelihood_branches();
    auto t = PP.t();

    int b1 = t.find_branch(two_way_nodes[4],two_way_nodes[1]);
    int b2 = t.find_branch(two_way_nodes[5],two_way_nodes[2]);
    int b3 = t.find_branch(two_way_nodes[5],two_way_nodes[3]);

    vector<Parameters> p(3,PP);

    // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
    p[1].NNI(b1, b2, true);

    // Internal node states may be inconsistent after this: p[2].alignment_prior() undefined!
    p[2].NNI(b1, b3, true);

    vector< vector< int> > nodes;
    for(int i=0;i<p.size();i++)
	nodes.push_back(A3::get_nodes_branch_random(p[i].t(), two_way_nodes[4], two_way_nodes[0]) );

    const vector<log_double_t> rho(3,1);

    int C = -1;
    try {
	C = sample_tri_multi(p,nodes,rho);
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
	result.totals[1] = L0;
    else
	result.counts[1] = 0;
  
    NNI_inc(Stats,"NNI (3-way) + A", result, L0);
}

void three_way_topology_and_alignment_sample(owned_ptr<context>& P, MoveStats& Stats, int b, std::optional<int> bandwidth)
{
    three_way_topology_and_A3_2D_sample(P, Stats, b, bandwidth);
}

