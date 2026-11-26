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
#include "util/log-level.hh"
#include "util/settings.hh"
#include "sample.hh"
#include "probability/choose.hh"
#include "util/rng.hh"
#include "dp/1way.hh"
#include "dp/4way.hh"
#include "dp/5way.hh"
#include "dp/alignment-sums.hh"
#include "alignment/alignment-util.hh"
#include "tree/tree-util.hh"

#include "dp/3way.hh"
#include "sample.hh"

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

optional<int> two_way_topology_sample(vector<Parameters>& p,const vector<log_double_t>& rho, int b)
{
    vector< A5::hmm_order > orders(2);
    orders[0] = A5::get_nodes_random(p[0].t(), b);
    orders[1] = A5::get_nodes_random(p[1].t(), b);

    try {
	return sample_A5_multi(p,orders,rho);
    }
    catch (choose_exception<Availability<ChoiceWeight>>& c)
    {
	c.prepend(__PRETTY_FUNCTION__);
	throw c;
    }
}


// Orient the six nodes so the specified exchange is between slots 1 and 2. Either
// end can be first: the smart order and the length join/split use this fair orientation.
static A5::hmm_order exchange_order(const TreeInterface& t, int b, int b1, int b2)
{
    auto order = A5::get_nodes(t, b);
    auto& n = order.nodes;
    if (n[0] == t.target(b1)) std::swap(n[0], n[1]);
    if (n[3] == t.target(b2)) std::swap(n[2], n[3]);
    assert(n[1] == t.target(b1) and n[2] == t.target(b2));
    if (uniform() < 0.5)
    {
        std::swap(n[0], n[3]);
        std::swap(n[1], n[2]);
        std::swap(n[4], n[5]);
    }
    return order;
}

static void two_way_topology_sample(Parameters& PP, MoveStats& Stats, int b, int b1, int b2)
{
    if (log_verbose >= 3) std::cerr<<"[two_way_topology_sample]\n";

    PP.select_root(b);
    PP.cache_likelihood_branches();
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
    bool smart = (uniform() < 0.5);
    if (smart)
    {
        // Assign the reference role uniformly to the two physical resolutions, so the
        // reverse draw can attach exactly the same tags and boundary priorities to both.
        int ref = uniform_int(0, 1);
        auto order = exchange_order(PP.t(), b, b1, b2);
        if (ref == 1) std::swap(order.nodes[1], order.nodes[2]);

        orders[ref] = order;
        std::swap(orders[ref].nodes[2], orders[ref].nodes[3]);
        orders[ref].topology = 0;
        orders[1-ref] = order;
        std::swap(orders[1-ref].nodes[1], orders[1-ref].nodes[2]);
        orders[1-ref].topology = 1;
#ifndef NDEBUG
        const auto& a = orders[ref].nodes;
        const auto& z = orders[1-ref].nodes;
        assert(a[0] == z[0] and a[1] == z[2] and a[2] == z[3] and a[3] == z[1]);
#endif
    }
    else
    {
        orders[0] = A5::get_nodes_random(p[0].t(), b);
        orders[1] = A5::get_nodes_random(p[1].t(), b);
    }

    optional<int> choice;
    try {
	choice = sample_A5_multi(p,orders,rho);
    }
    catch (choose_exception<Availability<ChoiceWeight>>& c)
    {
	c.prepend(__PRETTY_FUNCTION__);
	throw c;
    }

    if (choice)
	PP = p[*choice];

    bool changed = choice and *choice > 0;

    //  if (choice and *choice == 1) std::cerr<<"MH-diff = "<<Pr2 - Pr1<<"\n";

    MCMC::Result result(2);

    double L0 = 1;
    if (PP.t().has_branch_lengths())
        L0 = PP.t().branch_length(b);

    result.totals[0] = changed ? 1 : 0;
    // This gives us the average length of branches prior to successful swaps
    if (changed)
	result.totals[1] = L0;
    else
	result.counts[1] = 0;

    if (smart)
	NNI_inc(Stats,"NNI (2-way smart)", result, L0);
    else
	NNI_inc(Stats,"NNI (2-way stupid)", result, L0);
}

static void two_way_NNI_SPR_sample(Parameters& PP, MoveStats& Stats, int b, int b1, int b2)
{
    if (not PP.t().has_branch_lengths()) return;

    if (log_verbose >= 3) std::cerr<<"[two_way_NNI_SPR_sample]\n";

    // The inverse of a join on one side joins on the other side of the same exchange.
    auto order = exchange_order(PP.t(), b, b1, b2);
    const auto& nodes = order.nodes;
    auto t0 = PP.t();
    const int a = t0.find_branch(nodes[4], nodes[0]);
    const int c = t0.find_branch(nodes[5], nodes[3]);
    // The exchange moves the other two subtrees. These same three branch IDs are
    // joined and split in reverse, so a fixed coordinate rules out both directions.
    for (int branch: {a, b, c})
        if (not t0.can_set_branch_length_or_duration(branch)) return;
    PP.select_root(b);
    PP.cache_likelihood_branches();

    vector<Parameters> p(2,PP);

    // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
    p[1].NNI_discard_alignment(b1, b2);
  
    //  if (not extends(p[1].t(), PP.PC->TC))
    //    return;

    double LA = t0.branch_length_or_duration(a);
    double LB = t0.branch_length_or_duration(b);
    double LC = t0.branch_length_or_duration(c);

    // Join and split the additive stored coordinates.  The forward and reverse uniform-split
    // densities are 1/LC and 1/(LA+LB), so the reverse/forward ratio is LC/(LA+LB).
    double LAB = LA + LB;
    double LC1 = LC * uniform();
    double LC2 = LC - LC1;
    auto t1 = p[1].t();
    t1.set_branch_length_or_duration(a, LAB);
    t1.set_branch_length_or_duration(b, LC1);
    t1.set_branch_length_or_duration(c, LC2);

    vector<log_double_t> rho(2,1);
    rho[1] = LC/LAB;

    auto choice = two_way_topology_sample(p,rho,b);

    if (choice)
	PP = p[*choice];

    bool changed = choice and *choice > 0;

    MCMC::Result result(2);

    result.totals[0] = changed ? 1 : 0;
    // This gives us the average length of branches prior to successful swaps
    if (changed)
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

static void two_way_NNI_and_branches_sample(Parameters& PP, MoveStats& Stats, int b, int b1, int b2)
{
    if (not PP.t().has_branch_lengths()) return;

    if (log_verbose >= 3) std::cerr<<"[two_way_NNI_and_branches_sample]\n";

    PP.select_root(b);
    PP.cache_likelihood_branches();

    vector<Parameters> p(2,PP);

    //---------------- Do the NNI operation -------------------//
    // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
    p[1].NNI_discard_alignment(b1, b2);
  
    //  if (not extends(p[1].t(), PP.PC->TC))
    //    return;

    //------------- Propose new branch lengths ----------------//
    log_double_t ratio = 1.0;
    vector<int> branches = NNI_branches(p[1].t(), b);

    auto t1 = p[1].t();

    // Apply the five-dimensional log-scale random walk to stored lengths or durations.  Each
    // transformation L'=factor*L contributes its factor to the reverse/forward proposal ratio.
    for(int i=0;i<branches.size();i++) {

        auto factor = exp_to_log_space(gaussian(0,0.05));

        double L = t1.branch_length_or_duration(branches[i]) * double(factor);

        if (not t1.can_set_branch_length_or_duration(branches[i])) return;
        t1.set_branch_length_or_duration(branches[i], L);

        ratio *= factor;
    }


    vector<log_double_t> rho(2);
    rho[0] = 1.0;
    rho[1] = ratio;

    auto choice = two_way_topology_sample(p,rho,b);

    if (choice)
	PP = p[*choice];

    bool changed = choice and *choice > 0;

    MCMC::Result result(2);

    result.totals[0] = changed ? 1 : 0;
    // This gives us the average length of branches prior to successful swaps
    if (changed)
	result.totals[1] = p[0].t().branch_length(b);
    else
	result.counts[1] = 0;

    NNI_inc(Stats,"NNI (2-way) + branches", result, p[0].t().branch_length(b));
}

void two_way_NNI_sample(owned_ptr<context>& P, MoveStats& Stats, int b) 
{
    Parameters& PP = *P.as<Parameters>();
    if (PP.t().is_leaf_branch(b)) return;

    // The length-changing variants below require two degree-three nodes.
    if (PP.t().degree(PP.t().source(b)) != 3 or PP.t().degree(PP.t().target(b)) != 3)
    {
        three_way_topology_sample(P, Stats, b);
        return;
    }

    auto t = PP.t();
    int b1 = uniform_element(t.branches_after(t.reverse(b)));
    int b2 = uniform_element(t.branches_after(b));
    two_way_NNI_sample(PP, Stats, b, b1, b2);
}

// b1 and b2 point out of opposite ends of b into the two subtrees to exchange.
void two_way_NNI_sample(Parameters& PP, MoveStats& Stats, int b, int b1, int b2)
{
    if (log_verbose >= 3) std::cerr<<"[two_way_NNI_sample]\n";

    double U = uniform();
    if (U < 0.33333333) {
	two_way_topology_sample(PP,Stats,b,b1,b2);
    }
    else if (U < 0.66666666)
	two_way_NNI_SPR_sample(PP,Stats,b,b1,b2);
    else
	two_way_NNI_and_branches_sample(PP,Stats,b,b1,b2);
}

Availability<log_double_t> optimize(Parameters& P, const vector<int>& nodes)
{
    auto ratio = available(log_double_t(1));
    // Each ratio call also resamples an alignment.  Stop after a failure rather
    // than relying only on Availability multiplication and running later calls.
    if (uniform() < 0.5)
    {
	ratio *= tri_sample_alignment_ratio(P, nodes[4], nodes[0]);
	if (not ratio) return unavailable;
	ratio *= tri_sample_alignment_ratio(P, nodes[4], nodes[1]);
	if (not ratio) return unavailable;
	ratio *= tri_sample_alignment_ratio(P, nodes[5], nodes[2]);
	if (not ratio) return unavailable;
	ratio *= tri_sample_alignment_ratio(P, nodes[5], nodes[3]);
	if (not ratio) return unavailable;
	ratio *= tri_sample_alignment_ratio(P, nodes[5], nodes[4]);
	if (not ratio) return unavailable;
	ratio *= tri_sample_alignment_ratio(P, nodes[4], nodes[5]);
    }
    else
    {
	ratio *= tri_sample_alignment_ratio(P, nodes[4], nodes[5]);
	if (not ratio) return unavailable;
	ratio *= tri_sample_alignment_ratio(P, nodes[5], nodes[4]);
	if (not ratio) return unavailable;
	ratio *= tri_sample_alignment_ratio(P, nodes[5], nodes[3]);
	if (not ratio) return unavailable;
	ratio *= tri_sample_alignment_ratio(P, nodes[5], nodes[2]);
	if (not ratio) return unavailable;
	ratio *= tri_sample_alignment_ratio(P, nodes[4], nodes[1]);
	if (not ratio) return unavailable;
	ratio *= tri_sample_alignment_ratio(P, nodes[4], nodes[0]);
    }
    return ratio;
}

void two_way_topology_5A_sample(owned_ptr<context>& P, MoveStats& /*Stats*/, int b)
{
    Parameters& PP = *P.as<Parameters>();
    if (PP.t().degree(PP.t().source(b)) != 3 or PP.t().degree(PP.t().target(b)) != 3) return;

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
    auto ratio = available(log_double_t(1));
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
	auto choice = sample_A5_multi(p,orders,rho);

        if (choice)
            PP = p[*choice];

        bool changed = choice and *choice > 0;

        MCMC::Result result(2);

        result.totals[0] = changed ? 1 : 0;
        // This gives us the average length of branches prior to successful swaps
        if (changed)
            result.totals[1] = L0;
        else
            result.counts[1] = 0;

        NNI_inc(Stats,"NNI (3-way)", result, L0);
    }
    catch (choose_exception<Availability<ChoiceWeight>>& c)
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
        auto choice = sample_A4_multi(p,orders,rho);

        if (choice)
            PP = p[*choice];

        bool changed = choice and *choice > 0;

        MCMC::Result result(2);

        result.totals[0] = changed ? 1 : 0;
        // This gives us the average length of branches prior to successful swaps
        if (changed)
            result.totals[1] = L0;
        else
            result.counts[1] = 0;

        NNI_inc(Stats,"NNI (3-way)", result, L0);
    }
    catch (choose_exception<Availability<ChoiceWeight>>& c)
    {
        c.prepend(__PRETTY_FUNCTION__);
        throw c;
    }
}

void three_way_topology_sample(owned_ptr<context>& P, MoveStats& Stats, int b) 
{
    Parameters& PP = *P.as<Parameters>();
    if (PP.t().is_leaf_branch(b)) return;

    auto t = PP.t();
    if (t.degree(t.source(b)) == 3 and t.degree(t.target(b)) == 2)
        b = t.reverse(b);

    // A4 enumerates the three arrangements of the boundary subtrees, with the degree-two
    // node first. Swapping subtrees preserves these degrees and all stored branch lengths.
    if (t.degree(t.source(b)) == 2 and t.degree(t.target(b)) == 3)
    {
        int b1 = t.branches_after(t.reverse(b))[0];
        auto children = t.branches_after(b);
        three_way_NNI_A4_sample(PP, Stats, b, b1, children[0], children[1]);
        return;
    }
    if (t.degree(t.source(b)) != 3 or t.degree(t.target(b)) != 3) return;

    if (log_verbose >= 3) std::cerr<<"[three_way_topology_sample]\n";

    A5::hmm_order order = A5::get_nodes_random(PP.t(), b);
    const auto& nodes = order.nodes;

    int b1 = PP.t().find_branch(nodes[4],nodes[1]);
    int b2 = PP.t().find_branch(nodes[5],nodes[2]);
    int b3 = PP.t().find_branch(nodes[5],nodes[3]);

    three_way_NNI_sample(PP, Stats, b, b1, b2, b3);
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
    if (T.node_time(u) > T.node_time(y))
    {
        if (not PP.variable_alignment()) return;

        // The age condition rules out both swaps, but not an alignment-only update.
        // It depends only on the tree, so the reverse alignment move takes this same path.
        PP.select_root(b);
        PP.cache_likelihood_branches();
        if (T.degree(x) == 2)
            sample_A4(PP, b);
        else
            sample_A5(PP, b);
        return;
    }

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

    }
}

// Compare the three partners of the focal subtree using a five-branch alignment update.
static void three_way_topology_and_A5_2D_sample(owned_ptr<context>& P, MoveStats& Stats, int b,
                                                A5::hmm_order order, optional<int> bandwidth)
{
    Parameters& PP = *P.as<Parameters>();
    auto t = PP.t();

    if (log_verbose >= 3) std::cerr<<"[three_way_topology_and_A5_2D_sample]\n";

    //--------- Generate the Different Topologies -------//
    PP.select_root(b);
    PP.cache_likelihood_branches();

    //------ Generate Topologies and alter caches ------///
    vector<Parameters> p(3,PP);

    double L0 = 1;
    if (PP.t().has_branch_lengths())
	L0 = PP.t().branch_length(b);

    vector< A5::hmm_order > orders(3);
    orders[0] = std::move(order);

    auto& nodes = orders[0].nodes;
    int b1 = t.find_branch(nodes[4], nodes[1]);
    int b2 = t.find_branch(nodes[5], nodes[2]);
    int b3 = t.find_branch(nodes[5], nodes[3]);

    // Internal node states may be inconsistent after this: p[1].alignment_prior() undefined!
    p[1].NNI_discard_alignment(b1, b2);
    orders[1] = A5::get_nodes_random(p[1].t(), b, nodes[0]);

    // Internal node states may be inconsistent after this: p[2].alignment_prior() undefined!
    p[2].NNI_discard_alignment(b1, b3);
    orders[2] = A5::get_nodes_random(p[2].t(), b, nodes[0]);

    const vector<log_double_t> rho(3,1);

    optional<int> choice;
    try {
	choice = sample_A5_2D_multi(p, orders, rho, bandwidth);
    }
    catch (choose_exception<Availability<ChoiceWeight>>& c)
    {
	c.prepend(__PRETTY_FUNCTION__);
	throw c;
    }

    if (choice)
	PP = p[*choice];

    bool changed = choice and *choice > 0;

    MCMC::Result result(2);

    result.totals[0] = changed ? 1 : 0;
    // This gives us the average length of branches prior to successful swaps
    if (changed)
	result.totals[1] = L0;
    else
	result.counts[1] = 0;

    NNI_inc(Stats,"NNI (5-way) + A", result, L0);
}

// Compare the three attachment points for the focal subtree using constrained A3 updates.
static void three_way_topology_and_A3_2D_sample(owned_ptr<context>& P, MoveStats& Stats, int b,
                                                const vector<int>& two_way_nodes, optional<int> bandwidth)
{
    Parameters& PP = *P.as<Parameters>();

    if (log_verbose >= 3) std::cerr<<"[three_way_topology_and_A3_2D_sample]\n";

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

    // Prune P--u from (P,S)u--v(X,Y). All candidates constrain the same three ordered
    // pairwise alignments on the reduced tree: S--v, X--v and Y--v. In p[0] we obtain
    // S--v by projecting S--u--v through u. The order in nodes[0] determines which of
    // two unpaired residues comes first. Install that projection on both fused edges.
    const auto& old_star = nodes[0];
    for (int j = 0; j < p[0].n_data_partitions(); ++j)
        if (p[0][j].variable_alignment())
        {
            auto path = A3::get_bitpath(p[0][j], old_star);
            auto fused = get_pairwise_alignment_from_bits(path, 1, 2);
            for (int i = 1; i < p.size(); ++i)
            {
                int b23 = p[i].t().find_branch(old_star[2], old_star[3]);
                p[i][j].set_pairwise_alignment(b23, fused);
            }
        }

    const vector<log_double_t> rho(3,1);

    optional<int> choice;
    try {
	choice = sample_tri_2D_multi(p, nodes, rho, bandwidth);
    }
    catch (choose_exception<Availability<ChoiceWeight>>& c)
    {
	c.prepend(__PRETTY_FUNCTION__);
	throw c;
    }

    if (choice)
	PP = p[*choice];

    bool changed = choice and *choice > 0;
    
    MCMC::Result result(2);
    
    result.totals[0] = changed ? 1 : 0;
    // This gives us the average length of branches prior to successful swaps
    if (changed)
	result.totals[1] = L0;
    else
	result.counts[1] = 0;
  
    NNI_inc(Stats,"NNI (3-way) + A", result, L0);
}

// Both kernels keep the subtree in slot zero attached to the same central endpoint.
static void three_way_topology_and_alignment_sample(owned_ptr<context>& P, MoveStats& Stats, int b,
                                                    A5::hmm_order order, optional<int> bandwidth)
{
    if (uniform() < get_setting_or("A5_fraction",0.0))
        three_way_topology_and_A5_2D_sample(P, Stats, b, std::move(order), bandwidth);
    else
        three_way_topology_and_A3_2D_sample(P, Stats, b, order.nodes, bandwidth);
}

// Choose the focal subtree uniformly, using ordinary NNI outside a five-branch neighborhood.
void three_way_topology_and_alignment_sample(owned_ptr<context>& P, MoveStats& Stats, int b, std::optional<int> bandwidth)
{
    auto t = P.as<Parameters>()->t();
    // The specialized 2D proposals below use an A5 neighborhood; use A4 where appropriate.
    if (t.degree(t.source(b)) != 3 or t.degree(t.target(b)) != 3)
    {
        three_way_topology_sample(P, Stats, b);
        return;
    }

    three_way_topology_and_alignment_sample(P, Stats, b, A5::get_nodes_random(t, b), bandwidth);
}

// Compare all three partners of focal_node, a boundary node of this five-branch neighborhood.
void three_way_topology_and_alignment_sample_at_node(owned_ptr<context>& P, MoveStats& Stats, int b,
                                                     int focal_node, optional<int> bandwidth)
{
    auto t = P.as<Parameters>()->t();
    three_way_topology_and_alignment_sample(P, Stats, b, A5::get_nodes_random(t, b, focal_node), bandwidth);
}
