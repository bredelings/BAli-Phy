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

///
/// \file sample-topology-SPR.C
///
/// \brief Contains functions for subtree-prune-and-regraft (SPR) transition kernels.
///

#include <cmath>
#include "util/assert.hh"
#include <iostream>
#include "sample.H"
#include "rng.H"
#include "probability/choose.H"
#include "probability/probability.H"

#include "dp/3way.H"
#include "tree/tree-util.H"
#include "util-random.H"
#include "dp/alignment-sums.H"
#include "alignment/alignment-constraint.H"
#include "substitution/substitution.H"

using MCMC::MoveStats;

using boost::dynamic_bitset;
using std::vector;
using std::string;
using std::map;
using std::pair;
using std::tuple;

using std::cout;
using std::cerr;
using std::endl;

int random_int_from_double(double x)
{
    int n = (int)x;
    x -= n;
    n += poisson(x);
    return n;
}

int n_SPR_moves(const Parameters& P)
{
    double f = P.load_value("SPR_amount",0.1);
    int n = random_int_from_double(P.t().n_branches()*f);
    return n+1;
}

void SPR_inc(MoveStats& Stats, MCMC::Result result,const string& name,double L)
{
    Stats.inc(name, result);

    if (L < 0.5)
	Stats.inc(name+"-0.5", result);
    else if (L < 1)
	Stats.inc(name+"-1.0", result);
    else if (L < 2.0)
	Stats.inc(name+"-2.0", result);
    else 
	Stats.inc(name+"-2.0+", result);
}

int topology_sample_SPR(vector<Parameters>& p,const vector<log_double_t>& rho,int n1, int n2) 
{
    assert(p.size() == 2);
    assert(p[0].variable_alignment() == p[1].variable_alignment());

    //----------- Generate the Different node lists ---------//
    vector< vector<int> > nodes(2);
    nodes[0] = A3::get_nodes_branch_random(p[0].t(), n1, n2);     // Using two random orders can lead to different total
    nodes[1] = A3::get_nodes_branch_random(p[1].t(), n1, n2);     //  probabilities for p[i] and p[j] when p[i] == p[j].

    try {
	return sample_tri_multi(p,nodes,rho,true,true);
    }
    catch (choose_exception<log_double_t>& c)
    {
	c.prepend(string(__PRETTY_FUNCTION__)+"\n");

	throw c;
    }
}

#include "slice-sampling.H"

int topology_sample_SPR_slice_connecting_branch(vector<Parameters>& p,int b) 
{
    int b_ = p[0].t().undirected(b);

    branch_length_slice_function logp1(p[0],b_);
    branch_length_slice_function logp2(p[1],b_);

    //  We cannot evaluate Pr2 here unless -t: internal node states could be inconsistent!
    //  double Pr1 = log(p[0].probability());
    //  double Pr2 = log(p[1].probability());

    vector<slice_function*> logp;
    logp.push_back(&logp1);
    logp.push_back(&logp2);

    double w = p[0].branch_mean();

    std::pair<int,double> choice = slice_sample_multi(logp,w,-1);

    return choice.first;
}

int topology_sample_SPR_slice_slide_node(vector<Parameters>& p,int b) 
{
    slide_node_slice_function logp1(p[0],b);
    slide_node_slice_function logp2(p[1],b);

    //  We cannot evaluate Pr2 here unless -t: internal node states could be inconsistent!
    //  double Pr1 = log(p[0].probability());
    //  double Pr2 = log(p[1].probability());

    vector<slice_function*> logp;
    logp.push_back(&logp1);
    logp.push_back(&logp2);

    double w = p[0].branch_mean();

    vector<double> X0(2);
    X0[0] = logp1.current_value();
    X0[1] = uniform()*(logp2.upper_bound);

    std::pair<int,double> choice = slice_sample_multi(X0,logp,w,-1);

    return choice.first;
}

// Consider penalizing lengths for being too close to equilibrium: branches couldn't get infinitely long.
// Consider using actual substitution matrices.
// Consider measuring similarities/differences by counting.
// Problem: how do we handle multiple partitions?

vector<double> effective_lengths(const TreeInterface& t)
{
    vector<double> lengths(2*t.n_branches(),0);

    vector<int> branches = branches_from_leaves(t);

    for(int i=0;i<branches.size();i++)
    {
	lengths[branches[i]] = t.branch_length(branches[i]);

	vector<int> pre_b = t.branches_before(branches[i]);

	if (pre_b.size() > 0) {
	    double Pr_change_on_all = 1;
	    for(int j=0;j<pre_b.size();j++)
		Pr_change_on_all *= (1.0-exp(-lengths[pre_b[j]]));
	    double Pr_no_change_on_at_least_1 = 1.0-Pr_change_on_all;
	    if (Pr_no_change_on_at_least_1 > 0)
		lengths[branches[i]] += -log(Pr_no_change_on_at_least_1);
	    assert(lengths[branches[i]] >= t.branch_length(branches[i]));
	}
    }

    return lengths;
}

double effective_length(const TreeInterface& t, int b)
{
    return effective_lengths(t)[b];
}

double effective_length(const TreeInterface& T, const tree_edge& E)
{
    int b = T.find_branch(E);
    return effective_length(T, b);
}

vector<double> effective_lengths_min(const TreeInterface& t)
{
    vector<double> lengths(2*t.n_branches(),0);

    vector<int> branches = branches_from_leaves(t);

    for(int i=0;i<branches.size();i++)
    {
	lengths[branches[i]] = t.branch_length(branches[i]);

	vector<int> pre_b = t.branches_before(branches[i]);

	if (pre_b.size() > 0) 
	{
	    double min_prev = t.branch_length(pre_b[0]);
	    for(int j=1;j<pre_b.size();j++)
		min_prev = std::min(min_prev, t.branch_length(pre_b[j]));

	    lengths[branches[i]] += min_prev;
	}
    }

    return lengths;
}


int choose_SPR_target(const TreeInterface& T1, int b1) 
{
    //----- Select the branch to move to ------//
    auto subtree_nodes = T1.partition(T1.reverse(b1));
    subtree_nodes[T1.target(b1)] = true;

    vector<int> branches;
    vector<double> lengths;

    for(int i=0;i<T1.n_branches();i++) 
    {
	// skip branch if its contained in the subtree
	if (subtree_nodes[T1.target(i)] and 
	    subtree_nodes[T1.source(i)])
	    continue;

	double L = 1.0;

	// down-weight branch if it is one of the subtree's 2 neighbors
	if (subtree_nodes[T1.target(i)] or 
	    subtree_nodes[T1.source(i)])
	    L = 0.5;

	branches.push_back(i);
	lengths.push_back(L);
    }

    try {
	int b2 = branches[ choose(lengths) ];

	return b2;
    }
    catch (choose_exception<log_double_t>& c)
    {
	c.prepend(__PRETTY_FUNCTION__);
	throw c;
    }
}

MCMC::Result SPR_stats(const TreeInterface& T1, const TreeInterface& T2, bool success, int bins, int b1 = -1)
{
    MCMC::Result result(2+bins,0);

    result.counts[0] = 1;
    if (success) result.totals[0] = 1;

    int dist = topology_distance(T1,T2)/2;

    if (b1 != -1) 
	//---------------- Check if topology changed ----------------//
    {
	int n1 = T1.target(b1);
	int n2 = T1.source(b1);
	assert( n1 == T2.target(b1) );
	assert( n2 == T2.source(b1) );

	vector<int> connected1 = T1.branches_after(T1.find_branch(n2,n1));

	vector<int> connected2 = T2.branches_after(T2.find_branch(n2,n1));
 
	bool same_topology = (
	    (T1.target(connected1[0]) == T2.target(connected2[0]) and T1.target(connected1[1]) == T2.target(connected2[1])) or
	    (T1.target(connected1[0]) == T2.target(connected2[1]) and T1.target(connected1[1]) == T2.target(connected2[0]))
	    );

	if (same_topology)
	    assert(dist == 0);
	else
	    assert(dist > 0);
    }

    // count dist in [0,bins)
    if (dist > bins) dist = bins;
    for(int i=0;i<=bins;i++) {
	if (dist == i) {
	    result.counts[1+i] = 1;
	    if (success) result.totals[1+i] = 1;
	}
    }

    return result;
}

MCMC::Result SPR_stats(const TreeInterface& T1, const TreeInterface& T2, bool success, int bins, const tree_edge& B1)
{
    int b1 = T1.find_branch(B1);
    return SPR_stats(T1,T2,success,bins,b1);
}

// Do an SPR (moving the subtree behind b1 to branch b2) and create the pairwise alignment
// on the (fused) edge that we prune from.
double do_SPR(Parameters& P, int b1,int b2, const vector<int>& nodes0)
{
    auto t = P.t();
  
    //------ Find the two old branches ------//
    vector<int> connected1 = t.branches_after(b1);

    //------ Generate the new topology ------//
    if (t.target(b2) == t.target(b1) or 
	t.source(b2) == t.target(b1))
	;
    else
    {
	// For the method of choosing A23 here, see section MCMC: sample_tri + SPR in bali-phy.lyx

        // 1. First prune the subtree behind b1

	auto B1 = P.t().edge(b1);
	auto B2 = P.t().edge(b2);

	// 1a. Find the right pairwise alignment between nodes[2] and nodes[3]
	vector<pairwise_alignment_t> A23(P.n_data_partitions());
	for(int i = 0; i < P.n_data_partitions(); i++)
	{
	    // The central node (nodes[0]) is mapped to bit 3!
	    auto b0123 = A3::get_bitpath(P[i], nodes0);
	    A23[i]  = get_pairwise_alignment_from_bits(b0123, 1, 2);
	    assert(A23[i].length1() == P[i].seqlength(nodes0[2]));
	    assert(A23[i].length2() == P[i].seqlength(nodes0[3]));
	}

	// 1b. Pull out (nodes0[1],nodes0[0]) by connecting (nodes0[2],nodes0[3])
	//                                              and (nodes0[0],nodes0[0]) - a circular edge!
	P.prune_subtree(B1);

	// 1c. Set the correct pairwise alignment for the branch (nodes0[2],nodes0[3])
	int branch_from_2_to_3 = P.t().find_branch(nodes0[2],nodes0[3]);
	for(int i = 0; i < P.n_data_partitions(); i++)
	    P[i].set_pairwise_alignment(branch_from_2_to_3, A23[i]);

	// 2. Second, graft the subtree behind b1 onto b2.

	// Graft (nodes0[1],nodes0[0]) into b2=(nodes1[2],nodes1[3])
	P.regraft_subtree(B1, B2);
    }

    //------ Find the two new branches ------//
    vector<int> connected2 = t.branches_after(b1);

    assert(connected1.size() == 2);
    assert(connected2.size() == 2);

    //------- Place the split randomly -------//
    double L1 = t.branch_length(connected1[0]) + t.branch_length(connected1[1]);
    double L2 = t.branch_length(connected2[0]) + t.branch_length(connected2[1]);

    P.setlength(connected2[0], uniform() * L2 );
    P.setlength(connected2[1], L2 - t.branch_length(connected2[0]) );

    return L2/L1;
}

MCMC::Result sample_SPR(Parameters& P, int b1, int b2, bool slice = false)
{
    // Defs: ----- Object P stores alignment A0 on tree T1

    const int bins = 6;

    // 0. Check that b2 isn't behind b1
    assert(P.t().partition(b1)[P.t().target(b2)]);
    assert(P.t().partition(b1)[P.t().source(b2)]);

    // 1. ----- Create objects p[0] and p[1] to store new alignments A1 and A2 for tree T1 and T2 ----//
    int n1 = P.t().target(b1);
    int n2 = P.t().source(b1);

    P.set_root(n1);
    vector<Parameters> p(2,P);

    // 2. ----- Generate the node order for 3-way alignment paths P.t().target(b1)) -------- //

    vector< vector<int> > nodes(2);      // Using two random orders can lead to different total
                                         // probabilities for p[i] and p[j] when p[i].t() == p[j].t().

    nodes[0] = A3::get_nodes_branch_random(p[0].t(), n1, n2);

    // 3. ----- Create the new tree T2 and the pairwise alignment on the (fused) edge that we prune from.

    /* double ratio = */ do_SPR(p[1], b1, b2, nodes[0]); // FIXME - do we need to USE the ratio anywhere?

    // 4. ----- Generate the node order for 3-way alignment paths at p[1].t().target(b1) --- //

    nodes[1] = A3::get_nodes_branch_random(p[1].t(), n1, n2);

    // 5. ----- Check that nodes[i] are connected correctly in p[i].t()
    for(int i=0; i < p.size(); i++)
    {
	assert(p[i].t().is_connected(nodes[i][0],nodes[i][1]));
	assert(p[i].t().is_connected(nodes[i][0],nodes[i][2]));
	assert(p[i].t().is_connected(nodes[i][0],nodes[i][3]));
    }

    // 6. ----- Check that we are propagating variable_alignment correctly.
    assert(p.size() == 2);
    assert(p[0].variable_alignment() == p[1].variable_alignment());

    //  bool tree_changed = not p[1].t().is_connected(nodes[0],nodes[2]) or not p[1].t().is_connected(nodes[0],nodes[3]);


    // SLICE: optionall quit here and do the slide_node slice-sampling move variant instead.
    if (slice)
    {
	int C = topology_sample_SPR_slice_slide_node(p,b1);
	if (C != -1)
	    P = p[C];
	return SPR_stats(p[0].t(), p[1].t(), C>0, bins, b1);
    }
    
    try
    {
	// 6. ----- Choose * (C == -1) the original tree/alignment (C==-1)
	//                 * (C ==  0) the original tree with a new alignment
	//                 * (C ==  1) the      new tree with a new alignment

	vector<log_double_t> rho = {1, 1};
	int C = sample_tri_multi(p, nodes, rho, true, true);

	for(auto& x : p)
	{
	    for(int b=0; b<2*x.t().n_branches(); b++)
		for(int i=0;i<x.n_data_partitions(); i++)
		{
		    auto A = x[i].get_pairwise_alignment(b);
		    int n1 = x.t().source(b);
		    int n2 = x.t().target(b);
		    assert(A.length1() == x[i].seqlength(n1));
		    assert(A.length2() == x[i].seqlength(n2));
		}
	}

	// 7. Move to the new configuration if chosen.

	if (C != -1)  P = p[C];

	// 8. Return statistics about how far we did (or didn't) move via SPR.

	return SPR_stats(p[0].t(), p[1].t(), C>0, bins, b1);
    }
    catch (choose_exception<log_double_t>& c)
    {
	c.prepend(string(__PRETTY_FUNCTION__)+"\n");
	throw c;
    }
}

int choose_subtree_branch_uniform(const TreeInterface& T) 
{
    int b1 = -1;
    while (true)
    {
	b1 = myrandom(T.n_branches()*2);

	// forbid branches leaf branches - no attachment point!
	if (T.is_leaf_node(T.target(b1))) continue;

	break;
    }

    return b1;
}

int choose_subtree_branch_uniform2(const TreeInterface& T) 
{
    int b1 = -1;
    while (true)
    {
	b1 = myrandom(T.n_branches()*2);

	// forbid branches leaf branches - no attachment point!
	if (T.is_leaf_node(T.target(b1))) continue;

	// forbid branches with only 1 attachment point - not very useful.
	vector<int> after = T.branches_after(b1);
	bool ok = false;
	for(int b: after)
	    if (not T.is_leaf_node(T.target(b)))
		ok = true;
	if (not ok) continue;

	break;
    }

    return b1;
}


void sample_SPR_flat_one(owned_ptr<Model>& P,MoveStats& Stats,int b1) 
{
    Parameters& PP = *P.as<Parameters>();

    if (PP.t().is_leaf_node(PP.t().target(b1))) return;

    // Allow turning off these moves.
    if (not PP.load_value("SPR-jump",true)) return;

    // When we don't have an alignment matrix, we can't just attach to some
    // random branch if we don't know where it is, unless we can re-align.
    for(int i=0; i< PP.n_data_partitions(); i++)
	if (not PP[i].variable_alignment()) return;

    double p = P->load_value("SPR_slice_fraction",-0.25);

    int b2 = choose_SPR_target(PP.t(),b1);

    double L_effective = effective_length(PP.t(), b1);

    if (not PP.variable_alignment() and uniform() < p) {
	MCMC::Result result = sample_SPR(PP,b1,b2,true);
	SPR_inc(Stats,result,"SPR (flat/slice)",L_effective);
    }
    else  {
	MCMC::Result result = sample_SPR(PP,b1,b2);
	SPR_inc(Stats,result,"SPR (flat)",L_effective);
    }
}

std::ostream& operator<<(std::ostream& o, const tree_edge& b)
{
    o<<"["<<b.node1<<","<<b.node2<<"]";
    return o;
}

/// Represent positions along branches as a fraction in [0,1) from node1 to node2
struct spr_attachment_points: public map<tree_edge,double>
{
};

/// Represent the probability of attaching to a branch
struct spr_attachment_probabilities: public map<tree_edge,log_double_t>
{
    map<tree_edge,log_double_t> LLL;
};

bool points_to(const tree_edge& E1, const tree_edge& E2)
{
    return (E1.node2 == E2.node1 or E1.node2 == E2.node2);
}

bool behind_edge(const TreeInterface& T, const tree_edge& E1, const tree_edge& E2, bool close=false)
{
    if (points_to(E1, E2)) return true;
    if (not close) {
	int b = T.find_branch(E1);
	return T.partition(b)[E2.node1] and T.partition(b)[E2.node2];
    }
    else
	return false;
}

/* We need to NNI to ONE of the 4 grandchild edges */
// E1, E3, and E5 all point away from E1.node1.
void SPR_by_NNI(Parameters& P, const tree_edge& E1, const tree_edge& E2, bool close, bool disconnect_subtree)
{
    const auto& t = P.t();

    assert(behind_edge(t, E1.reverse(), E2));

    if (points_to(E1.reverse(),E2))
	return; //already attached to edge E2!

    // 1. Get edges E3 and E5 behind E1.reverse(), and decide which one points to E2.
    vector<int> connected = t.branches_after(t.reverse(t.find_branch(E1)));
    assert(connected.size() == 2);
    tree_edge E3 = t.edge(connected[0]);
    tree_edge E5 = t.edge(connected[1]);

    if (behind_edge(t, E3, E2, close))
	std::swap(E3,E5);
    else if (behind_edge(t, E5, E2, close))
	; // OK
    else
    {
	std::abort();
    }

    assert(behind_edge(t, E5, E2, close));

    // 2. Get edges E4a and E4b in front of E5, and decide which one points to E2.
    connected = t.branches_after(t.find_branch(E5));

    tree_edge E4a = t.edge(connected[0]);
    tree_edge E4b = t.edge(connected[1]);

    if (E2 == E4a or behind_edge(t,E4a,E2,close))
	std::swap(E4a,E4b);
    else if (E2 == E4b or behind_edge(t,E4b,E2,close))
	; // OK
    else
	std::abort();

    if (close)
	assert(E2 == E4b);
    else
	assert(E2 == E4b or behind_edge(t,E4b,E2));

    // 3. Record lengths for branches before NNI.
    double L3 = t.branch_length(t.find_branch(E3));
    /* double L2 = */ t.branch_length(t.find_branch(E2));
    double L5 = t.branch_length(t.find_branch(E5));

    // 4. Do NNI.
    P.NNI(E3, E4b, disconnect_subtree); // Source nodes for E4b/E2 and E3 are not moved, but branch names are moved.
    std::swap(E4b.node1, E3.node1);

    // 5. Fix up branch lengths and source nodes for E4b/E2 and E3.
    P.setlength(P.t().find_branch(E3), L3 + L5);
    P.setlength(P.t().find_branch(E5), 0.0);
}


// Let b_target = (x,y).  Then we have (x,n0) and (n0,y).
// B_unbroken_target specifies the orientation for the distance U.
void set_lengths_at_location(Parameters& P, int n0, double L, const tree_edge& b_target, const spr_attachment_points& locations)
{
    // 1. Look up location attachment info for this branch.
    map<tree_edge, double>::const_iterator record = locations.find(b_target);
    if (record == locations.end())
    {
	std::cerr<<"Branch not found in spr location object!\n"<<std::endl;
	std::abort();
    }

    // 2. U is the fraction of the way from B_unbroken_target.node1 toward B_unbroken_target.node2 to place the new node.
    tree_edge B_unbroken_target = record->first;
    double U = record->second; 

    // 3. Find the names of the branches
    int b1 = P.t().find_branch(B_unbroken_target.node1, n0);
    int b2 = P.t().find_branch(n0, B_unbroken_target.node2);

    // 4. Get the lengths of the two branches
    double L1 = L*U;
    double L2 = L - L1;

    // 5. Set the lengths of the two branches
    P.setlength(b1, L1);
    P.setlength(b2, L2);
}

/// Perform an SPR move: move the subtree BEHIND \a b1 to the branch indicated by \a b2,
///  and choose the point on the branch specified in \a locations.
void SPR_at_location(Parameters& P, const tree_edge& b_subtree, const tree_edge& b_target, const spr_attachment_points& locations, bool disconnect_subtree=false)
{
#ifndef NDEBUG
    double total_length_before = tree_length(P.t());
#endif

    // If we are already at b_target, then its one of the branches after b_subtree.  Then do nothing.
    if (b_subtree.node2 == b_target.node1 or b_subtree.node2 == b_target.node2)
	return;

    // unbroken target branch
    /// \todo Correctly handle moving to the same topology -- but allow branch lengths to change.
    double L = P.t().branch_length(P.t().find_branch(b_target));

    // node joining the subtree to the rest of the tree
    int n0 = b_subtree.node2;

    // Perform the SPR operation (specified by a branch TOWARD the pruned subtree)
    SPR_by_NNI(P, b_subtree.reverse(), b_target, true, disconnect_subtree);

    set_lengths_at_location(P, n0, L, b_target, locations);

#ifndef NDEBUG
    double total_length_after = tree_length(P.t());
    assert(std::abs(total_length_after - total_length_before) < 1.0e-9);
#endif

    //  FIXME - switch SPR_at_location( ) to use NNI.
    //  FIXME - split spr_search routines into separate file?
    //  FIXME - Don't send a variable parameters object into spr_search_attachment_points
    //  NOTE - The NNI doesn't actually change the alignment MATRIX.
    //         Since we are actually using the MATRIX, perhaps we should fix this.
}

struct attachment_branch
{
    int prev_i = -1;
    tree_edge edge;
    tree_edge sibling;
};

/// A struct to compute and store information about attachment points their branch names
struct spr_info
{
public:
    const TreeInterface T;

    /// The branch pointing AWAY from the subtree to prune
    tree_edge b_parent;

    /// The two branches that make up the current attachment branch
    vector<int> child_branches;

    /// The current attachment branch, specified in terms of its endpoint nodes
    tree_edge initial_edge;

    vector<attachment_branch> attachment_branch_pairs;

    // Lengths of branches
    vector<double> L;

    /// The number of places we could regraft, including the current site
    unsigned n_attachment_branches() const {return attachment_branch_pairs.size();}

    /// The length of each attachment branch, indexed in the same way as attachment_branches
    const vector<double>& attachment_branch_lengths() const
	{
	    return L;
	}
  
    /// Express a branch \a in attachment_branches in terms of its endpoint nodes
    tree_edge get_tree_edge(int b) const
	{
#ifndef NDEBUG
	    if (not T.subtree_contains_branch(T.find_branch(b_parent),b))
		throw myexception()<<"spr_info::get_tree_edge( ): Subtree does not contain branch "<<b;
#endif

	    int n0 = b_parent.node2;
	    if (T.target(b) == n0 or T.source(b) == n0)
		return initial_edge;

	    return T.edge(b);
	}

    /// Express properties of branches as vectors indexed by their position in attachment_branch_pairs
    template<typename U>
    vector<U> convert_to_vector(const map<tree_edge,U>& M) const
	{
	    assert(M.size() == n_attachment_branches());
	    vector<U> v;
	    v.reserve(n_attachment_branches());
	    for(const auto& bp : attachment_branch_pairs)
		v.push_back( M.at( bp.edge ) );      // Assumes a particular orientation of the edge

	    return v;
	}

    spr_info(const TreeInterface& T, const tree_edge& b);
}; 

void branch_pairs_after(const TreeInterface& T, int prev_i, const tree_edge& prev_b, vector<attachment_branch>& branch_pairs)
{
    vector<int> after = T.branches_after(T.find_branch(prev_b));
    assert(after.size() == 0 or after.size() == 2);
    for(int j=0; j<after.size(); j++)
    {
	tree_edge curr_b = T.edge(after[j]);
	tree_edge sibling = T.edge(after[1-j]);
	branch_pairs.push_back({prev_i, curr_b, sibling});
	int curr_i = branch_pairs.size()-1;
	branch_pairs_after(T, curr_i, curr_b, branch_pairs);
    }
}


vector<attachment_branch> branch_pairs_after(const TreeInterface& T, const tree_edge& b_parent)
{
    auto child_branches = T.branches_after(T.find_branch(b_parent));
    auto b1 = T.edge( child_branches[0] );
    auto b2 = T.edge( child_branches[1] );
    tree_edge b0 { b1.node2, b2.node2 };

    vector<attachment_branch> branch_pairs;
    branch_pairs.push_back({-1,b0,{}});

    branch_pairs_after(T, 0, b1, branch_pairs);
    branch_pairs_after(T, 0, b2, branch_pairs);

    return branch_pairs;
}

spr_info::spr_info(const TreeInterface& T_, const tree_edge& b)
    :T(T_),b_parent(b)
{
    child_branches = sort_and_randomize(T.branches_after(T.find_branch(b_parent)));
    assert(child_branches.size() == 2);

    int B1 = child_branches[0];
    int B2 = child_branches[1];
    initial_edge = tree_edge(T.target(B1), T.target(B2));

    /*----------- get the list of possible attachment points, with [0] being the current one.------- */
    // \todo - With tree constraints, or with a variable alignment and alignment constraints,
    //          we really should eliminate branches that we couldn't attach to, here.

    attachment_branch_pairs = branch_pairs_after(T, b_parent);

    for(const auto& bp: attachment_branch_pairs)
    {
	const auto& E = bp.edge;
	if (E == initial_edge)
	    L.push_back(T.branch_length(child_branches[0]) + T.branch_length(child_branches[1]));
	else if (E == initial_edge.reverse())
	    std::abort();
	else
	    L.push_back(T.branch_length(T.find_branch(E)));
    }
}

/// Get a list of attachment branches, and a location for attachment on each branch
spr_attachment_points get_spr_attachment_points(const TreeInterface& T, const tree_edge& subtree_edge)
{
    spr_info I(T, subtree_edge);

    tree_edge initial_edge(T.target(I.child_branches[0]), T.target(I.child_branches[1]));
    double L0a = T.branch_length(I.child_branches[0]);
    double L0b = T.branch_length(I.child_branches[1]);

    // compute attachment location for current branche
    spr_attachment_points locations;
    locations[initial_edge] = L0a/(L0a+L0b);

    // compute attachment locations for non-current branches
    for(int i=1;i<I.n_attachment_branches();i++)
	locations[I.attachment_branch_pairs[i].edge] = uniform();

    return locations;
}

// OK so if B1=(x,y) and initial_edge = (a,b) then we should have edges (y,a) and (y.b) in the tree
// Here we are aligning {x,a,b} with y undefined.
vector<HMM::bitmask_t> get_3way_alignment(data_partition P, int a, int b, int x, int y)
{
    constexpr int a_bit = 0;
    constexpr int b_bit = 1;
    constexpr int x_bit = 2;
    constexpr int y_bit = 3;

    int bxy = P.t().find_branch(x,y);
    int bya = P.t().find_branch(y,a);
    int byb = P.t().find_branch(y,b);

    auto Axy = convert_to_bits(P.get_pairwise_alignment(bxy), x_bit, y_bit);
    auto Aya = convert_to_bits(P.get_pairwise_alignment(bya), y_bit, a_bit);
    auto Ayb = convert_to_bits(P.get_pairwise_alignment(byb), y_bit, b_bit);

    auto Aabxy = Glue_A(Axy,Glue_A(Aya, Ayb));

    // Clear bit #3 (y)
    for(auto& z: Aabxy)
	z.set(y_bit,false);

    P.unset_pairwise_alignment(bxy);
    P.unset_pairwise_alignment(bya);
    P.unset_pairwise_alignment(byb);

    return Aabxy;
}

tuple<int,int,int,vector<vector<HMM::bitmask_t>>>
get_3way_alignments(const Parameters& P,  const tree_edge& b_subtree, const tree_edge& b_target)
{
    int a = b_target.node1; // bit 0
    int b = b_target.node2; // bit 1
    int x = b_subtree.node1; // bit 2
    int y = b_subtree.node2; // bit 3

    vector<vector<HMM::bitmask_t>> As(P.n_data_partitions());
    for(int i=0;i<P.n_data_partitions();i++)
	As[i] = get_3way_alignment(P.get_data_partition(i), a, b, x, y);
    return tuple<int,int,int,vector<vector<HMM::bitmask_t>>>{a, b, x, As};
}

void set_3way_alignment(data_partition P, int bxy, int bya, int byb, vector<HMM::bitmask_t> alignment)
{
    constexpr int a_bit = 0;
    constexpr int b_bit = 1;
    constexpr int x_bit = 2;
    constexpr int y_bit = 3;

    // compute the alignment for a-y,b-y,y-x being minimally connected
    for(auto& z: alignment)
    {
	int inc=0;
	if (z.test(a_bit)) inc++;
	if (z.test(b_bit)) inc++;
	if (z.test(x_bit)) inc++;
	if (inc >= 2)
	    z.set(y_bit,true);
    }

    P.set_pairwise_alignment(bxy, get_pairwise_alignment_from_bits(alignment, x_bit, y_bit));
    P.set_pairwise_alignment(bya, get_pairwise_alignment_from_bits(alignment, y_bit, a_bit));
    P.set_pairwise_alignment(byb, get_pairwise_alignment_from_bits(alignment, y_bit, b_bit));
}

void set_3way_alignments(Parameters& P, const tree_edge& b_subtree, const tree_edge& b_target, const tuple<int,int,int,vector<vector<HMM::bitmask_t>>>& alignments)
{
    using std::get;

    int a = b_target.node1; // bit 0
    int b = b_target.node2; // bit 1
    int x = b_subtree.node1; // bit 2
    int y = b_subtree.node2; // bit 3

    int bxy = P.t().find_branch(x,y);
    int bya = P.t().find_branch(y,a);
    int byb = P.t().find_branch(y,b);

    // Orient the target branch to match the 3-way alignment
    if (get<0>(alignments) == b and get<1>(alignments) == a) std::swap(a,b);

    assert(get<2>(alignments) == x);
    assert(get<0>(alignments) == a and get<1>(alignments) == b);
    for(int i=0; i<P.n_data_partitions(); i++)
	set_3way_alignment(P[i], bxy, bya, byb, get<3>(alignments)[i]);
}

vector<HMM::bitmask_t>
move_pruned_subtree(data_partition P, const vector<HMM::bitmask_t>& alignment_prev, int b_ab, int b_bc, int b_bd)
{
    constexpr int a_bit = 0;
    constexpr int b_bit = 1;
    constexpr int x_bit = 2;
    constexpr int c_bit = 3;
    constexpr int d_bit = 4;

    // 1. Construct 5-way alignment of {a,b,c,d,x}
    auto A_bc = convert_to_bits(P.get_pairwise_alignment(b_bc), b_bit, c_bit);
    auto A_bd = convert_to_bits(P.get_pairwise_alignment(b_bd), b_bit, d_bit);
    auto A_abxcd = Glue_A(alignment_prev,Glue_A(A_bc, A_bd));

    // 2. Recompute presence/absence of character in b after moving x from ab to bc
    for(auto& a: A_abxcd)
    {
	int inc=0;
	if (a.test(0)) inc++;
	if (a.test(4)) inc++;
	if (a.test(2) or a.test(3)) inc++;
	a.set(1, inc > 1);
    }

    // 3. Modify the pairwise alignments along ab and bd since b has been modified.
    P.set_pairwise_alignment(b_ab, get_pairwise_alignment_from_bits(A_abxcd, a_bit, b_bit));
    P.set_pairwise_alignment(b_bd, get_pairwise_alignment_from_bits(A_abxcd, b_bit, d_bit));

    // 4. Return alignment of bc with x.  (0 <- b, 1 <- c, 2 <- x)
    return remap_bitpath(A_abxcd, {{b_bit,0}, {c_bit,1}, {x_bit,2}});
}

tuple<int,int,int,vector<vector<HMM::bitmask_t>>>
move_pruned_subtree(Parameters& P,
		    const tuple<int,int,int,vector<vector<HMM::bitmask_t>>>& alignments_prev,
		    const tree_edge& b_subtree, tree_edge b_prev, const tree_edge& b_next, const tree_edge& b_sibling)
{
    using std::get;
    // 1. Rotate b_prev to point toward b_next
    if (b_prev.node2 != b_next.node1 and b_prev.node2 != b_next.node2) b_prev = b_prev.reverse();

    // 2. Determine if the prev_alignment has its node0 and node1 in the right order.
    bool flip_prev_A = false;
    if (get<0>(alignments_prev) != b_prev.node1)
    {
	flip_prev_A = true;
	assert(get<0>(alignments_prev) == b_prev.node2 and get<1>(alignments_prev) == b_prev.node1);
    }
    else
	assert(get<0>(alignments_prev) == b_prev.node1 and get<1>(alignments_prev) == b_prev.node2);

    // 2. Extract branch names from tree
    int x = b_subtree.node1;
//    int y = b_subtree.node2;

    int a = b_prev.node1;
    int b = b_prev.node2;

    assert(b_next.node1 == b);
    int c = b_next.node2;

    assert(b_sibling.node1 == b);
    int d = b_sibling.node2;

    int b_ab = P.t().find_branch(a,b);
    int b_bc = P.t().find_branch(b,c);
    int b_bd = P.t().find_branch(b,d);

    // 3. Adjust pairwise alignments and construct 3-node alignment for attaching to new edge
    vector<vector<HMM::bitmask_t>> alignments_next(P.n_data_partitions());
    for(int i=0; i<P.n_data_partitions(); i++)
	if (flip_prev_A)
	    alignments_next[i] = move_pruned_subtree(P[i], remap_bitpath(std::get<3>(alignments_prev)[i],{1,0,2,3,4}), b_ab, b_bc, b_bd);
	else
	    alignments_next[i] = move_pruned_subtree(P[i], std::get<3>(alignments_prev)[i], b_ab, b_bc, b_bd);

    return tuple<int,int,int,vector<vector<HMM::bitmask_t>>>{b, c, x, alignments_next};
}

/// Compute the probability of pruning b1^t and regraftion at \a locations
///
/// After this routine, likelihood caches and subalignment indices for branches in the
/// non-pruned subtree should reflect the situation where the subtree has been pruned.
///
spr_attachment_probabilities SPR_search_attachment_points(Parameters P, const tree_edge& subtree_edge, const spr_attachment_points& locations)
{
#ifndef NDEBUG
    auto peels0 = substitution::total_peel_internal_branches + substitution::total_peel_leaf_branches;
#endif

    // The attachment node for the pruned subtree.
    // This node will move around, but we will always peel up to this node to calculate the likelihood.
    int root_node = subtree_edge.node2;
    // Because the attachment node keeps its name, this will stay in effect throughout the likelihood calculations.
    P.set_root(root_node);

    spr_info I(P.t(), subtree_edge);

    if (I.n_attachment_branches() == 1) return spr_attachment_probabilities();

    /*----------------------- Initialize likelihood for each attachment point ----------------------- */
    Parameters initial = P;
    Parameters detached = initial;
    detached.prune_subtree(subtree_edge);
    spr_attachment_probabilities Pr;
    Pr[I.initial_edge] = initial.heated_likelihood() * initial.prior_no_alignment();
#ifdef DEBUG_SPR_ALL
    Pr.LLL[I.initial_edge] = initial.heated_likelihood();
#endif

    vector<Parameters> Ps;
    vector<tuple<int,int,int,vector<vector<HMM::bitmask_t>>>> alignments3way;
    Ps.reserve(I.attachment_branch_pairs.size());
    Ps.push_back(detached);
    alignments3way.reserve(I.attachment_branch_pairs.size());
    alignments3way.push_back(get_3way_alignments(initial, subtree_edge, I.initial_edge));
    for(int i=1;i<I.attachment_branch_pairs.size();i++)
    {
	// Define target branch b2 - pointing away from subtree_edge
	const auto& BB = I.attachment_branch_pairs[i];
	const tree_edge& next_target_edge = BB.edge;

	int prev_i = BB.prev_i;
	const tree_edge& prev_target_edge = I.attachment_branch_pairs[prev_i].edge;

	if (prev_i != 0) assert(prev_target_edge.node2 == next_target_edge.node1);
	Ps.push_back(Ps[prev_i]);
	assert(Ps.size() == i+1);
	auto& p = Ps.back();
	alignments3way.push_back( move_pruned_subtree(p, alignments3way[prev_i], subtree_edge, prev_target_edge, next_target_edge, BB.sibling) );
    }
    for(int i=(int)I.attachment_branch_pairs.size()-1;i>0;i--)
    {
	const tree_edge& next_target_edge = I.attachment_branch_pairs[i].edge;
	auto& p = Ps[i];
	double L = p.t().branch_length(p.t().find_branch(next_target_edge));

	// 1. Reconnect the tree
	p.regraft_subtree(subtree_edge, next_target_edge);

        // 2. Set branch lengths
	int n0 = subtree_edge.node2;
	set_lengths_at_location(p, n0, L, next_target_edge, locations);

	// 3. Set pairwise alignments on three branches
	set_3way_alignments(p, subtree_edge, next_target_edge, alignments3way[i]);

	// 4. Compute likelihood and probability
	Pr[next_target_edge] = p.heated_likelihood() * p.prior_no_alignment();

#ifdef DEBUG_SPR_ALL
	Pr.LLL[next_target_edge] = p.heated_likelihood();
#endif
	Ps.pop_back();
    }

#ifndef NDEBUG
    auto peels1 = substitution::total_peel_internal_branches + substitution::total_peel_leaf_branches;
    std::cerr<<"total_peels2 = "<<peels1 - peels0<<std::endl;
#endif
    /*----------------------- Initialize likelihood for each attachment point ----------------------- */

    // We had better not let this get changed!
    assert(P.subst_root() == root_node);

    return Pr;
}

/// This just computes nodes and calls sample_tri_multi
bool SPR_accept_or_reject_proposed_tree(Parameters& P, vector<Parameters>& p,
					const vector<log_double_t>& Pr,
					const vector<log_double_t>& PrL,
					const spr_info& I, int C,
					const spr_attachment_points& locations
    )
{
    tree_edge E_parent = I.b_parent;
    int n1 = E_parent.node2;
    int n2 = E_parent.node1;

    assert(p.size() == 2);
    assert(p[0].variable_alignment() == p[1].variable_alignment());

    vector<double> L = I.attachment_branch_lengths();

    //----------------- Generate the Different node lists ---------------//
    vector< vector<int> > nodes(2);
    nodes[0] = A3::get_nodes_branch_random(p[0].t(), n1, n2);     // Using two random orders can lead to different total
    nodes[1] = A3::get_nodes_branch_random(p[1].t(), n1, n2);     //  probabilities for p[0] and p[1] when p[0] == p[1].
    bool do_cube = (uniform() < p[0].load_value("cube_fraction",0.0));

    boost::shared_ptr<sample_A3_multi_calculation> tri;
    if (do_cube)
	tri = boost::shared_ptr<sample_A3_multi_calculation>(new sample_cube_multi_calculation(p, nodes, true, true));
    else
	tri = boost::shared_ptr<sample_A3_multi_calculation>(new sample_tri_multi_calculation(p, nodes, true, true));
    tri->run_dp();

    //--------- Compute PrL2: reverse proposal probabilities ---------//

    vector<log_double_t> PrL2 = PrL;
#ifndef DEBUG_SPR_ALL
    if (P.variable_alignment())
#endif
    {
	spr_attachment_probabilities PrB2 = SPR_search_attachment_points(p[1], E_parent, locations);
	vector<log_double_t> Pr2 = I.convert_to_vector(PrB2);
    
	if (not P.variable_alignment())
	    for(int i=0;i<Pr.size();i++)
		assert(std::abs(Pr[i].log() - Pr2[i].log()) < 1.0e-9);
    
	PrL2 = Pr2;
	for(int i=0;i<PrL2.size();i++)
	    PrL2[i] *= L[i];
    }

    //----------------- Specify proposal probabilities -----------------//
    vector<log_double_t> rho(2,1);
    rho[0] = L[0]*choose_MH_P(0, C, PrL ); // Pr(proposing 0->C)
    rho[1] = L[C]*choose_MH_P(C, 0, PrL2); // Pr(proposing C->0)
  
    tri->set_proposal_probabilities(rho);

    //------------- Accept or reject the proposed topology -------------//
    int C2 = tri->choose();

    // If the alignment is not variable, then we should always accept on this second move.
    //
    // If the alignment is variable, then we might still choose C2==0 because of the
    // different node orders in sample_tri( ).
    if (not P.variable_alignment()) 
	assert(C2 == 1);

    // If the move violates alignment constraints the we can't accept it.
    if (C2 == -1) return false;

    //---------------------- Update P based on choice ------------------//
    for(int i=0;i<P.n_data_partitions();i++) {
	//    dynamic_bitset<> s1 = constraint_satisfied(P[i].alignment_constraint, P[i].A());
	//    dynamic_bitset<> s2 = constraint_satisfied(p[C2][i].alignment_constraint, p[C2][i].A());
    
	//    report_constraints(s1,s2,i);
    }
    P = p[C2];
  
    return (C2 == 1);
}

/*
 *  1. Factoring in branch lengths?
 *
 *  pi(x) = the desired equilibrium probability.
 *  rho(x,S) = the probability of x proposing the set S to be sampled from.
 *  alpha(x,S,y) = the probability of accepting/choosing y from S when S is proposed by x.
 *  L[x] = the length of attachment branch x.
 *
 *  pi(x) * rho(x,S) * alpha(x,S,y) = pi(y) * rho(y,S) * alpha(y,S,x) 
 *  rho(x,S) = prod over branches[b] (1/L[b]) * L[x]
 *           = C * L[x]
 *  pi(x) * C * L[x] * alpha(x,S,y) = pi(y) * C * L[y] * alpha(y,S,x) 
 *  alpha(x,S,y) / alpha(y,S,x) = (pi(y)*L[y])/(pi(x)*L[x])
 *
 *  2. Therefore, the probability of choosing+accepting y should be proportional to Pr[y] * L[y].
 *  In the simplest incarnation, this is Gibbs sampling, and is independent of x.
 * 
 *  3. However, we can also use choose_MH(0,Pr), where Pr[i] = likelihood[i]*prior[i]*L[i]
 *  since this proposal/acceptance function also has the property that
 *
 *       choose_MH_P(i,j,Pr)/choose_MH_P(j,i,Pr) = Pr[j]/Pr[i].
 *
 *  4. Now, if we make this whole procedure into a proposal, the ratio for this 
 *     proposal density is
 *
 *       rho(x,S) * alpha(x,S,y)   (C * L[x]) * (D * pi(y) * L[y] )    pi(y)   1/pi(x)
 *       ----------------------- = -------------------------------- = ----- = -------
 *       rho(y,S) * alpha(y,S,x)   (C * L[y]) * (D * pi(x) * L[x] )    pi(x)   1/pi(y)
 *
 *  While the result is independent of the lengths L (which we want), the procedure for
 *  achieving this result need not be independent of the lengths.
 *
 *  We must also remember that the variable rho[i] is the proposal density for proposing the set
 *  S2 = {x,y} and so is proportional to rho(x,S) * alpha(x,S,y) = pi(y).  We could also use 1/pi(x)
 *  and get the same ratio.
 *
 */


bool sample_SPR_search_one(Parameters& P,MoveStats& Stats, const tree_edge& subtree_edge) 
{
    const int bins = 6;

    if (P.t().is_leaf_node(subtree_edge.node2)) return false;

    // The attachment node for the pruned subtree.
    // This node will move around, but we will always peel up to this node to calculate the likelihood.
    int root_node = subtree_edge.node2;
    // Because the attachment node keeps its name, this will stay in effect throughout the likelihood calculations.
    P.set_root(root_node);

    // Compute and cache conditional likelihoods up to the (likelihood) root node.
    P.heated_likelihood();

    spr_attachment_points locations = get_spr_attachment_points(P.t(), subtree_edge);

    vector<Parameters> p(2,P);

    spr_info I(P.t(), subtree_edge);

    // Compute total lengths for each of the possible attachment branches
    vector<double> L = I.attachment_branch_lengths();

    if (I.n_attachment_branches() == 1) return false;

    spr_attachment_probabilities PrB = SPR_search_attachment_points(p[1], subtree_edge, locations);

    vector<log_double_t> Pr = I.convert_to_vector(PrB);
#ifdef DEBUG_SPR_ALL
    vector<log_double_t> LLL = I.convert_to_vector(PrB.LLL);
#endif

    // Step N-1: CHOOSE an attachment point

    vector<log_double_t> PrL = Pr;
    for(int i=0;i<PrL.size();i++)
	PrL[i] *= L[i];

    int C = -1;
    try {
	C = choose_MH(0,PrL);
    }
    catch (choose_exception<log_double_t>& c)
    {
	c.prepend(__PRETTY_FUNCTION__);
	throw c;
    }

    // Step N: ATTACH to that point
    if (C != 0)
    {
	vector<int> indices;
	indices.push_back(C);
	for(int i=0;i<indices.size();i++)
	{
	    int I1 = indices[i];
	    int I2 = I.attachment_branch_pairs[I1].prev_i;
	    if (I2 > 0)
		indices.push_back(I2);
	}
	std::reverse(indices.begin(), indices.end());
	for(int i: indices)
	    SPR_at_location(p[1], subtree_edge, I.attachment_branch_pairs[i].edge, locations, true);
    }

    // enforce tree constraints
    //  if (not extends(p[1].t(), P.PC->TC))
    //    return false;

#ifdef DEBUG_SPR_ALL
    // The likelihood for attaching at a particular place should not depend on the initial attachment point.
    log_double_t L_1 = p[1].heated_likelihood();
    auto diff = L_1.log() - LLL[C].log();
    std::cerr<<"re-attachment diff = "<<diff<<std::endl;

    // FIXME - If we have an IModel, then re-attaching means that we unalign characters instead of extending their alignment
    //         to keep them aligned.
    bool has_imodel = true;
    for(int i=0;i<P.n_data_partitions();i++)
	if (not P[i].has_IModel())
	    has_imodel = false;
    if (not has_imodel)
	assert(std::abs(diff) < 1.0e-9);
#endif

    // Step N+1: Use the chosen tree as a proposal, allowing us to sample the alignment.
    //           (This should always succeed, if the alignment is fixed.)

    // Do we accept the tree proposed above?
    bool accepted = true;

    // Only resample the alignment if we are proposing a different topology.
    /// \todo Mixing: Should we realign at least one other attachment location, if P.variable_alignment()?
    try
    {
	if (C > 0)
	    accepted = SPR_accept_or_reject_proposed_tree(P, p, Pr, PrL, I, C, locations);
    }
    catch (std::bad_alloc&) {
	std::cerr<<"Allocation failed in sample_try_multi (in SPR_search_one)!  Proceeding."<<std::endl;
	return false;
    }


    MCMC::Result result = SPR_stats(p[0].t(), p[1].t(), accepted, bins, subtree_edge);
    double L_effective = effective_length(P.t(), subtree_edge);
    SPR_inc(Stats, result, "SPR (all)", L_effective);

    return ((C != 0) and accepted);
}

void sample_SPR_all(owned_ptr<Model>& P,MoveStats& Stats) 
{
    Parameters& PP = *P.as<Parameters>();
    int n = n_SPR_moves(PP);

    // double p = P.load_value("SPR_slice_fraction",-0.25);

    for(int i=0;i<n;i++) 
    {
	// Choose a directed branch to prune and regraft -- pointing away from the pruned subtree.
	int b1 = choose_subtree_branch_uniform2(PP.t());

	sample_SPR_search_one(PP, Stats, PP.t().edge(b1));
    }

    if (P->load_value("SPR_longest", true))
    {
	// Try moving the longest or least-determined branch every time.
	int least_informed_branch = argmax(effective_lengths_min(PP.t()));
	sample_SPR_flat_one(P, Stats, least_informed_branch);
	sample_SPR_search_one(PP, Stats, PP.t().edge(least_informed_branch));
    }
}

void sample_SPR_search_all(owned_ptr<Model>& P,MoveStats& Stats) 
{
    int B = P.as<Parameters>()->t().n_branches();

    for(int b=0;b<2*B;b++) {
	slice_sample_branch_length(P,Stats,b);
	auto& PP = *P.as<Parameters>();
	bool changed = sample_SPR_search_one(PP,Stats,PP.t().edge(b));
	if (not changed) three_way_topology_sample(P,Stats,b);
	slice_sample_branch_length(P,Stats,b);
    }
}

void sample_SPR_A_search_all(owned_ptr<Model>& P,MoveStats& Stats) 
{
    int B = P.as<Parameters>()->t().n_branches();

    for(int b=0;b<2*B;b++) {
	slice_sample_branch_length(P,Stats,b);
	sample_SPR_flat_one(P, Stats, b);
	auto& PP = *P.as<Parameters>();
	bool changed = sample_SPR_search_one(PP,Stats,PP.t().edge(b));
	if (not changed) three_way_topology_sample(P,Stats,b);
	slice_sample_branch_length(P,Stats,b);
	scale_means_only(P,Stats);
    }
}

vector<int> path_to(const TreeInterface& T,int n1, int n2) 
{
    assert(0 <= n1 and n1 < T.n_leaves());
    assert(0 <= n2 and n2 < T.n_leaves());
    assert(n1 != n2);

    vector<int> path; 
    path.push_back(n1);
    path.push_back(T.target(T.undirected(n1)));

    while(path.back() != n2) 
    {
	int b = T.find_branch(path[path.size()-2], path[path.size()-1]);

	for(int i: T.branches_after(b))
	{
	    if (T.partition(i)[n2]) {
		path.push_back(T.target(i));
		break;
	    }
	}
    }

    return path;
}

int jump() {
    double delta = 0;

    double U = uniform();
    if (U < 0.05)
	delta = 0;
    else if (U < 0.15)
	delta = 1;
    else {
	double a = 1;
	double b = 4;
	if (uniform() < 0.5) {
	    a = 0.5;
	    b = 4;
	}
    
	delta = 2+gamma(a,b);// mean = 2.5, shape = 2
    }
    if (uniform() < 0.5) delta = -delta;

    // round to the nearest integer
    return (int)floor(delta+0.5);
}



void choose_subtree_branch_nodes(const TreeInterface& T,int & b1, int& b2) 
{
    //------------------- Choose nodes --------------------//
    int n1 = myrandom(T.n_leaves());
    int n2 = -1;
    do { n2 = myrandom(T.n_leaves());} while (n2 == n1);

    vector<int> path = path_to(T,n1,n2);
    assert(path.size() >= 3);

    //-------------- Choose subtree on path ----------------//
    int N = path.size() - 2;
    int A = 1+myrandom(N);

    b1 = -1;
    for(int n: T.neighbors(path[A])) {
	if (n == path[A-1]) continue;
	if (n == path[A+1]) continue;

	b1 = T.find_branch(n,path[A]);
	break;
    }
    assert(b1 != -1);

    //-------------- Choose branch on path ----------------//

    // The allowed branches are [Nl,Nr]
    int Nl = -(A-1) , Nr = N-A;

    // Jump and then reflect so that we stay in the interval
    int delta = wrap(jump(), Nl, Nr);
    assert(Nl <= delta and delta <= Nr);

    // walk 'delta' way from A
    int C2 = delta + A;

    assert(1 <= C2 and C2 <= path.size()-2);

    int C3 = C2;
    if (delta < 0) C3--;
    else C3++;

    assert(0 <= C3 and C3 <= path.size()-1);

    b2 = T.undirected(T.find_branch(path[C2],path[C3]));
}

void sample_SPR_flat(owned_ptr<Model>& P,MoveStats& Stats) 
{
    Parameters& PP = *P.as<Parameters>();
    int n = n_SPR_moves(PP);

    //  double p = P->load_value("SPR_slice_fraction",-0.25);

    for(int i=0;i<n;i++) 
    {
	int b1 = choose_subtree_branch_uniform(PP.t());

	sample_SPR_flat_one(P, Stats, b1);
    }

    if (P->load_value("SPR_longest", true))
    {
	// Try moving the longest or least-determined branch every time.
	int least_informed_branch = argmax(effective_lengths_min(PP.t()));
	sample_SPR_flat_one(P, Stats, least_informed_branch);
	sample_SPR_search_one(PP, Stats, PP.t().edge(least_informed_branch));
    }
}

void sample_SPR_nodes(owned_ptr<Model>& P,MoveStats& Stats) 
{
    Parameters& PP = *P.as<Parameters>();

    // Allow turning off these moves.
    if (not PP.load_value("SPR-jump",true)) return;

    // When we don't have an alignment matrix, we can't just attach to some
    // random branch if we don't know where it is, unless we can re-align.
    for(int i=0; i< PP.n_data_partitions(); i++)
	if (not PP[i].variable_alignment()) return;

    int n = n_SPR_moves(PP);

    double p = P->load_value("SPR_slice_fraction",-0.25);

    for(int i=0;i<n;i++) {

	int b1=-1, b2=-1;
	choose_subtree_branch_nodes(PP.t(), b1, b2);

	double L_effective = effective_length(PP.t(), b1);

	if (not PP.variable_alignment() and uniform()< p) {
	    MCMC::Result result = sample_SPR(*P.as<Parameters>(),b1,b2,true);
	    SPR_inc(Stats,result,"SPR (path/slice)", L_effective);
	}
	else {
	    MCMC::Result result = sample_SPR(*P.as<Parameters>(),b1,b2);
	    SPR_inc(Stats,result,"SPR (path)", L_effective);
	}
    }

    if (P->load_value("SPR_longest", true))
    {
	// Try moving the longest or least-determined branch every time.
	int least_informed_branch = argmax(effective_lengths_min(PP.t()));
	sample_SPR_flat_one(P, Stats, least_informed_branch);
	sample_SPR_search_one(PP, Stats, PP.t().edge(least_informed_branch));
    }
}
