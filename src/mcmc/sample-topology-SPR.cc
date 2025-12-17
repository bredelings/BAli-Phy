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

// This define causes assertion failures about the length of the root sequence in several tests.
// #define DEBUG_SPR_ALL

#include "dp/2way.H"
#include <cmath>
#include "util/assert.hh"
#include <iostream>
#include "sample.H"
#include "util/rng.H"
#include "probability/choose.H"
#include "probability/probability.H"
#include "util/log-level.H"                         // for log_verbose
#include "util/settings.H"                          // for get_setting_or( )

#include "dp/3way.H"
#include "tree/tree-util.H"
#include "util/permute.H"
#include "dp/alignment-sums.H"
#include "alignment/alignment-constraint.H"
#include "substitution/likelihood.H"

using MCMC::MoveStats;

using boost::dynamic_bitset;
using std::vector;
using std::string;
using std::map;
using std::pair;
using std::tuple;
using std::optional;
using std::shared_ptr;

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
    double f = get_setting_or("SPR_amount",0.5);
    if (P.t().has_node_times())
    {
        if (P.t().n_branches() <= 4) return 0;
    }
    else
    {
        if (P.t().n_branches() < 4) return 0;
    }
    double n = f * sqrt(P.t().n_branches());
    return random_int_from_double(n) + 1;
}

template <typename I, typename T>
I argmax(const std::unordered_map<I,T>& m)
{
    auto [index0,value] = *m.begin();
    auto index = index0;
    for(auto& [i,v]: m)
    {
	if (v > value)
	{
	    index = i;
	    value = v;
	}
    }
    return index;
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
	return sample_tri_multi(p,nodes,rho);
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
    X0[1] = uniform()*(*logp2.upper_bound);

    std::pair<int,double> choice = slice_sample_multi(X0,logp,w,-1);

    return choice.first;
}

int choose_SPR_target(const TreeInterface& T1, int b1) 
{
    //----- Select the branch to move to ------//
    auto subtree_nodes = T1.partition(T1.reverse(b1));
    subtree_nodes.insert(T1.target(b1));

    std::unordered_map<int,double> branch_lengths;
    vector<double> lengths;

    for(int b: T1.branches())
    {
	// skip branch if its contained in the subtree
	if (subtree_nodes.contains(T1.target(b)) and
	    subtree_nodes.contains(T1.source(b)))
	    continue;

	double L = 1.0;

	// down-weight branch if it is one of the subtree's 2 neighbors
	if (subtree_nodes.contains(T1.target(b)) or
	    subtree_nodes.contains(T1.source(b)))
	    L = 0.5;

	branch_lengths.insert({b,L});
    }

    try {
	int b2 = choose(branch_lengths);

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

    int dist = 0; // topology_distance(T1,T2)/2;

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
	{
	    dist = 1;
	    assert(dist > 0);
	}
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

struct attachment_branch
{
    std::optional<tree_edge> prev_edge;
    tree_edge edge;
    std::optional<tree_edge> sibling;
};

typedef std::map<tree_edge, bool> spr_range;

vector<int> attachment_sub_branches(const TreeInterface& T, const tree_edge& b_parent)
{
    auto child_branches = T.branches_after(T.find_branch(b_parent));
    if (child_branches.size() != 2)
	throw myexception()<<"SPR: expected two child branches after subtree edge!";
    int b1 = child_branches[0];
    int b2 = child_branches[1];
    return vector<int>({b1,b2});
}

tree_edge attachment_edge(const TreeInterface& T, const tree_edge& b_parent)
{
    auto child_branches = attachment_sub_branches(T, b_parent);
    return tree_edge(T.target(child_branches[0]), T.target(child_branches[1]));
}

double attachment_edge_length(const TreeInterface& T, const tree_edge& b_parent)
{
    auto child_branches = attachment_sub_branches(T, b_parent);
    return T.branch_length(child_branches[0]) + T.branch_length(child_branches[1]);
}

spr_range spr_full_range(const TreeInterface& T, const tree_edge& b_parent)
{
    spr_range range;
    std::optional<double> min_age;

    // If the pruned branch doesn't point to a degree-3 node, then quit.
    if (T.degree(b_parent.node2) != 3)
        return range;

    // If the pruned branch doesn't point rootward, then quit.
    bool is_time_tree = T.has_node_times();
    if (is_time_tree)
    {
        if (not T.toward_root(T.find_branch(b_parent)))
            return range;

        min_age = T.node_time(b_parent.node1);
    }

    auto child_branches = attachment_sub_branches(T, b_parent);
    auto initial_edge = tree_edge(T.target(child_branches[0]), T.target(child_branches[1]));
    range[initial_edge] = true;

    for(int cb: child_branches)
	for(int b: T.all_branches_after(cb))
        {
            // For time trees, we can only attach on the edge if the older age is older than the min age.
            if (is_time_tree)
            {
                double t1 = T.node_time(T.source(b));
                double t2 = T.node_time(T.target(b));
                if (*min_age < std::max(t1,t2))
                    range[T.edge(b)] = true;
            }
            else
                range[T.edge(b)] = true;
        }

    return range;
}

/// A struct to compute and store information about attachment points their branch names
struct spr_info
{
public:
    const TreeInterface& T;

    /// The branch pointing AWAY from the subtree to prune
    tree_edge b_parent;

    /// The edges the parent is allowed to attach on.
    spr_range range;

    /// The two branches that make up the current attachment branch
    vector<int> child_branches;

    /// The current attachment branch, specified in terms of its endpoint nodes
    tree_edge initial_edge;

    vector<attachment_branch> attachment_branch_pairs;

    // Lengths of branches
    vector<double> relative_proposal_probs_;

    /// The number of places we could regraft, including the current site
    unsigned n_attachment_branches() const {return attachment_branch_pairs.size();}

    /// The length of each attachment branch, indexed in the same way as attachment_branches
    const vector<double>& relative_proposal_probs() const
	{
	    return relative_proposal_probs_;
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

    spr_info(const TreeInterface& T, const tree_edge& b, const spr_range& r);
};

void spr_to_index(Parameters& P, spr_info& I, int C, const vector<int>& nodes0);

// Do an SPR (moving the subtree behind b1 to branch b2) and create the pairwise alignment
// on the (fused) edge that we prune from.
void do_SPR(Parameters& P, int b1,int b2, const vector<int>& nodes0)
{
    auto B1 = P.t().edge(b1);
  
    auto B2 = P.t().edge(b2);

    if (B1.node2 == B2.node1 or B1.node2 == B2.node2)
    {
	// B2 is one of the two branches that get fused when we pull out B1.
    }
    else
    {
	auto I = spr_info(P.t(), B1, spr_full_range(P.t(), B1));

	// 1. Find the index in the attachment branch pairs
	optional<int> index;
	for(int i=0; i< I.attachment_branch_pairs.size(); i++)
	    if (I.attachment_branch_pairs[i].edge == B2)
		index = i;

	// 2. Get the original length of the attachment edge
	double original_length = P.t().branch_length(b2);

	// 3. Attach to the target edge
	spr_to_index(P, I, *index, nodes0);

	// 4. Choose the split uniformly at random
	int b3 = P.t().find_branch(B1.node2, B2.node1);
	int b4 = P.t().find_branch(B1.node2, B2.node2);

	auto U = uniform();
	P.setlength(b3, original_length * U );
	P.setlength(b4, original_length * (1-U) );
    }
}

MCMC::Result sample_SPR(Parameters& P, int b1, int b2, bool slice = false)
{
    // Defs: ----- Object P stores alignment A0 on tree T1

    const int bins = 6;

    // 0. Check that b2 isn't behind b1
    assert(P.t().partition(b1).contains(P.t().target(b2)));
    assert(P.t().partition(b1).contains(P.t().source(b2)));

    // 1. ----- Create objects p[0] and p[1] to store new alignments A1 and A2 for tree T1 and T2 ----//
    int n1 = P.t().target(b1);
    int n2 = P.t().source(b1);

    P.set_root(n1);
    P.cache_likelihood_branches();
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
	int C = sample_tri_multi(p, nodes, rho);

#ifndef NDEBUG
	for(auto& x : p)
            for(int i=0;i<x.n_data_partitions(); i++)
                if (x[i].has_pairwise_alignments())
                    for(int b: x.t().directed_branches())
                    {
                        auto A = x[i].get_pairwise_alignment(b);
                        int n1 = x.t().source(b);
                        int n2 = x.t().target(b);
                        assert(A.length1() == x[i].seqlength(n1));
                        assert(A.length2() == x[i].seqlength(n2));
                    }
#endif

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

// UNSAFE (infinite loop)!
int choose_subtree_branch_uniform(const TreeInterface& T) 
{
    assert(T.n_branches() >= 3);

    bool timetree = T.has_node_times();

    int b1 = -1;
    while (true)
    {
	b1 = uniform_element(T.directed_branches());

	// forbid branches leaf branches - no attachment point!
	if (T.is_leaf_node(T.target(b1))) continue;

        // forbid pruning subtrees that contain the root on a time-tree.
        if (timetree and T.away_from_root(b1)) continue;

	break;
    }

    return b1;
}

// UNSAFE (infinite loop)!
int choose_subtree_branch_uniform2(const TreeInterface& T) 
{
    assert(T.n_branches() >= 4);

    bool timetree = T.has_node_times();

    int b1 = -1;
    while (true)
    {
	b1 = uniform_element(T.directed_branches());

        // forbid branches leaf branches - no attachment point!
	if (T.is_leaf_node(T.target(b1))) continue;

        // forbid pruning subtrees that contain the root on a time-tree.
        if (timetree and T.away_from_root(b1)) continue;

        // forbid branches with only 1 attachment point - not very useful.
	bool ok = false;
	for(int b: T.branches_after(b1))
	    if (not T.is_leaf_node(T.target(b)))
		ok = true;

	if (not ok) continue;

	break;
    }

    return b1;
}


void sample_SPR_flat_one(owned_ptr<context>& P,MoveStats& Stats,int b1) 
{
    Parameters& PP = *P.as<Parameters>();

    if (PP.t().is_leaf_node(PP.t().target(b1))) return;

    // Allow turning off these moves.
    if (not get_setting_or("SPR-jump",true)) return;

    double p = get_setting_or("SPR_slice_fraction",-0.25);

    int b2 = choose_SPR_target(PP.t(),b1);

    double L = PP.t().branch_length(b1);

    if (not PP.variable_alignment() and uniform() < p) {
	MCMC::Result result = sample_SPR(PP,b1,b2,true);
	SPR_inc(Stats,result,"SPR (flat/slice)",L);
    }
    else  {
	MCMC::Result result = sample_SPR(PP,b1,b2);
	SPR_inc(Stats,result,"SPR (flat)",L);
    }
}

std::ostream& operator<<(std::ostream& o, const tree_edge& b)
{
    o<<"["<<b.node1<<","<<b.node2<<"]";
    return o;
}

/// Represent positions along branches.
//   With BranchLengthTrees, this is a raction in [0,1) from node1 to node2
//   With TimeTrees, this is a fraction from T1 to T2, there T1 <= T2.
struct spr_attachment_points: public map<tree_edge,double>
{
};

/// Represent the probability of attaching to a branch
struct spr_attachment_probabilities: public map<tree_edge,log_double_t>
{
    map<tree_edge,log_double_t> LLL;
};

// Let b_target = (x,y).  Then we have (x,n0) and (n0,y).
// B_unbroken_target specifies the orientation for the distance U.
void set_lengths_at_location(Parameters& P, const tree_edge& subtree_edge, const tree_edge& b_target, const spr_attachment_points& locations)
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
    int b1 = P.t().find_branch(B_unbroken_target.node1, subtree_edge.node2);
    int b2 = P.t().find_branch(subtree_edge.node2, B_unbroken_target.node2);

    if (P.t().has_node_times())
    {
        double T1a = P.t().node_time(B_unbroken_target.node1);
        double T3 = P.t().node_time(B_unbroken_target.node2);
        if (T1a > T3) std::swap(T1a,T3);
        assert(T1a <= T3);

        double T1b = P.t().node_time(subtree_edge.node1);
        assert(P.t().node_time(subtree_edge.node2) > P.t().node_time(subtree_edge.node1));
        assert(T1b <= T3);
        double T1 = std::max(T1a, T1b);
        assert(T1 <= T3);

        double T2 = T1 + U*(T3-T1);
        P.t().set_node_time(subtree_edge.node2, T2);
    }
    else
    {
        double L = P.t().branch_length(b1) + P.t().branch_length(b2);

        // 4. Get the lengths of the two branches
        double L1 = L*U;
        double L2 = L - L1;

        // 5. Set the lengths of the two branches
        P.setlength(b1, L1);
        P.setlength(b2, L2);
    }
}

void branch_pairs_after(const TreeInterface& T, const tree_edge& prev_b, const tree_edge& prev_b_pruned, vector<attachment_branch>& branch_pairs, const spr_range& range)
{
    vector<int> after = T.branches_after(T.find_branch(prev_b));

    if (after.size() == 0)
        ; // nothing to do
    else if (after.size() == 1)
    {
        tree_edge curr_b = T.edge(after[0]);
        if (range.count(curr_b))
        {
            // NO SIBLING
            branch_pairs.push_back({prev_b_pruned, curr_b, {}});
            branch_pairs_after(T, curr_b, curr_b, branch_pairs, range);
        }
    }
    else if (after.size() == 2)
    {
        for(int j=0; j<after.size(); j++)
        {
            tree_edge curr_b = T.edge(after[j]);
            if (range.count(curr_b))
            {
                tree_edge sibling = T.edge(after[1-j]);
                branch_pairs.push_back({prev_b_pruned, curr_b, {sibling}});
                branch_pairs_after(T, curr_b, curr_b, branch_pairs, range);
            }
        }
    }
    else
        throw myexception()<<"branch_pairs_after: SPR can't handle node of degree "<<after.size()+1;
}

vector<attachment_branch> branch_pairs_after(const TreeInterface& T, const tree_edge& b_parent, const spr_range& range)
{
    auto child_branches = T.branches_after(T.find_branch(b_parent));
    auto b1 = T.edge( child_branches[0] );
    auto b2 = T.edge( child_branches[1] );
    tree_edge b0 { b1.node2, b2.node2 };

    vector<attachment_branch> branch_pairs;
    branch_pairs.push_back({ {}, b0, {} });

    branch_pairs_after(T, b1, b0.reverse(), branch_pairs, range);
    branch_pairs_after(T, b2, b0,           branch_pairs, range);

    // Check that the range is connected, and connected to b_parent
    assert(branch_pairs.size() == range.size());

    return branch_pairs;
}

spr_info::spr_info(const TreeInterface& T_, const tree_edge& b, const spr_range& r)
    :T(T_), b_parent(b), range(r)
{
    child_branches = sort_and_randomize(T.branches_after(T.find_branch(b_parent)));
    assert(child_branches.size() == 2);

    int B1 = child_branches[0];
    int B2 = child_branches[1];
    initial_edge = tree_edge(T.target(B1), T.target(B2));

    /*----------- get the list of possible attachment points, with [0] being the current one.------- */
    // \todo - With tree constraints, or with a variable alignment and alignment constraints,
    //          we really should eliminate branches that we couldn't attach to, here.

    attachment_branch_pairs = branch_pairs_after(T, b_parent, range);

    std::optional<double> min_age;
    bool timetree = T.has_node_times();
    if (timetree)
    {
        assert(T.node_time(b_parent.node1) < T.node_time(b_parent.node2));
        min_age = T.node_time(b_parent.node1);
    }

    for(const auto& bp: attachment_branch_pairs)
    {
	const auto& E = bp.edge;
        if (timetree)
        {
            double T1 = T.node_time(E.node1);
            double T2 = T.node_time(E.node2);
            if (T1 > T2) std::swap(T1,T2);
            T1 = std::max(T1, *min_age);
            assert(T1 < T2);
            relative_proposal_probs_.push_back(T2 - T1);
        }
        else
        {
            if (E == initial_edge)
                relative_proposal_probs_.push_back(T.branch_length(child_branches[0]) + T.branch_length(child_branches[1]));
            else if (E == initial_edge.reverse())
                std::abort();
            else
                relative_proposal_probs_.push_back(T.branch_length(T.find_branch(E)));
        }
    }
}

/// Get a list of attachment branches, and a location for attachment on each branch
spr_attachment_points get_spr_attachment_points(const TreeInterface& T, const tree_edge& subtree_edge, const spr_range& range)
{
    spr_info I(T, subtree_edge, range);

    tree_edge initial_edge(T.target(I.child_branches[0]), T.target(I.child_branches[1]));
    spr_attachment_points locations;

    if (T.has_node_times())
    {
        double T1a = T.node_time(initial_edge.node1);
        double T3 = T.node_time(initial_edge.node2);
        if (T1a > T3) std::swap(T1a, T3);
        assert(T1a <= T3);

        double T1b = T.node_time(subtree_edge.node1);
        double T1 = std::max(T1a, T1b);

        double T2 = T.node_time(subtree_edge.node2);
        assert(T1b <= T2);
        assert(T2 <= T3);
        double U = (T2 - T1)/(T3 - T1);
        locations[initial_edge] = U;
    }
    else
    {
        double L0a = T.branch_length(I.child_branches[0]);
        double L0b = T.branch_length(I.child_branches[1]);

        // compute attachment location for current branch
        locations[initial_edge] = L0a/(L0a+L0b);
    }

    // compute attachment locations for non-current branches
    for(int i=1;i<I.n_attachment_branches();i++)
	locations[I.attachment_branch_pairs[i].edge] = uniform();

    return locations;
}

/// Extract the relationship between {x,a,b}, and unset the pairwise alignments on (x,y),(y,a),(y,b)
// OK so if B1=(x,y) and initial_edge = (a,b) then we should have edges (y,a) and (y.b) in the tree
// Here we are aligning {x,a,b} with y undefined.
vector<HMM::bitmask_t> get_3way_alignment(mutable_data_partition P, int a, int b, int x, int y)
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

// preserve_homology = not sum_out_A (in search_attachment_points) and false (in spr_to_index).
// not preserve_homology = sum_out_A
// So, I think that unless we are doing sum_out_A, we preserve homologies with the original attachment point,
//  even if there is a - on the path.  We do that by flipping the - to +.
// But if we are doing sum_out_A, we always drop homologies with the original attachment point if there is
//  a - on the path.
// In theory, we could instead perform an NNI where we resample all 5 pairwise alignments, which would allow
//  preserving homologies even when we integrate out the alignment.

tuple<int,int,int,vector<optional<vector<HMM::bitmask_t>>>>
prune_subtree_and_get_3way_alignments(Parameters& P,  const tree_edge& b_subtree, const tree_edge& b_target,
				      const vector<int>& nodes0, bool preserve_homology=false)
{
    int a = b_target.node1; // bit 0
    int b = b_target.node2; // bit 1
    int x = b_subtree.node1; // bit 2
    int y = b_subtree.node2; // bit 3

    vector<optional<pairwise_alignment_t>> A23(P.n_data_partitions());
    vector<optional<vector<HMM::bitmask_t>>> As(P.n_data_partitions());
    for(int i=0;i<P.n_data_partitions();i++)
	// 1a. For variable alignments that we aren't preserving homologies for: record the pairwise alignment A{2,3}
        if (P[i].has_pairwise_alignments())
        {
            if (not P[i].alignment_is_random())
                throw myexception()<<"Partition "<<i+1<<": can't change the tree topology because the tree-alignment is fixed!\n  Consider adding --imodel=none or --fix=tree or removing --fix=alignment.";

            if (P[i].variable_alignment() and not preserve_homology)
            {
                auto b0123 = A3::get_bitpath(P[i], nodes0); 	    // The central node (nodes[0]) is mapped to bit 3!
                A23[i]  = get_pairwise_alignment_from_bits(b0123, 1, 2);
                assert(A23[i]->length1() == P[i].seqlength(nodes0[2]));
                assert(A23[i]->length2() == P[i].seqlength(nodes0[3]));
            }
            // 1b. If we are preserving homology, then record the alignment A{a,b,x}
            else
                As[i] = get_3way_alignment(P.get_data_partition(i), a, b, x, y);
        }

    P.prune_subtree(b_subtree);

    // 2. For variable alignment partitions that we aren't preserving homologies for,
    //    set the correct pairwise alignment for the branch (nodes0[2],nodes0[3])
    int branch_from_2_to_3 = P.t().find_branch(nodes0[2],nodes0[3]);
    for(int i=0;i<P.n_data_partitions();i++)
        if (P[i].has_pairwise_alignments())
            if (P[i].variable_alignment() and not preserve_homology)
            {
                P[i].set_pairwise_alignment(branch_from_2_to_3, *A23[i]);
                // clear other pairwise alignments??
            }

    return tuple<int,int,int,vector<optional<vector<HMM::bitmask_t>>>>{a, b, x, std::move(As)};
}

void set_3way_alignment(mutable_data_partition P, int bxy, int bya, int byb, vector<HMM::bitmask_t> alignment)
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

void regraft_subtree_and_set_3way_alignments(Parameters& P, const tree_edge& b_subtree, const tree_edge& b_target, const tuple<int,int,int,vector<optional<vector<HMM::bitmask_t>>>>& alignments, bool preserve_homology=false)
{
    using std::get;

    // 1. Reconnect the tree
    P.regraft_subtree(b_subtree, b_target);

    int a = b_target.node1; // bit 0
    int b = b_target.node2; // bit 1
    int x = b_subtree.node1; // bit 2
    int y = b_subtree.node2; // bit 3

    int bxy = P.t().find_branch(x,y);
    int bya = P.t().find_branch(y,a);
    int byb = P.t().find_branch(y,b);

    // Orient the target branch to match the 3-way alignment
    if (get<0>(alignments) == b and get<1>(alignments) == a) std::swap(a,b);

    for(int i=0; i<P.n_data_partitions(); i++)
    {
        if (P[i].has_pairwise_alignments())
            if (preserve_homology or not P[i].variable_alignment())
            {
                assert(get<2>(alignments) == x);
                assert(get<0>(alignments) == a and get<1>(alignments) == b);
                set_3way_alignment(P[i], bxy, bya, byb, *get<3>(alignments)[i]);
            }
    }
}


/// Given tree with edges (a,b),(b,c),(b,d) where (x,y) is considered to attach on edge (a,b):
//    Given alignment bitpath for {abx} and
//    Given that the pairwise alignment for (a,b) is unset:
//     (1) construct alignment bitpath for {abxcd}
//     (2) update bitpath for SPR moving x from (a,b) -> (b,c).
//     (3)  set the pairwise alignment for (a,b)
//     (4)  modify the pairwise alignment for (b,d)
//     (5)  unset the pairwise alignment for (b,c)
//     (6)  return the alignment bitpath for {bcx}
//    Steps 5&6 maintain the invariant that for the edges that (x,y) is considered to attach to,
//    we don't have a pairwise alignment.
vector<HMM::bitmask_t>
move_pruned_subtree(mutable_data_partition P, const vector<HMM::bitmask_t>& alignment_prev, int b_ab, int b_bc, std::optional<int> b_bd)
{
    constexpr int a_bit = 0;
    constexpr int b_bit = 1;
    constexpr int x_bit = 2;
    constexpr int c_bit = 3;
    constexpr int d_bit = 4;

    // 1. Construct 5-way alignment of {a,b,c,d,x}
    auto A_bc = convert_to_bits(P.get_pairwise_alignment(b_bc), b_bit, c_bit);
    decltype(A_bc) A_bcd;
    if (b_bd)
    {
        auto A_bd = convert_to_bits(P.get_pairwise_alignment(*b_bd), b_bit, d_bit);
        A_bcd = Glue_A(A_bc, A_bd);
    }
    else
        auto A_bcd = std::move(A_bc);
    auto A_abxcd = Glue_A(alignment_prev, A_bcd);

    // 2. Recompute presence/absence of character in b after moving x from ab to bc
    for(auto& a: A_abxcd)
    {
	int inc=0;
	if (a.test(a_bit)) inc++;
	if (a.test(d_bit)) inc++;
	if (a.test(x_bit) or a.test(c_bit)) inc++;
	a.set(b_bit, inc > 1);
    }

    // 3. Modify the pairwise alignments along ab and bd since b has been modified.
    P.set_pairwise_alignment(b_ab, get_pairwise_alignment_from_bits(A_abxcd, a_bit, b_bit));
    if (b_bd) P.set_pairwise_alignment(*b_bd, get_pairwise_alignment_from_bits(A_abxcd, b_bit, d_bit));
    P.unset_pairwise_alignment(b_bc);

    // 4. Return alignment of bc with x.  (0 <- b, 1 <- c, 2 <- x)
    return remap_bitpath(A_abxcd, {{b_bit,0}, {c_bit,1}, {x_bit,2}});
}

tuple<int,int,int,vector<optional<vector<HMM::bitmask_t>>>>
move_pruned_subtree(Parameters& P,
		    const tuple<int,int,int,vector<optional<vector<HMM::bitmask_t>>>>& alignments_prev,
		    const tree_edge& b_subtree, tree_edge b_prev, const tree_edge& b_next, const std::optional<tree_edge>& b_sibling,
		    bool preserve_homology=false)
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

    int b_ab = P.t().find_branch(a,b);
    int b_bc = P.t().find_branch(b,c);

    std::optional<int> b_bd;
    if (b_sibling)
    {
        assert(b_sibling->node1 == b);
        int d = b_sibling->node2;
        b_bd = P.t().find_branch(b,d);
    }

    // 3. Adjust pairwise alignments and construct 3-node alignment for attaching to new edge
    vector<optional<vector<HMM::bitmask_t>>> alignments_next(P.n_data_partitions());
    for(int i=0; i<P.n_data_partitions(); i++)
        if (P[i].has_pairwise_alignments())
            if (preserve_homology or not P[i].variable_alignment())
            {
                if (flip_prev_A)
                    alignments_next[i] = move_pruned_subtree(P[i], remap_bitpath(*std::get<3>(alignments_prev)[i],{1,0,2,3,4}), b_ab, b_bc, b_bd);
                else
                    alignments_next[i] = move_pruned_subtree(P[i], *std::get<3>(alignments_prev)[i], b_ab, b_bc, b_bd);
            }

    return tuple<int,int,int,vector<optional<vector<HMM::bitmask_t>>>>{b, c, x, std::move(alignments_next)};
}

vector<optional<vector<HMM::bitmask_t>>> A23_constraints(const Parameters& P, const vector<int>& nodes, bool original);
optional<log_double_t> pr_sum_out_A_tri(Parameters& P, const vector<optional<vector<HMM::bitmask_t>>>& a23, const vector<int>& nodes);

void set_attachment_probability(spr_attachment_probabilities& Pr, const spr_attachment_points& locations, const tree_edge& subtree_edge, const tree_edge& target_edge, Parameters p2, const map<tree_edge,vector<int>>& nodes, const tuple<int,int,int,vector<optional<vector<HMM::bitmask_t>>>>& alignment3way, bool sum_out_A)
{
    // ---------------- Compute the attachment probability -----------------------
    auto& nodes_ = nodes.at(target_edge);

    // 0. Save constraints before we regraft
    vector<optional<vector<HMM::bitmask_t>>> a23_constraint;
    if (sum_out_A)
        a23_constraint = A23_constraints(p2, nodes_, false);

    // 1. Regraft subtree and set pairwise alignments on three branches
    regraft_subtree_and_set_3way_alignments(p2, subtree_edge, target_edge, alignment3way, not sum_out_A);

    // 2. Set branch lengths
    set_lengths_at_location(p2, subtree_edge, target_edge, locations);

    // 3a. Compute substitution likelihood AND alignment probability.
    if (sum_out_A)
    {
        //Resample the alignment and compute the sampling probability.
        auto A_sampling_pr = pr_sum_out_A_tri(p2, a23_constraint, nodes_);
        if (A_sampling_pr)
            Pr[target_edge] = p2.heated_probability() / *A_sampling_pr;
        else
            Pr[target_edge] = 0;
    }
    // 3b. Compute substitution likelihood
    else
    {
        Pr[target_edge] = p2.heated_likelihood();
    }

#ifdef DEBUG_SPR_ALL
    Pr.LLL[target_edge] = p2.heated_likelihood();
#endif

}

/// Compute the probability of pruning b1^t and regraftion at \a locations
///
/// After this routine, likelihood caches and subalignment indices for branches in the
/// non-pruned subtree should reflect the situation where the subtree has been pruned.
///
spr_attachment_probabilities
SPR_search_attachment_points(Parameters P, const tree_edge& subtree_edge, const spr_range& range, const spr_attachment_points& locations,
			     const map<tree_edge,vector<int>>& nodes, bool sum_out_A)
{
    auto peels0 = substitution::total_peel_internal_branches + substitution::total_peel_leaf_branches;

    // The attachment node for the pruned subtree.
    // This node will move around, but we will always peel up to this node to calculate the likelihood.
    int root_node = subtree_edge.node2;
    // Because the attachment node keeps its name, this will stay in effect throughout the likelihood calculations.

    spr_info I(P.t(), subtree_edge, range);

    if (I.n_attachment_branches() == 1) return spr_attachment_probabilities();

    /*----------------------- Initialize likelihood for each attachment point ----------------------- */
    spr_attachment_probabilities Pr;
    if (sum_out_A)
    {
	auto& nodes_ = nodes.at(I.initial_edge);
        P.set_root(root_node);
        auto A_sampling_pr = pr_sum_out_A_tri(P, A23_constraints(P, nodes_, true), nodes_) ;
        if (A_sampling_pr)
            Pr[I.initial_edge] = P.heated_probability() / *A_sampling_pr;
        else
            Pr[I.initial_edge] = 0;
    }
    else
    {
	Pr[I.initial_edge] = P.heated_likelihood();
        P.set_root(root_node);
    }
    assert(P.n_data_partitions() == 0 or P.subst_root() == root_node);

    // Cache probabilities from behind subtree.
    for(int j=0;j<P.n_data_partitions();j++)
    {
        int b0 = P.t().find_branch(subtree_edge);
        P[j].cache(b0);
        P[j].transition_P(b0);
    }

#ifdef DEBUG_SPR_ALL
    Pr.LLL[I.initial_edge] = P.heated_likelihood();
#endif

    std::map<tree_edge, tuple<int,int,int,vector<optional<vector<HMM::bitmask_t>>>>> alignments3way;
    std::map<tree_edge, Parameters> pruned_Ps;
    std::map<tree_dir_edge, Parameters> directed_attached_Ps;

    // 1. Prune subtree and store homology bitpath
    auto alignments3way_initial = prune_subtree_and_get_3way_alignments(P, subtree_edge, I.initial_edge, nodes.at(I.initial_edge), not sum_out_A);

    alignments3way.insert({I.initial_edge, alignments3way_initial});

    pruned_Ps.insert({I.initial_edge, P});

    auto peels1 = substitution::total_peel_internal_branches + substitution::total_peel_leaf_branches;

    // 2. Move to each attachment branch and compute homology bitpath, but don't attch
    for(int i=1;i<I.attachment_branch_pairs.size();i++)
    {
	// Define target branch b2 - pointing away from subtree_edge
	const auto& [prev_target_edge, target_edge, sibling_edge] = I.attachment_branch_pairs[i];

        // Wouldn't BB.prev_edge always be true?
        assert(prev_target_edge->node2 == target_edge.node1);

        // This edge points to the node shared with the prev_target.
        tree_dir_edge prev_target_dir_edge{prev_target_edge->node1, prev_target_edge->node2};
        int attach_node = prev_target_dir_edge.node2;

        // We want to attach on a target_node, but keep the pairwise alignment settings for being on the branch.
        if (not directed_attached_Ps.count(prev_target_dir_edge))
        {
            // Attach the pruned subtree at attach_node
            auto attach_node_P = pruned_Ps.at(*prev_target_edge);
            attach_node_P.reconnect_branch(root_node, root_node, attach_node);

            // Cache probabilities from behind subtree.
            for(int j=0;j<attach_node_P.n_data_partitions();j++)
            {
                int b = attach_node_P.t().find_branch(prev_target_dir_edge);
                attach_node_P[j].cache(b);
                attach_node_P[j].transition_P(b);
            }

            directed_attached_Ps.insert({prev_target_dir_edge, attach_node_P});
        }

        auto prev_p = directed_attached_Ps.at(prev_target_dir_edge);
        prev_p.reconnect_branch(root_node, attach_node, root_node);

        pruned_Ps.insert({target_edge, prev_p});

	auto& p0 = pruned_Ps.at(target_edge);

	auto alignment_3way = move_pruned_subtree(p0, alignments3way.at(*prev_target_edge), subtree_edge, *prev_target_edge, target_edge, sibling_edge, not sum_out_A);

        set_attachment_probability(Pr, locations, subtree_edge, target_edge, pruned_Ps.at(target_edge), nodes, alignment_3way, sum_out_A);

        alignments3way.insert({target_edge, alignment_3way});
    }
    
    for(int i=(int)I.attachment_branch_pairs.size()-1;i>0;i--)
    {
	pruned_Ps.erase(I.attachment_branch_pairs[i].edge);
    }

    if (log_verbose >= 4)
    {
	auto peels2 = substitution::total_peel_internal_branches + substitution::total_peel_leaf_branches;
	std::cerr<<"SPR_search_attachment_points:  range = "<<I.attachment_branch_pairs.size()<<"   branch_peels_to_root = "<<peels1 - peels0<<"   branch_peels_to_other_branches = "<<peels2 - peels1<<std::endl;
    }
    /*----------------------- Initialize likelihood for each attachment point ----------------------- */

    // We had better not let this get changed!
    assert(P.n_data_partitions() == 0 or P.subst_root() == root_node);

    return Pr;
}

/// This just computes nodes and calls sample_tri_multi
bool SPR_accept_or_reject_proposed_tree(Parameters& P, vector<Parameters>& p,
					const vector<log_double_t>& Pr,
					const vector<log_double_t>& PrL,
					const spr_info& I, int C,
					const spr_attachment_points& locations,
					const map<tree_edge, vector<int>>& nodes_for_branch,
					bool sum_out_A
    )
{
    tree_edge E_parent = I.b_parent;

    assert(p.size() == 2);
    assert(p[0].variable_alignment() == p[1].variable_alignment());

    vector<double> relative_proposal_probs = I.relative_proposal_probs();

    //----------------- Generate the Different node lists ---------------//
    vector< vector<int> > nodes(2);
    nodes[0] = nodes_for_branch.at(I.initial_edge);                                 // If p[1].t() == p[0].t() then nodes[0] == nodes[1]
    nodes[1] = nodes_for_branch.at(I.attachment_branch_pairs[C].edge);              // in this formulation.
    bool do_cube = (uniform() < get_setting_or("cube_fraction",0.0));

    optional<int> bandwidth;
    if (setting_exists("simple_bandwidth"))
        bandwidth  = get_setting("simple_bandwidth").as_int64();

    shared_ptr<sample_A3_multi_calculation> tri;
    if (do_cube)
	tri = shared_ptr<sample_A3_multi_calculation>(new sample_cube_multi_calculation(p, nodes, bandwidth));
    else
	tri = shared_ptr<sample_A3_multi_calculation>(new sample_tri_multi_calculation(p, nodes, bandwidth));
    tri->run_dp();

    //--------- Compute PrL2: reverse proposal probabilities ---------//

    vector<log_double_t> PrL2 = PrL;
#ifndef DEBUG_SPR_ALL
    if (P.variable_alignment() and not sum_out_A)
#endif
    {
        p[1].cache_likelihood_branches();
	spr_attachment_probabilities PrB2 = SPR_search_attachment_points(p[1], E_parent, I.range, locations, nodes_for_branch, sum_out_A);
	vector<log_double_t> Pr2 = I.convert_to_vector(PrB2);
    
	if (not P.variable_alignment() or sum_out_A)
	    for(int i=0;i<Pr.size();i++)
		assert(std::abs(Pr[i].log() - Pr2[i].log()) < 1.0e-9);
    
	PrL2 = Pr2;
	for(int i=0;i<PrL2.size();i++)
	    PrL2[i] *= relative_proposal_probs[i];
    }

    //----------------- Specify proposal probabilities -----------------//
    vector<log_double_t> rho(2,1);
    rho[0] = relative_proposal_probs[0] * choose_MH_P(0, C, PrL ); // Pr(proposing 0->C)
    rho[1] = relative_proposal_probs[C] * choose_MH_P(C, 0, PrL2); // Pr(proposing C->0)
  
    tri->set_proposal_probabilities(rho);

    //------------- Accept or reject the proposed topology -------------//
    int C2 = tri->choose();

    // It used to be that, if the alignment is not variable, then we should always accept the proposal.
    // However, now that we're not including the branch-length priors and other priors, this can fail.
    //
    // If the alignment is variable, then we might still choose C2==0 because of the
    // different node orders in sample_tri( ).
    if (not P.variable_alignment())
    {
	// assert(C2 == 1);
    }

    // If the move violates alignment constraints the we can't accept it.
    if (C2 == -1) return false;

    //---------------------- Update P based on choice ------------------//
    P = p[C2];
  
    return (C2 == 1);
}

//
// NOTE: Alignment bandwidth constraints on SPR at the target attachment site.
//
//       To use a simple alignment bandwidth constraint on SPR at the target attachment site we need an initial alignment there.
//       We could use move_pruned_subtree to move the pruned subtree iteratively from edge(j-1) to edge(j) to the attachment site.
//       This is necessary in order to get an initial alignment at the regrafting location.
//       Such an alignment could possibly be used to create a bandwidth around the current path.
//       We would need to check what bandwidth would be created around the initial alignment from the reverse proposal.
//
//       Currently move_pruned_subtree extends the homology of characters at the initial edge so that they are present at the target edge.
//       To attach at the final location, it seems like extending vanished homology would be wrong.
//       So we would need an alternative method that unalign characters in the subtree that are not homologous to anything at the attachment point.
//       I expect that extending the homology would result in higher likelihoods at the target edge.
//       But it does not seem reversible.
//
//       So there are three options: none, extending vanish homology, unalign vanishing homology.
//       Model with optional<bool>?
//
//       Ideally we would use a full probability to assess attaching on each edge.
//       Ideally we would really construct a new alignment on each edge.
//       Constructing a new alignment would allow using the probability...
//       If we used A5-2D, it would simplify a lot of things, but might be too slow...
//
//       PROBLEM: Sometimes the branch selected by SPR_search_all seems to be rejected, even when the alignment is fixed.
//                How can that be?
//
//       - BDR 05/18/2025
//
void spr_to_index(Parameters& P, spr_info& I, int C, const vector<int>& nodes0)
{
    auto target_edge = I.attachment_branch_pairs[C].edge;
    auto& subtree_edge = I.b_parent;

    // 1. Record a map from each edge to its prev_edge.
    std::map<tree_edge, tree_edge> prev_edges;
    std::map<tree_edge, optional<tree_edge>> sibling_edges;
    for(auto& [prev, edge, sibling]: I.attachment_branch_pairs)
        if (prev)
        {
            prev_edges.insert({edge,*prev});
            sibling_edges.insert({edge,sibling});
        }

    // 2. Record list of edges from target edge to initial edge
    vector<tree_edge> edges;
    edges.push_back(target_edge);

    while(prev_edges.count(edges.back()))
        edges.push_back(prev_edges.at(edges.back()));

    // 3. Reverse the list of edges, so that it goes from initial edge to target_edge.
    std::reverse(edges.begin(), edges.end());

    // 4. Prune subtree and store homology bitpath
    vector<tuple<int,int,int,vector<optional<vector<HMM::bitmask_t>>>>> alignments3way;
    alignments3way.reserve(I.attachment_branch_pairs.size());
    alignments3way.push_back(prune_subtree_and_get_3way_alignments(P, subtree_edge, I.initial_edge, nodes0, false));

//  NOTE: This was for preliminary work to get an alignment bandwidth at the new attachment site...
//    // 5. Move the pruned subtree iteratively from edge (j-1) -> edge (j) until we get to target_edge.
//    for(int j=1;j<edges.size();j++)
//	alignments3way.push_back( move_pruned_subtree(P, alignments3way[j-1], subtree_edge, edges[j-1], edges[j], sibling_edges.at(edges[j]), false) );

    // 6. Finally, regraft the subtree to the target edge and set branch lengths
    regraft_subtree_and_set_3way_alignments(P, subtree_edge, target_edge, alignments3way.back(), false);
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

bool sample_SPR_search_one(Parameters& P,MoveStats& Stats, const tree_edge& subtree_edge, const spr_range& range, bool sum_out_A = false)
{
    const int bins = 6;

    if (P.t().degree(subtree_edge.node2) != 3) return false;

    if (P.t().has_node_times())
    {
        int b = P.t().find_branch(subtree_edge);

        // We can only prune subtrees that don't include the root.
        if (P.t().away_from_root(b)) return false;
    }

    sum_out_A = sum_out_A or (uniform() < get_setting_or("spr_sum_out_A",0.0));

    // 1. Always peel up to attachment node to calculate the likelihood.
    // * The attachment node keeps its name as we regraft on different branches.
    P.set_root( subtree_edge.node2 );

    // 2. Compute the fractions along each edge that we will attach at.
    spr_attachment_points locations = get_spr_attachment_points(P.t(), subtree_edge, range);

    // 3. Construct data structure with information for incremental regrafting.
    spr_info I(P.t(), subtree_edge, range);

    // 4. Compute the nodes for each edge.
    map<tree_edge,vector<int>> nodes;
    for(auto& E: I.attachment_branch_pairs)
    {
	vector<int> edge_nodes { subtree_edge.node2, subtree_edge.node1, E.edge.node1, E.edge.node2 };
	if (uniform() < 0.5)
	    std::swap(edge_nodes[2], edge_nodes[3]);
	nodes[E.edge] = edge_nodes;
    }
    const auto& nodes0 = nodes[I.initial_edge];

    // 5. Compute total lengths for each of the possible attachment branches
    vector<double> relative_proposal_probs = I.relative_proposal_probs();

    if (I.n_attachment_branches() == 1) return false;

    // 6. Compute the probabilities for attaching on each branch.
    spr_attachment_probabilities PrB;
    try
    {
        P.cache_likelihood_branches();
	PrB = SPR_search_attachment_points(P, subtree_edge, range, locations, nodes, sum_out_A);
    }
    catch (choose_exception<log_double_t>& c)
    {
	// FIXME: what if only SOME of the attachment points have NaN/Inf/Inf?
	std::cerr<<"sample_SPR_search_one: caught choose_exception:\n";
	std::cerr<<c.what();
	std::cerr<<"continuing on with P.probability() = "<<P.probability()<<".\n";
	return false;
    }


    vector<log_double_t> Pr = I.convert_to_vector(PrB);
#ifdef DEBUG_SPR_ALL
    vector<log_double_t> LLL = I.convert_to_vector(PrB.LLL);
#endif

    // 7. Scale the attachment probabilities by 1/(1/L), since we don't have to propose an attachment point for the starting branch.
    vector<log_double_t> PrL = Pr;
    for(int i=0;i<PrL.size();i++)
	PrL[i] *= relative_proposal_probs[i];


    // 8. Choose the attachment branch.
    int C = -1;
    try {
	C = choose_MH(0,PrL);
    }
    catch (choose_exception<log_double_t>& c)
    {
	c.prepend(__PRETTY_FUNCTION__);
	throw c;
    }

    // 9. Compute the proposed tree by attaching to the selected branch.
    vector<Parameters> p(2,P);
    if (C != 0)
    {
	auto target_edge = I.attachment_branch_pairs[C].edge;
	spr_to_index(p[1], I, C, nodes0);
	set_lengths_at_location(p[1], subtree_edge, target_edge, locations);
    }

#ifdef DEBUG_SPR_ALL
    // The likelihood for attaching at a particular place should not depend on the initial attachment point.
    log_double_t L_1 = p[1].heated_likelihood();
    auto diff = L_1.log() - LLL[C].log();
    std::cerr<<"re-attachment diff = "<<diff<<std::endl;

    // FIXME - If we have an IModel, then re-attaching means that we unalign characters instead of extending their alignment
    //         to keep them aligned.
    bool has_imodel = true;
    for(int i=0;i<P.n_data_partitions();i++)
	if (not P[i].alignment_is_random())
	    has_imodel = false;
    if (not has_imodel)
	assert(std::abs(diff) < 1.0e-9);
#endif

    // 10. Accept or reject the proposed tree by integrating out the alignment.
    //     (This should always succeed, if the alignment is fixed.)
    bool accepted = true;
    try
    {
	if (C > 0)
	    accepted = SPR_accept_or_reject_proposed_tree(P, p, Pr, PrL, I, C, locations, nodes, sum_out_A);
    }
    catch (std::bad_alloc&) {
	std::cerr<<"Allocation failed in sample_try_multi (in SPR_search_one)!  Proceeding."<<std::endl;
	return false;
    }

    // 11. Record SPR move statistics.
    MCMC::Result result = SPR_stats(p[0].t(), p[1].t(), accepted, bins, subtree_edge);
    double LM = P.t().branch_length(P.t().find_branch(subtree_edge));
    if (sum_out_A)
	SPR_inc(Stats, result, "SPR (all-sum)", LM);
    else
	SPR_inc(Stats, result, "SPR (all)", LM);

    // Return true if we accept a DIFFERENT tree.
    return ((C != 0) and accepted);
}

bool sample_SPR_search_one(Parameters& P,MoveStats& Stats, const tree_edge& subtree_edge, bool sum_out_A = false)
{
    return sample_SPR_search_one(P, Stats, subtree_edge, spr_full_range(P.t(), subtree_edge), sum_out_A);
}

void sample_SPR_all(owned_ptr<context>& P,MoveStats& Stats) 
{
    Parameters& PP = *P.as<Parameters>();
    int n = n_SPR_moves(PP);

    // double p = get_setting_or("SPR_slice_fraction",-0.25);

    for(int i=0;i<n;i++) 
    {
        if (log_verbose >= 4)  std::cerr<<"    sample_SPR_all: "<<i+1<<"/"<<n<<"\n";

        // Choose a directed branch to prune and regraft -- pointing away from the pruned subtree.
        int b1 = choose_subtree_branch_uniform2(PP.t());

        sample_SPR_search_one(PP, Stats, PP.t().edge(b1));
    }
}

void sample_SPR_search_all(owned_ptr<context>& P,MoveStats& Stats, bool sum_out_A) 
{
    auto branches = P.as<Parameters>()->t().directed_branches();
    for(int b: branches)
    {
	slice_sample_branch_length(P,Stats,b);
	auto& PP = *P.as<Parameters>();
	bool changed = sample_SPR_search_one(PP, Stats, PP.t().edge(b), sum_out_A);
	if (not changed) three_way_topology_sample(P,Stats,b);
	slice_sample_branch_length(P,Stats,b);
    }
}

void sample_SPR_search_all(owned_ptr<context>& P,MoveStats& Stats) 
{
    sample_SPR_search_all(P, Stats, false);
}

void sample_SPR_A_search_all(owned_ptr<context>& P,MoveStats& Stats) 
{
    sample_SPR_search_all(P, Stats, true);
}

vector<int> path_to(const TreeInterface& T,int n1, int n2) 
{
    assert(T.is_leaf_node(n1));
    assert(T.is_leaf_node(n2));
    assert(n1 != n2);

    vector<int> path; 
    path.push_back(n1);
    path.push_back(T.neighbor(n1,0));

    while(path.back() != n2) 
    {
	int b = T.find_branch(path[path.size()-2], path[path.size()-1]);

	for(int i: T.branches_after(b))
	{
	    if (T.partition(i).contains(n2)) {
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
    n1 = T.leaf_nodes()[n1];
    n2 = T.leaf_nodes()[n2];

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

void sample_SPR_flat(owned_ptr<context>& P,MoveStats& Stats) 
{
    Parameters& PP = *P.as<Parameters>();
    int n = n_SPR_moves(PP);

    //  double p = get_setting_or("SPR_slice_fraction",-0.25);

    for(int i=0;i<n;i++) 
    {
	int b1 = choose_subtree_branch_uniform(PP.t());

	sample_SPR_flat_one(P, Stats, b1);
    }
}

void sample_SPR_nodes(owned_ptr<context>& P,MoveStats& Stats) 
{
    Parameters& PP = *P.as<Parameters>();

    // Allow turning off these moves.
    if (not get_setting_or("SPR-jump",true)) return;

    int n = n_SPR_moves(PP);

    double p = get_setting_or("SPR_slice_fraction",-0.25);

    for(int i=0;i<n;i++) {

	int b1=-1, b2=-1;
	choose_subtree_branch_nodes(PP.t(), b1, b2);

	double L = PP.t().branch_length(b1);

	if (not PP.variable_alignment() and uniform()< p) {
	    MCMC::Result result = sample_SPR(*P.as<Parameters>(),b1,b2,true);
	    SPR_inc(Stats,result,"SPR (path/slice)", L);
	}
	else {
	    MCMC::Result result = sample_SPR(*P.as<Parameters>(),b1,b2);
	    SPR_inc(Stats,result,"SPR (path)", L);
	}
    }
}
