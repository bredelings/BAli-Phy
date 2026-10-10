/*
  Copyright (C) 2005-2009 Benjamin Redelings

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

#include <algorithm>
#include <cmath>
#include <limits>
#include <iostream>
#include <list>
#include <utility>
#include "tree/tree.hh"
#include "tree/sequencetree.hh"
#include "tree/tree-util.hh"
#include "tree-dist.hh"
#include "util/myexception.hh"
#include "util/cmdline.hh"

#include <boost/program_options.hpp>

extern int log_verbose;

using boost::dynamic_bitset;

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;
using std::list;
using std::valarray;
using std::pair;

using std::optional;

// mark nodes in T according to what node of Q they map to
vector<int> get_nodes_map(const SequenceTree& Q,const SequenceTree& T,
			  const vector<int>& branches_map)
{
    assert(branches_map.size() == Q.n_branches() * 2);

    vector<int> nodes_map(T.n_nodes(),-1);

    // map nodes from T -> Q that are in both trees
    for(int b=0;b<Q.n_branches();b++)
    {
	int Q_source = Q.branch(b).source();
	int Q_target = Q.branch(b).target();

	int b2 = branches_map[b];

	int T_source = T.directed_branch(b2).source();
	int T_target = T.directed_branch(b2).target();

	if (nodes_map[T_source] == -1)
	    nodes_map[T_source] = Q_source;
	else
	    assert(nodes_map[T_source] == Q_source);

	if (nodes_map[T_target] == -1)
	    nodes_map[T_target] = Q_target;
	else
	    assert(nodes_map[T_target] == Q_target);
    }

    // map the rest of the nodes from T -> Q
    for(int i=Q.n_leaves();i<Q.n_nodes();i++) 
    {
	unsigned D = Q.node(i).degree();
	if (D <= 3) continue;

	// get a branch of Q pointing into the node
	const_branchview outside = *(Q.node(i).branches_in());
	// get a branch of T pointing into the node
	outside = T.directed_branch(branches_map[outside.name()]);

	list<const_branchview> branches;
	typedef list<const_branchview>::iterator list_iterator;
	append(outside.branches_after(),branches);
	for(list_iterator b = branches.begin() ; b != branches.end();)
	{
	    int node = (*b).target();
	    if (nodes_map[node] == -1)
		nodes_map[node] = i;

	    if (nodes_map[node] == i) {
		append((*b).branches_after(),branches);
		b++;
	    }
	    else {
		list_iterator prev = b;
		b++;
		branches.erase(prev);
	    }
	}
	assert(branches.size() == D-3);
    }

    for(int i=0;i<nodes_map.size();i++)
	assert(nodes_map[i] != -1);

    return nodes_map;
}


bool update_lengths(const SequenceTree& Q,const SequenceTree& T,
		    valarray<double>& branch_lengths, 
		    valarray<double>& branch_lengths_squared, 
		    valarray<double>& node_lengths)
{
    // map branches from Q -> T

    vector<int> branches_map;
    if (T.n_nodes() == 2)
	branches_map = { 0 };
    else
	branches_map = extends_map(T,Q);

    if (not branches_map.size())
	return false;

    // incorporate lengths of branches that map to Q
    for(int b=0;b<Q.n_branches();b++)
    {
	int b2 = branches_map[b];
	double L = T.directed_branch(b2).length();
	branch_lengths[b] += L;
	branch_lengths_squared[b] += L*L;
    }

    // incorprate lengths of branches that map to nodes in Q
    if (T.n_nodes() > 2)
    {
	// map nodes from T -> Q
	vector<int> nodes_map = get_nodes_map(Q,T,branches_map);

	for(int i=T.n_leafbranches();i<T.n_branches();i++) 
	{
	    const_branchview b = T.branch(i);
	    int n1 = nodes_map[b.source()];
	    int n2 = nodes_map[b.target()];

	    if (n1 == n2)
		node_lengths[n1] += T.branch(i).length();
	}
    }

    return true;
}


variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    // named options
    options_description all("Allowed options");
    all.add_options()
	("help,h", "produce help message")
	("tree,T", value<string>(),"tree to re-root")
	("files",value<vector<string> >()->composing(),"tree samples to examine")
	("skip,s",value<int>()->default_value(0),"number of tree samples to skip")
	("until,u",value<int>(),"Read until this number of trees.")
	("ignore",value<string>(),"Comma-separated taxa to remove")
	("simple","Ignore all branches not in the query tree")
	("subsample,x",value<int>()->default_value(1),"factor by which to sub-sample")
	("var","report standard deviation of branch lengths instead of mean")
	("no-node-lengths","ignore branches not in the specified topology")
	("safe","Don't die if no trees match the topology")
	("show-node-lengths","Output special format")
	("verbose,V","Output more log messages on stderr.")
	;

    // positional options
    positional_options_description p;
    p.add("files", -1);
  
    variables_map args;     
    store(command_line_parser(argc, argv).
	  options(all).positional(p).run(), args);
    notify(args);

    if (args.count("help")) {
	cout<<"Compute the mean lengths for branches in the given topology.\n\n";
	cout<<"Usage: tree-mean-lengths [OPTIONS] <tree> <file1> [<file2> ...]\n\n";
	cout<<all<<"\n";
	exit(0);
    }

    if (args.count("verbose")) log_verbose = 1;

    if (not args.count("tree"))
	throw myexception()<<"No tree topology specified.\n\nTry `"<<argv[0]<<" --help' for more information.";

    if (not args.count("files") or args["files"].as<vector<string>>().empty())
	throw myexception()<<"No filenames for trees specified.\n\nTry `"<<argv[0]<<" --help' for more information.";

    return args;
}


struct accum_branch_lengths_same_topology: public accumulator<SequenceTree>
{
    int n_samples;
    int n_matches;

    SequenceTree Q;

    valarray<double> m1;
    valarray<double> m2;
    valarray<double> n1;

    void operator()(const SequenceTree&);

    void finalize() 
	{
	    if (n_samples == 0)
		throw myexception()<<"No trees were read in!";
  
	    if (n_matches == 0)
		throw myexception()<<"No trees matched the specified topology!";

	    m1 /= n_matches;
	    m2 /= n_matches;
	    n1 /= n_matches;

	    m2 -= m1*m1;
	    m2 = sqrt(m2);
	}

    accum_branch_lengths_same_topology(const SequenceTree& T)
	:
	n_samples(0),
	n_matches(0),
	Q(T),
	m1(0.0, Q.n_branches()),
	m2(0.0, Q.n_branches()),
	n1(0.0, Q.n_nodes())
	{}
};

void accum_branch_lengths_same_topology::operator()(const SequenceTree& T)
{
    if (not n_samples) {
	if (T.n_leaves() != Q.n_leaves())
	    throw myexception()<<"Query tree has "<<Q.n_leaves()<<" leaves, but tree sample has "<<T.n_leaves()<<" leaves.";
    }

    n_samples++;
    if (update_lengths(Q,T,m1,m2,n1))
	n_matches++;
}


namespace {

// A subtree is summarized by the extremes and number of its ranked descendant tips.
// The empty summary is also used for the reference tip, which lies outside every clade.
struct leaf_interval
{
    int first = std::numeric_limits<int>::max();
    int last = -1;
    int count = 0;

    // Children have disjoint tip sets, so their counts add without duplicate removal.
    void include(const leaf_interval& child)
    {
        first = std::min(first, child.first);
        last = std::max(last, child.last);
        count += child.count;
    }
};

// Return edges pointing away from start in depth-first, parent-before-child order.
// An explicit stack avoids recursion depth limits on long, ladder-shaped trees.
vector<const_branchview> depth_first_branches(const Tree& T, int start)
{
    vector<const_branchview> order;
    vector<const_branchview> pending;
    order.reserve(T.n_branches());
    if (T.n_branches() == 0) return order;
    append(T.node(start).branches_out(), pending);
    while (not pending.empty())
    {
        auto b = pending.back();
        pending.pop_back();
        order.push_back(b);
        append(b.branches_after(), pending);
    }
    return order;
}

// Aggregate tip ranks toward the traversal's start. Reversing preorder visits every
// child before its parent; the same operation works for binary trees and polytomies.
vector<leaf_interval> summarize_subtrees(const Tree& T, const vector<const_branchview>& order,
                                       const vector<int>& tip_rank)
{
    vector<leaf_interval> intervals(T.n_nodes());
    for (int tip = 0; tip < T.n_leaves(); ++tip)
        if (tip_rank[tip] >= 0)
            intervals[tip] = {tip_rank[tip], tip_rank[tip], 1};
    for (auto i = order.rbegin(); i != order.rend(); ++i)
        intervals[i->source()].include(intervals[i->target()]);
    return intervals;
}

// Day's algorithm (1985, doi:10.1007/BF01908061) matches splits without bitsets.
// Orient both unrooted trees away from the same tip (tip 0); an edge then represents
// the descendant clade on its other side. The reader has already aligned tip identities.
//
// Number reference tips in DFS order. Every reference clade is a consecutive interval.
// Store an internal edge at its interval's right endpoint if it is the first child,
// otherwise at its left endpoint. These slots are distinct: along a chain sharing a
// left endpoint only the highest node can be a non-first child, and along a chain
// sharing a right endpoint only the lowest node can be a first child. A first child's
// right endpoint cannot equal a non-first child's left endpoint. Singleton clades
// (pendant edges) are matched separately by tip identity.
//
// For a sampled clade, min/max alone are insufficient: count == max-min+1 proves it
// contains the entire interval. Comparing both bounds at both endpoint slots then
// finds the exact reference edge. Sibling order is arbitrary but must agree with the
// DFS used to assign ranks. No sorting, probabilistic hashes, or tie-breaking is needed.
// Index construction and each comparison use O(n) time and O(n) memory, excluding input.
struct length_accumulator: public accumulator<SequenceTree>
{
    const SequenceTree& Q;
    vector<int> tip_rank;
    vector<leaf_interval> reference_intervals;
    vector<int> interval_branch; // Directed reference branch, or -1 for an empty slot.
    vector<double> branch_sum;
    vector<double> branch_sum_squares;
    vector<size_t> branch_count;
    size_t n_samples = 0;

    // Prepare the immutable reference index once for all input files.
    explicit length_accumulator(const SequenceTree& query)
        : Q(query), tip_rank(Q.n_leaves(), -1), interval_branch(Q.n_leaves(), -1),
          branch_sum(Q.n_branches(), 0), branch_sum_squares(Q.n_branches(), 0),
          branch_count(Q.n_branches(), 0)
    {
        const auto order = depth_first_branches(Q, 0);
        vector<int> first_child(Q.n_nodes(), -1);
        int rank = 0;
        for (auto b: order)
        {
            if (first_child[b.source()] == -1) first_child[b.source()] = b.name();
            if (b.target().is_leaf_node()) tip_rank[b.target()] = rank++;
        }
        reference_intervals = summarize_subtrees(Q, order, tip_rank);
        for (auto b: order)
        {
            if (b.is_leaf_branch()) continue;
            const auto& interval = reference_intervals[b.target()];
            int slot = first_child[b.source()] == b.name() ? interval.last : interval.first;
            assert(interval_branch[slot] == -1);
            interval_branch[slot] = b.name();
        }
    }

    // Find an exact clade match, checking contiguity before accessing endpoint slots.
    int find_interval(const leaf_interval& interval) const
    {
        if (interval.count != interval.last - interval.first + 1) return -1;
        for (int slot: {interval.first, interval.last})
        {
            int b = interval_branch[slot];
            if (b == -1) continue;
            const auto& reference = reference_intervals[Q.directed_branch(b).target()];
            if (reference.first == interval.first and reference.last == interval.last) return b;
        }
        return -1;
    }

    // Map both directions of each shared edge. In particular, mapped incoming edges
    // retain the endpoint facing a consensus node, which node-length accumulation needs.
    vector<int> match_branches(const SequenceTree& T, const vector<const_branchview>& order) const
    {
        const auto intervals = summarize_subtrees(T, order, tip_rank);
        vector<int> matches(2 * Q.n_branches(), -1);
        for (auto b: order)
        {
            int q;
            if (b.source().is_leaf_node()) // The reference-tip edge, including the two-tip case.
                q = (*Q.node(0).branches_out()).name();
            else if (b.target().is_leaf_node())
                q = (*Q.node(b.target()).branches_in()).name();
            else
                q = find_interval(intervals[b.target()]);
            if (q == -1) continue;
            assert(matches[q] == -1);
            matches[q] = b.name();
            matches[Q.directed_branch(q).reverse().name()] = b.reverse().name();
        }
        return matches;
    }

    // Each branch contributes whenever its own split occurs, regardless of conflicts elsewhere.
    void operator()(const SequenceTree& T) override
    {
        if (T.n_leaves() != Q.n_leaves())
            throw myexception()<<"Query tree has "<<Q.n_leaves()<<" leaves, but tree sample has "
                               <<T.n_leaves()<<" leaves.";
        ++n_samples;
        const auto order = depth_first_branches(T, 0);
        const auto matches = match_branches(T, order);
        for (int b = 0; b < Q.n_branches(); ++b)
        {
            if (matches[b] == -1) continue;
            double length = T.directed_branch(matches[b]).length();
            branch_sum[b] += length;
            branch_sum_squares[b] += length * length;
            ++branch_count[b];
        }
    }

    // Validate before writing any output. Sums stay unchanged, so repeated checks cannot
    // divide already-finalized values and all files share the same observation counts.
    void check_estimates() const
    {
        if (n_samples == 0) throw myexception()<<"No trees were read in!";
        for (int b = 0; b < Q.n_branches(); ++b)
            if (branch_count[b] == 0)
                throw myexception()<<"No samples contain query branch "<<b<<".";
    }

    double branch_mean(int b) const { return branch_sum[b] / branch_count[b]; }

    // Preserve the population SD convention; subtraction may give a tiny negative
    // variance through roundoff even though the mathematical variance is nonnegative.
    double branch_sd(int b) const
    {
        double mean = branch_mean(b);
        return std::sqrt(std::max(0.0, branch_sum_squares[b] / branch_count[b] - mean * mean));
    }
};

} // namespace



int main(int argc,char* argv[]) 
{ 
    try {
	//----------- Parse command line  ----------//
	variables_map args = parse_cmd_line(argc,argv);

	int skip = args["skip"].as<int>();

	optional<int> last;
	if (args.count("until"))
	    last = args["until"].as<int>();

	int subsample = args["subsample"].as<int>();

	vector<string> prune = get_string_list(args, "ignore");
	vector<string> files;
	if (args.count("files"))
	    files = args["files"].as<vector<string>>();

	//----------- Read the topology -----------//
	RootedSequenceTree query = load_T(args);
	// Match the sample reader's unrooted interpretation: the two edges at an
	// artificial degree-two root represent one split, not two independent branches.
	if (query.root().degree() == 2)
	    query.remove_node_from_branch(query.root());
	SequenceTree Q = query;
	standardize(Q);
	const int B = Q.n_branches();
	const int N = Q.n_nodes();
	vector<double> bf(B);
	for(int b=0;b<bf.size();b++)
	    bf[b] = Q.branch(b).length();

	//-------- Read in the tree samples --------//
        if (args.count("simple"))
        {
            length_accumulator A(Q);
            try
            {
                for (auto& file: files)
                {
                    istream_or_ifstream f(std::cin, "-", file);
                    scan_trees(f, skip, last, subsample, prune, Q.get_leaf_labels(), A);
                }
                A.check_estimates();
            }
            catch (std::exception& e)
            {
                if (not args.count("safe")) throw;
                std::cerr<<"tree-mean-lengths: Error! "<<e.what()<<endl;
                cout<<Q.write(false)<<endl;
                return 0;
            }
            for (int b = 0; b < B; ++b) Q.branch(b).set_length(A.branch_mean(b));
            cout<<Q<<endl;
            return 0;
        }

	accum_branch_lengths_same_topology A(Q);

	try {
            for(auto& file: files)
            {
                istream_or_ifstream f(std::cin,"-",file);
                scan_trees(f,skip,last,subsample,prune,Q.get_leaf_labels(), A);
            }
            A.finalize();
	}
	catch (std::exception& e) 
	{
	    if (args.count("safe"))
		cout<<Q.write(false)<<endl;
	    std::cerr<<"tree-mean-lengths: Error! "<<e.what()<<endl;
	    exit(0);
	}

	if (log_verbose) std::cerr<<A.n_matches<<" out of "<<A.n_samples<<" trees matched the topology";
	if (log_verbose) std::cerr<<" ("<<double(A.n_matches)/A.n_samples*100<<"%)"<<std::endl;

	//------- Merge lengths and topology -------//
	if (args.count("var")) {
	    for(int b=0;b<B;b++)
		Q.branch(b).set_length(A.m2[b]);
	    cout<<Q;
	    exit(0);
	}
	else {
	    for(int b=0;b<B;b++)
		Q.branch(b).set_length(A.m1[b]);

	    if (not args.count("no-node-lengths") and 
		not args.count("show-node-lengths")) {
		for(int n=0;n<N;n++) {
		    int degree = Q.node(n).neighbors().size();
		    for(out_edges_iterator b = Q.node(n).branches_out();b;b++)
			(*b).set_length((*b).length() + A.n1[n]/degree);
		}
	    }

	    //------- Print Tree and branch lengths -------//
	    cout<<Q<<endl;

	    //------------ Print node lengths -------------//
	    if (args.count("show-node-lengths"))
		for(int n=0;n<Q.n_nodes();n++) {
		    if (A.n1[n] > 0) {
			cout<<"node "<<A.n1[n]<<endl;
			int b = (*Q.node(n).branches_in()).name();
			cout<<partition_from_branch(Q,b)<<endl;
		    }
		}

	}
    }
    catch (std::exception& e) {
	std::cerr<<"tree-mean-lengths: Error! "<<e.what()<<endl;
	exit(1);
    }
    return 0;
}
