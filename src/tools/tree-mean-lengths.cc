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
#include "tree/tree.hh"
#include "tree/sequencetree.hh"
#include "tree/tree-util.hh"
#include "tree-dist.hh"
#include "util/myexception.hh"
#include "util/cmdline.hh"

#include <boost/program_options.hpp>

extern int log_verbose;

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;

using std::optional;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    // named options
    options_description all("Allowed options");
    all.add_options()
	("help,h", "produce help message")
	("tree,T", value<string>(),"unrooted query tree")
	("files",value<vector<string> >()->composing(),"tree samples to examine")
	("skip,s",value<int>()->default_value(0),"number of tree samples to skip")
	("until,u",value<int>(),"Read until this number of trees.")
	("ignore",value<string>(),"Comma-separated taxa to remove")
	("simple","Report conditional branch means only (overrides other output modes)")
	("subsample,x",value<int>()->default_value(1),"factor by which to sub-sample")
	("var","report standard deviation of branch lengths instead of mean")
	("no-node-lengths","Do not distribute node lengths onto branches")
	("safe","Print topology without lengths if requested estimates are unavailable")
	("show-node-lengths","Report conditional branch and node means separately for draw-tree")
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
	cout<<"Usage: tree-mean-lengths --tree <tree> [OPTIONS] <file1> [<file2> ...]\n\n";
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
    vector<int> multifurcations; // Only nodes whose lengths are requested; empty in branch-only modes.
    vector<double> node_sum;
    vector<size_t> node_count;
    size_t n_samples = 0;

    // Prepare the immutable reference index once for all input files.
    length_accumulator(const SequenceTree& query, bool include_nodes)
        : Q(query), tip_rank(Q.n_leaves(), -1), interval_branch(Q.n_leaves(), -1),
          branch_sum(Q.n_branches(), 0), branch_sum_squares(Q.n_branches(), 0),
          branch_count(Q.n_branches(), 0), node_sum(Q.n_nodes(), 0), node_count(Q.n_nodes(), 0)
    {
        if (include_nodes)
            for (int n = 0; n < Q.n_nodes(); ++n)
                if (Q.node(n).degree() > 3) multifurcations.push_back(n);
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

    // Cut shared edges conceptually, assigning components in parent-before-child order.
    // Each unmatched edge stays in its parent's component and contributes its length once.
    // If every incident split of a consensus node occurs, their inward endpoints enclose
    // exactly its resolving component. No other consensus edge lies inside that component:
    // every other edge is beyond one of the incident boundaries. Conflicts outside these
    // boundaries are irrelevant. Eligibility is tested within one sample, never by pooling
    // incident matches across samples. A zero-length component is still an observation.
    void accumulate_nodes(const SequenceTree& T, const vector<const_branchview>& order,
                          const vector<int>& matches)
    {
        vector<bool> matched(T.n_branches(), false);
        for (int b = 0; b < Q.n_branches(); ++b)
            if (matches[b] != -1)
                matched[T.directed_branch(matches[b]).undirected_name()] = true;

        vector<int> component(T.n_nodes(), 0);
        vector<double> component_length(1, 0);
        for (auto b: order)
        {
            int c = component[b.source()];
            if (matched[b.undirected_name()])
            {
                c = component_length.size();
                component_length.push_back(0);
            }
            else
                component_length[c] += b.length();
            component[b.target()] = c;
        }

        for (int n: multifurcations)
        {
            bool complete = true;
            for (auto b = Q.node(n).branches_in(); b; ++b)
                if (matches[(*b).name()] == -1)
                {
                    complete = false;
                    break;
                }
            if (not complete) continue;

            int incoming = (*Q.node(n).branches_in()).name();
            int c = component[T.directed_branch(matches[incoming]).target()];
            for (auto b = Q.node(n).branches_in(); b; ++b)
                assert(component[T.directed_branch(matches[(*b).name()]).target()] == c);
            node_sum[n] += component_length[c];
            ++node_count[n];
        }
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
        if (not multifurcations.empty()) accumulate_nodes(T, order, matches);
    }

    // Validate before writing any output. Sums stay unchanged, so repeated checks cannot
    // divide already-finalized values and all files share the same observation counts.
    void check_estimates() const
    {
        if (n_samples == 0) throw myexception()<<"No trees were read in!";
        for (int b = 0; b < Q.n_branches(); ++b)
            if (branch_count[b] == 0)
                throw myexception()<<"No samples contain query branch "<<b<<".";
        for (int n: multifurcations)
            if (node_count[n] == 0)
                throw myexception()<<"No samples contain all branches incident to query node "<<n<<".";
    }

    double branch_mean(int b) const { return branch_sum[b] / branch_count[b]; }
    double node_mean(int n) const { return node_count[n] ? node_sum[n] / node_count[n] : 0; }

    // Preserve the population SD convention; subtraction may give a tiny negative
    // variance through roundoff even though the mathematical variance is nonnegative.
    double branch_sd(int b) const
    {
        double mean = branch_mean(b);
        return std::sqrt(std::max(0.0, branch_sum_squares[b] / branch_count[b] - mean * mean));
    }
};

} // namespace



int main(int argc, char* argv[])
{
    try
    {
        variables_map args = parse_cmd_line(argc, argv);
        int skip = args["skip"].as<int>();
        optional<int> last;
        if (args.count("until")) last = args["until"].as<int>();
        int subsample = args["subsample"].as<int>();
        vector<string> prune = get_string_list(args, "ignore");
        const auto files = args["files"].as<vector<string>>();

        RootedSequenceTree query = load_T(args);
        // Match the sample reader's unrooted interpretation: the two edges at an
        // artificial degree-two root represent one split, not two independent branches.
        if (query.root().degree() == 2)
            query.remove_node_from_branch(query.root());
        SequenceTree Q = query;
        standardize(Q);

        // Preserve output precedence: simple means first, then SDs, then separate node
        // lengths, then branch-only means or the default redistribution of node lengths.
        const bool simple = args.count("simple");
        const bool deviation = not simple and args.count("var");
        const bool show_nodes = not simple and not deviation and args.count("show-node-lengths");
        const bool include_nodes = not simple and not deviation
                                   and (show_nodes or not args.count("no-node-lengths"));
        length_accumulator A(Q, include_nodes);
        try
        {
            for (const auto& file: files)
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

        if (log_verbose)
        {
            cerr<<"Read "<<A.n_samples<<" sampled trees."<<endl;
            for (int b = 0; b < Q.n_branches(); ++b)
                cerr<<"Query branch "<<b<<": "<<A.branch_count[b]<<" contributing samples."<<endl;
            for (int n: A.multifurcations)
                cerr<<"Query node "<<n<<": "<<A.node_count[n]<<" contributing samples."<<endl;
        }

        // Each estimate has its own denominator; do not condition on whole-tree agreement.
        // Q remains unchanged until every requested estimate has passed validation.
        for (int b = 0; b < Q.n_branches(); ++b)
            Q.branch(b).set_length(deviation ? A.branch_sd(b) : A.branch_mean(b));

        if (include_nodes and not show_nodes)
            for (int n: A.multifurcations)
            {
                double share = A.node_mean(n) / Q.node(n).degree();
                for (auto b = Q.node(n).branches_out(); b; ++b)
                    (*b).set_length((*b).length() + share);
            }

        cout<<Q<<endl;
        if (show_nodes)
            for (int n: A.multifurcations)
                if (A.node_mean(n) > 0)
                {
                    cout<<"node "<<A.node_mean(n)<<endl;
                    int b = (*Q.node(n).branches_in()).name();
                    cout<<partition_from_branch(Q, b)<<endl;
                }
    }
    catch (std::exception& e)
    {
        std::cerr<<"tree-mean-lengths: Error! "<<e.what()<<endl;
        return 1;
    }
    return 0;
}
