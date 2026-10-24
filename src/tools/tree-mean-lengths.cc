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
#include <filesystem>
#include <memory>
#include "tree/tree.hh"
#include "tree/sequencetree.hh"
#include "tree/tree-util.hh"
#include "tree-dist.hh"
#include "util/myexception.hh"
#include "util/cmdline.hh"

#include <CLI/CLI.hpp>

extern int log_verbose;

using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;

using std::optional;

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

// Load one unrooted query, using the same leaf ordering for every estimator in a batch.
SequenceTree load_query(const string& filename)
{
    auto tree = load_tree_from_file(filename);
    // The two edges at an artificial degree-two root represent one unrooted split.
    if (tree.root().degree() == 2) tree.remove_node_from_branch(tree.root());
    SequenceTree query = tree;
    standardize(query);
    return query;
}

// A query owns the tree referenced by its accumulator. Keep records at stable addresses
// with unique_ptr; copying a record would leave the accumulator referring to the old tree.
struct query_record
{
    string input, output;
    SequenceTree tree;
    length_accumulator lengths;
    optional<string> error;

    query_record(const string& in, const string& out, bool include_nodes)
        : input(in), output(out), tree(load_query(in)), lengths(tree, include_nodes) {}
    query_record(const query_record&) = delete;
    query_record& operator=(const query_record&) = delete;
};

// Parse/normalize each sample once. Each query still has its own Day index and conditional
// counts; no nesting or compatibility between queries is assumed, and no samples are retained.
struct batch_accumulator: accumulator<SequenceTree>
{
    vector<std::unique_ptr<query_record>> queries;

    void operator()(const SequenceTree& sample) override
    {
        for (auto& query: queries) query->lengths(sample);
    }
};

// Compare canonical paths and existing file identities, including symlinks and hard links.
// Nonexistent output paths can still alias each other through '.', '..', or a symlinked parent.
bool same_file(const string& a, const string& b)
{
    namespace fs = std::filesystem;
    std::error_code ec;
    if (fs::equivalent(a, b, ec)) return true;
    auto path_a = fs::weakly_canonical(fs::absolute(a), ec);
    if (ec) path_a = fs::absolute(a).lexically_normal();
    auto path_b = fs::weakly_canonical(fs::absolute(b), ec);
    if (ec) path_b = fs::absolute(b).lexically_normal();
    // Defer access errors to that query's output attempt, so other outputs can still succeed.
    return path_a == path_b;
}

// Validate all destinations before reading samples or opening any output for truncation.
void check_destinations(const vector<string>& pairs, const vector<string>& samples)
{
    bool stdout_used = false;
    for (size_t i = 0; i < pairs.size(); i += 2)
    {
        const auto& output = pairs[i+1];
        if (output == "-")
        {
            if (stdout_used) throw myexception()<<"Only one query may write to stdout.";
            stdout_used = true;
            continue;
        }
        for (size_t j = 0; j < i; j += 2)
            if (pairs[j+1] != "-" and same_file(output, pairs[j+1]))
                throw myexception()<<"Duplicate output destination '"<<output<<"'.";
        for (size_t j = 0; j < pairs.size(); j += 2)
            if (same_file(output, pairs[j]))
                throw myexception()<<"Output '"<<output<<"' would overwrite query '"<<pairs[j]<<"'.";
        for (const auto& sample: samples)
            if (sample != "-" and same_file(output, sample))
                throw myexception()<<"Output '"<<output<<"' would overwrite sample file '"<<sample<<"'.";
    }
}

// Render one validated query using the existing branch and node output conventions.
// Only lengths change; the tree topology referenced by the Day index remains intact.
void write_estimates(std::ostream& out, SequenceTree& Q, const length_accumulator& A,
                     bool deviation, bool show_nodes)
{
    for (int b = 0; b < Q.n_branches(); ++b)
        Q.branch(b).set_length(deviation ? A.branch_sd(b) : A.branch_mean(b));
    if (not show_nodes)
        for (int n: A.multifurcations)
        {
            double share = A.node_mean(n) / Q.node(n).degree();
            for (auto b = Q.node(n).branches_out(); b; ++b)
                (*b).set_length((*b).length() + share);
        }
    out<<Q<<'\n';
    if (show_nodes)
        for (int n: A.multifurcations)
            if (A.node_mean(n) > 0)
            {
                out<<"node "<<A.node_mean(n)<<'\n';
                int b = (*Q.node(n).branches_in()).name();
                out<<partition_from_branch(Q, b)<<'\n';
            }
}

} // namespace


int main(int argc, char* argv[])
{
    try
    {
        CLI::App app{"Compute conditional branch and node lengths for query trees.", "tree-mean-lengths"};
        app.usage("Usage: tree-mean-lengths --tree QUERY [OPTIONS] FILE [FILE ...]\n"
                  "       tree-mean-lengths --trees QUERY OUTPUT [--trees QUERY OUTPUT ...]\n"
                  "                         [OPTIONS] FILE [FILE ...]");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        string tree, ignore;
        vector<string> pairs, files;
        int skip = 0, subsample = 1;
        optional<int> last;
        bool simple = false, var = false, no_node_lengths = false, show_node_lengths = false;
        bool safe = false, verbose = false;
        app.add_option("-T,--tree", tree, "Unrooted query tree; write to stdout");
        // Each occurrence consumes one two-filename group; take_all retains repeated groups
        // while disabling extra arguments leaves subsequent sample filenames positional.
        app.add_option("--trees", pairs, "Query and output filenames (repeatable; - outputs to stdout)")
            ->type_name("INPUT OUTPUT")->type_size(2)->expected(1)->allow_extra_args(false)->take_all();
        app.add_option("FILE", files, "Tree samples to examine")->required()->type_name("");
        app.add_option("-s,--skip", skip, "Number of tree samples to skip")->capture_default_str();
        app.add_option("-u,--until", last, "Read until this number of trees");
        app.add_option("--ignore", ignore, "Taxa to remove (comma-separated list or @filename)");
        app.add_flag("--simple", simple, "Report conditional branch means only (overrides other output modes)");
        app.add_option("-x,--subsample", subsample, "Factor by which to sub-sample")->capture_default_str();
        app.add_flag("--var", var, "Report standard deviation of branch lengths instead of mean");
        app.add_flag("--no-node-lengths", no_node_lengths, "Do not distribute node lengths onto branches");
        app.add_flag("--safe", safe, "Print topology without lengths if requested estimates are unavailable");
        app.add_flag("--show-node-lengths", show_node_lengths,
                     "Report conditional branch and node means separately for draw-tree");
        app.add_flag("-V,--verbose", verbose, "Output more log messages on stderr");
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }
        if (verbose) log_verbose = 1;
        if (not app.count("--tree") and not app.count("--trees"))
            throw myexception()<<"No tree topology specified.";
        if (app.count("--tree") and app.count("--trees"))
            throw myexception()<<"Use either --tree or --trees, not both.";
        if (app.count("--tree")) pairs = {tree, "-"};
        vector<string> prune = get_string_list(ignore);
        check_destinations(pairs, files);

        // Preserve output precedence: simple means first, then SDs, then separate node
        // lengths, then branch-only means or the default redistribution of node lengths.
        const bool deviation = not simple and var;
        const bool show_nodes = not simple and not deviation and show_node_lengths;
        const bool include_nodes = not simple and not deviation and (show_nodes or not no_node_lengths);
        batch_accumulator batch;
        for (size_t i = 0; i < pairs.size(); i += 2)
        {
            try
            {
                auto query = std::make_unique<query_record>(pairs[i], pairs[i+1], include_nodes);
                if (not batch.queries.empty() and
                    query->tree.get_leaf_labels() != batch.queries.front()->tree.get_leaf_labels())
                    throw myexception()<<"Leaf labels differ from query '"<<batch.queries.front()->input<<"'.";
                batch.queries.push_back(std::move(query));
            }
            catch (std::exception& e)
            {
                throw myexception()<<"Query '"<<pairs[i]<<"': "<<e.what();
            }
        }

        for (const auto& file: files)
        {
            istream_or_ifstream f(std::cin, "-", file);
            scan_trees(f, skip, last, subsample, prune, batch.queries.front()->tree.get_leaf_labels(), batch);
            // The legacy reader reports parse errors and sets badbit instead of throwing.
            // Do not mistake that early stop for EOF and output estimates from a partial sample.
            if (f.bad()) throw myexception()<<"Failed to read sample file '"<<file<<"'.";
        }

        if (log_verbose)
            cerr<<"Read "<<batch.queries.front()->lengths.n_samples<<" sampled trees."<<endl;
        bool failed = false;
        for (auto& query: batch.queries)
        {
            try { query->lengths.check_estimates(); }
            catch (std::exception& e)
            {
                query->error = e.what();
                cerr<<"tree-mean-lengths: Query '"<<query->input<<"': "<<e.what()<<endl;
                if (not safe) failed = true;
            }
            if (log_verbose)
            {
                const auto& A = query->lengths;
                cerr<<"Query '"<<query->input<<"':"<<endl;
                for (int b = 0; b < query->tree.n_branches(); ++b)
                    cerr<<"  Branch "<<b<<": "<<A.branch_count[b]<<" contributing samples."<<endl;
                for (int n: A.multifurcations)
                    cerr<<"  Node "<<n<<": "<<A.node_count[n]<<" contributing samples."<<endl;
            }
        }

        // All queries have been checked, but one unavailable estimate must not suppress
        // other results. Failed destinations remain untouched unless --safe requests fallback.
        for (auto& query: batch.queries)
        {
            if (query->error and not safe) continue;
            try
            {
                ostream_or_ofstream out(cout, "-", query->output, "annotated query tree");
                if (query->error) out<<query->tree.write(false)<<'\n';
                else write_estimates(out, query->tree, query->lengths, deviation, show_nodes);
                out.flush();
                if (not out) throw myexception()<<"Failed to write output '"<<query->output<<"'.";
            }
            catch (std::exception& e)
            {
                cerr<<"tree-mean-lengths: Query '"<<query->input<<"': "<<e.what()<<endl;
                failed = true;
            }
        }
        return failed ? 1 : 0;
    }
    catch (std::exception& e)
    {
        cerr<<"tree-mean-lengths: Error! "<<e.what()<<endl;
        return 1;
    }
}
