/*
  Copyright (C) 2005,2008-2009,2019 Benjamin Redelings

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

#include <string>
#include <vector>
#include "util/myexception.hh"
#include "util/log-level.hh"
#include "alignment/alignment.hh"

#include "util/string/split.hh"
#include "tree/tree-util.hh"
#include "alignment/alignment-util.hh"
#include "distance-methods.hh"
#include "joint-A-T.hh"
#include "partition.hh"
#include "dp/A2_states.hh"
#include <optional>
#include <map>
#include <CLI/CLI.hpp>

#include <boost/dynamic_bitset.hpp>

extern int log_verbose;

/// Group consecutive identical elements: [M,M,G1,G1,M] -> [[M,M],[G1,G1],[M]]
template <typename T>
std::vector<std::vector<T>> group_consecutive(const std::vector<T>& items)
{
    std::vector<std::vector<T>> groups;
    if (items.empty()) return groups;

    std::vector<T> current_group = {items[0]};
    for (size_t i = 1; i < items.size(); i++)
    {
        if (items[i] == items[i-1])
            current_group.push_back(items[i]);
        else
        {
            groups.push_back(std::move(current_group));
            current_group = {items[i]};
        }
    }
    groups.push_back(std::move(current_group));
    return groups;
}

using std::cout;
using std::cerr;
using std::vector;
using std::string;
using std::endl;
using std::optional;
using std::map;
using boost::dynamic_bitset;

using namespace A2;

// OK, what does a complete conflict analysis look like?
// For summaries, we want to map branches in the sampled tree
//  to either branches or nodes of the consensus tree.
// A branch can then map to a branch (aligned), a node (extends), or
//  nothing (conflict)

// For extracting sequences corresponding to a node, we want to
// map nodes on the query tree to nodes in the sampled tree.

map<dynamic_bitset<>,int> get_splits(const Tree& T)
{
    map<dynamic_bitset<>,int> split_to_branch;

    for(int b=0;b<2*T.n_branches();b++)
    {
        auto s = branch_partition(T,b);
        split_to_branch.insert({s,b});
        int b2 = T.directed_branch(b).reverse();
        split_to_branch.insert({~s,b2});
    }
    return split_to_branch;
}


vector<optional<int>> get_partial_branches_map(const Tree& Q, const Tree& T)
{
    auto T_splits = get_splits(T);

    vector<optional<int>> Q_to_T_branches(2*Q.n_branches());

    for(int b=0;b<Q_to_T_branches.size();b++)
    {
        auto qs = branch_partition(Q,b);
        if (T_splits.count(qs))
            Q_to_T_branches[b] = T_splits[qs];
    }
    return Q_to_T_branches;
}


optional<int> get_corresponding_node(int q_node, const Tree& Q, const Tree& T, const vector<optional<int>>& Q_to_T_branches)
{
    optional<int> t_node;
    for(auto q_branch=Q.node(q_node).branches_out(); q_branch; q_branch++)
    {
        auto t_branch = Q_to_T_branches[*q_branch];
        if (not t_branch) return {};

        int t_node2 = (int)T.directed_branch(*t_branch).source();
        if (not t_node)
            t_node = t_node2;
        else if (t_node2 != *t_node)
            return {};
    }
    return *t_node;
}


vector<optional<int>> get_nodes_map(const Tree& Q, const Tree& T)
{
    // For trees with no branches, we can't use branch correspondence.
    if (Q.n_nodes() == 1) return { { 0 } };

    vector<optional<int>> Q_to_T_nodes(Q.n_nodes());

    auto Q_to_T_branches = get_partial_branches_map(Q,T);

    for(int q_node=0; q_node<Q.n_nodes(); q_node++)
        Q_to_T_nodes[q_node] = get_corresponding_node(q_node, Q, T, Q_to_T_branches);

    return Q_to_T_nodes;
}

void extract_sequence(const string& query_file, const joint_A_T& J)
{
    auto Q = load_tree_from_file(query_file);

    remap_T_leaf_indices(Q, J.leaf_names());

    vector<int> node_counts(Q.n_nodes());

    for(auto& [A,T]: J)
    {
        auto Q_to_T_nodes = get_nodes_map(Q,T);

        if (log_verbose > 1)
        {
            std::cerr<<Q.write(false)<<"\n";
            std::cerr<<T.write(false)<<"\n";
        }
        for(int q_node=Q.n_leaves(); q_node < Q.n_nodes(); q_node++)
        {
            auto t_node = Q_to_T_nodes[q_node];
            if (not t_node) continue;

            string q_node_name = Q.get_label(q_node);
            if (q_node_name.empty()) continue;

            node_counts[q_node]++;

            string t_node_name = T.get_label(*t_node);
            if (log_verbose > 0)
                std::cerr<<"Mapped "<<q_node_name<<" -> "<<t_node_name<<"\n";
            std::cout<<">"<<q_node_name<<"\n";
            int widened = 0;
            for(int i=0;i<A.length();i++)
            {
                auto [spelling, exact] = A.decode(A(i,*t_node));
                std::cout<<spelling;
                widened += not exact;
            }
            std::cout<<"\n";
            if (widened)
                std::cerr<<"Warning: widened "<<widened
                         <<" non-Cartesian or unspellable ambiguities while writing sequences.\n";
        }
        std::cout<<"\n\n";
    }
    for(int q_node=Q.n_leaves(); q_node<Q.n_nodes(); q_node++)
    {
        auto q_node_name = Q.get_label(q_node);
        if (q_node_name.empty()) continue;

        std::cerr<<"Node '"<<q_node_name<<"': present in "<<node_counts[q_node]<<"/"<<J.size()<<" = "<<double(node_counts[q_node])/J.size()*100<<"% of samples.\n";
    }
}

void run_analysis(const string& partition_name, bool output_details, const joint_A_T& J) {

    vector<string> pnames = split(partition_name, ':');

    // Struct to collect indel info
    // Start1/Start2: symbols emitted in each sequence BEFORE the indel
    // End1/End2: symbols emitted in each sequence AFTER the indel
    struct IndelInfo {
        int start1, start2;  // symbols in seq1/seq2 before indel
        int end1, end2;      // symbols in seq1/seq2 after indel
        int type;
        int length;
        std::string sequence;  // the actual indel sequence
    };

    //--------------------- Load (A,T) ------------------------//
    if (output_details)
        std::cout << "Sample\tStart1\tStart2\tEnd1\tEnd2\tType\tLength\tLen1\tLen2\tSequence" << endl;
    else
        std::cout << "Len\tIndels" << endl;

    int consistentsamples = 0;
    int numindels = 0;
    int sample_num = 0;

    for(auto& [A,T]: J)
    {
        Partition part = full_partition_from_names(T.get_leaf_labels(),pnames);

        bool exists = implies(T,part);

        if( exists ) {
            consistentsamples++;
            int b = which_branch(T,part);
            if (b == -1) throw myexception()<<"Can't find branch in tree!";

            int node1 = T.directed_branch(b).target();
            int node2 = T.directed_branch(b).source();
            vector<int> pairwiseA = get_path(A, node1, node2);

            // Coordinates count symbols, so retain their alignment columns until decoding.
            vector<int> columns1, columns2;
            if (output_details)
            {
                for (int c = 0; c < A.length(); c++)
                {
                    if (not A.gap(c, node1)) columns1.push_back(c);
                    if (not A.gap(c, node2)) columns2.push_back(c);
                }
            }

            // Group consecutive identical states
            auto groups = group_consecutive(pairwiseA);

            int uniqueindels = 0;
            int pairwise_length = 0;
            int pos1 = 0;  // symbols emitted in sequence 1
            int pos2 = 0;  // symbols emitted in sequence 2

            std::vector<IndelInfo> sample_indels;

            for (const auto& group : groups)
            {
                int state = group[0];
                int length = group.size();
                if (state == states::M or state == states::G1 or state == states::G2)
                    pairwise_length += length;

                if (state == states::G1 or state == states::G2)
                {
                    uniqueindels++;

                    int start1 = pos1;
                    int start2 = pos2;

                    if (state == states::G1)
                        pos2 += length;
                    else
                        pos1 += length;

                    if (output_details)
                    {
                        const auto& columns = state == states::G1 ? columns2 : columns1;
                        int node = state == states::G1 ? node2 : node1;
                        int start = state == states::G1 ? start2 : start1;
                        string sequence;
                        for (int i = start; i < start + length; i++)
                            sequence += A.lookup(A(columns[i], node));
                        sample_indels.push_back({start1, start2, pos1, pos2, state, length, std::move(sequence)});
                    }
                }
                else if (state == states::M)
                {
                    pos1 += length;
                    pos2 += length;
                }
                // states::S and states::E don't emit characters
            }

            // Output all indels with total lengths
            if (output_details) {
                int len1 = pos1;
                int len2 = pos2;
                for (const auto& indel : sample_indels) {
                    std::cout << sample_num << "\t"
                              << indel.start1 << "\t" << indel.start2 << "\t"
                              << indel.end1 << "\t" << indel.end2 << "\t"
                              << (indel.type == states::G1 ? "I" : "D") << "\t"
                              << indel.length << "\t"
                              << len1 << "\t" << len2 << "\t"
                              << indel.sequence << endl;
                }
            }

            if( uniqueindels > 0 ) {
                numindels++;
            }

            if (not output_details)
                std::cout << pairwise_length << "\t" <<  uniqueindels << endl;
        } else {
            if (output_details)
                std::cout << sample_num << "\tNA" << endl;
            else
                std::cout << "NA\t0" << endl;
        }

        sample_num++;
    }


    if (log_verbose) {
        cerr<<"joint-indels: Total # samples      = " << J.size() << endl;
        cerr<<"joint-indels: # Consistent samples = " << consistentsamples << endl;
        cerr<<"joint-indels: # Indel samples      = " << numindels << endl;
        cerr<<"joint-indels: Posterior prob       = " << ((double)numindels/(double)J.size()) << endl;
    }
}

int main(int argc,char* argv[])
{
    try {
        CLI::App app{"Summarize indels along a specified branch or extract ancestral sequences.", "joint-indels"};
        app.usage("Usage: joint-indels ALIGNMENTS TREES [OPTIONS]");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        string alignment_file, tree_file, alphabet_name;
        unsigned subsample = 10;
        app.add_option("ALIGNMENTS", alignment_file, "File of alignment samples")->required()->type_name("");
        app.add_option("TREES", tree_file, "File of corresponding tree samples")->required()->type_name("");
        app.add_option("--subsample", subsample, "Factor by which to subsample trees")->capture_default_str();
        app.add_option("--alphabet", alphabet_name, "Alphabet to use (e.g. Codons)");
        string partition_name;
        optional<string> extract_tree;
        bool details = false;
        int verbose = 0;
        app.add_option("--partition", partition_name, "Branch identified by taxa on one side (taxon1:taxon2:...)");
        app.add_option("--extract-sequences", extract_tree, "Extract sequences corresponding to this query tree");
        app.add_flag("--details", details, "Output individual indel positions instead of counts");
        app.add_flag("-V,--verbose", verbose, "Output more log messages (repeat or use --verbose=2 for more detail)");
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }
        log_verbose = verbose;
        if (not extract_tree and not app.count("--partition"))
            throw myexception()<<"Must specify a unique partition of taxa by name.";

        if (log_verbose) cerr<<"joint-indels: Loading alignments and trees...\n";
        joint_A_T J = get_joint_A_T(alignment_file, tree_file, alphabet_name, true, subsample);

        if (J.size() == 0)
            throw myexception()<<"No (A,T) read in!";
        else
            if (log_verbose) cerr<<"joint-indels: Loaded "<<J.size()<<" (A,T) pairs."<<endl;

        if (extract_tree)
            extract_sequence(*extract_tree,J);
        else
            run_analysis(partition_name,details,J);
    }
    catch (std::exception& e) {
        cerr<<"joint-indels: Error! "<<e.what()<<endl;
        exit(1);
    }
    return 0;
}
