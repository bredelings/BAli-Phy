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

#include "ancestor-queries.hh"
#include "partition.hh"
#include "tree/tree-util.hh"
#include "util/io.hh"
#include "util/log-level.hh"
#include "util/string/strip.hh"
#include "util/string/split.hh"
#include <ranges>

using std::string;
using std::vector;
using std::pair;
using std::map;
using std::optional;
using boost::dynamic_bitset;

namespace {

// Index directed branches by the leaf set on their target side.
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


// Match query branches to sampled branches with the same directed split.
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


// Check that all corresponding branches in T point to a single node in T.
// This should rule out cases where the Q node is a polytomy, but T is bifurcating.
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


// Map query nodes whose incident branches identify one sampled node.
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

// Turn labelled tree edges into named queries for their internal endpoints.
vector<pair<string,dynamic_bitset<>>> get_branch_queries_from_tree(SequenceTree Q, const vector<string>& leaf_names)
{
    // 1. Read tree from file
    remap_T_leaf_indices(Q, leaf_names);

    // 2. Check that all nodes are labelled.
    int n_unlabelled = 0;
    for(auto& label: Q.get_labels())
        if (label.empty())
            n_unlabelled++;

    if (n_unlabelled > 0)
        std::cerr<<"Warning: Tree contains "<<n_unlabelled<<" unlabelled nodes!\n";

    // 3. Construct groups from branches
    vector<pair<string,dynamic_bitset<>>> groups;
    for(int n=0;n<Q.n_nodes();n++)
    {
        if (Q.node(n).is_leaf_node()) continue;

        for(auto n2: Q.neighbors(n))
        {
            if (Q.get_labels()[n].empty() or Q.get_labels()[n2].empty()) continue;
            string name = Q.get_labels()[n] + "<=" + Q.get_labels()[n2];

            int b = Q.directed_branch(n2,n);

            auto split = branch_partition(Q,b);
            groups.push_back({name, split});

            if (log_verbose)
            {
                std::cerr<<name<<" = ";
                for(int i=0;i<split.size();i++)
                {
                    if (split[i])
                        std::cerr<<Q.get_label(i)<<" ";
                }
                std::cerr<<"\n\n";
            }
        }
    }

    return groups;
}

// Read named leaf groups or labelled Newick trees as directed branch queries.
vector<pair<string,dynamic_bitset<>>> load_groups_from_file(const string& filename, const vector<string>& leaf_names)
{
    namespace views = std::views;
    vector<pair<string,dynamic_bitset<>>> groups;

    istream_or_ifstream groups_file(std::cin,"-",filename,"groups file");

    for(auto& line_: read_lines(groups_file))
    {
        // Remove leading and ending whitespace
        auto line = lstrip(rstrip(line_," \t")," \t");
        if (not line.size()) continue;

        // Skip comments
        if (line[0] == '#') continue;

        // Treat this line as a tree
        if (line[0] == '(')
        {
            SequenceTree Q;
            Q.parse(line);
            auto queries = get_branch_queries_from_tree(Q, leaf_names);
            groups.insert(groups.end(), queries.begin(), queries.end());
        }
        // Treat this line as a directed split
        else
        {
            auto words = resplit(line,R"([ \t]+)");

            if (words.size() < 3 or words[1] != "=")
                throw myexception()<<"In groups file '"<<filename<<"':\n  Badly formed line:\n    |"<<line_;

            auto name = words[0];
            auto taxon_names = words | views::drop(2) | std::ranges::to<std::vector>();
            dynamic_bitset<> group = group_from_names(leaf_names, taxon_names);
            groups.push_back({name,group});
        }
    }

    return groups;
}

} // namespace

// Locate each directed group query in the sampled tree.
map<int,int> find_and_name_nodes(const SequenceTree& T,const vector<pair<string,dynamic_bitset<>>>& groups)
{
    // 1. Make something we can use to quickly look up groups
    map<dynamic_bitset<>,int> partitions;
    for(int b=0;b<T.n_branches()*2;b++)
        partitions.insert({branch_partition(T,b),b});

    // 2. Look up each group
    map<int,int> node_map;
    for(int g=0;g<groups.size();g++)
    {
        auto& [name,group] = groups[g];
        auto it = partitions.find(group);
        if (it != partitions.end())
        {
            int b = it->second;
            int node = T.directed_branch(b).target();
            node_map.insert({g,node});
        }
    }
    return node_map;
}

// Locate labelled internal query nodes in the sampled tree.
map<int,int> find_and_name_nodes(const SequenceTree& T,const SequenceTree& Q)
{
    map<int,int> node_map;
    auto Q_to_T_nodes = get_nodes_map(Q,T);

    if (log_verbose > 1)
    {
        std::cerr<<Q.write(false)<<"\n";
        std::cerr<<T.write(false)<<"\n";
    }

    for(int q_node=Q.n_leaves(); q_node < Q.n_nodes(); q_node++)
    {
        string q_node_name = Q.get_label(q_node);
        if (q_node_name.empty()) continue;

        auto t_node = Q_to_T_nodes[q_node];
        if (not t_node) continue;

        string t_node_name = T.get_label(*t_node);
        if (log_verbose > 0)
            std::cerr<<"Mapped "<<q_node_name<<" -> "<<t_node_name<<"\n";
        node_map.insert({q_node,*t_node});
    }

    return node_map;
}


// Load an optional node-query tree with the sample leaf ordering.
optional<SequenceTree> get_node_queries(const optional<string>& filename, const vector<string>& leaf_names)
{
    optional<SequenceTree> Q;
    if (filename)
    {
        Q = load_tree_from_file(*filename);
        remap_T_leaf_indices(*Q, leaf_names);
    }
    return Q;
}


// Load optional named branch queries using the sample leaf ordering.
optional<vector<pair<string,dynamic_bitset<>>>> get_branch_queries(const optional<string>& filename, const vector<string>& leaf_names)
{
    optional<vector<pair<string,dynamic_bitset<>>>> groups;
    if (filename)
        groups = load_groups_from_file(*filename, leaf_names);
    return groups;
}

// Write one named FASTA row, decoding ambiguities in the source alignment.
std::ostream& write_alignment_row(std::ostream& o, const string& name, const alignment& A, int row)
{
    o<<">"<<name<<"\n";
    int widened = 0;
    for(int i=0;i<A.length();i++)
    {
        auto [spelling, exact] = A.decode(A(i,row));
        o<<spelling;
        widened += not exact;
    }
    o<<"\n";
    if (widened)
        std::cerr<<"Warning: widened "<<widened
                 <<" non-Cartesian or unspellable ambiguities while writing sequences.\n";
    return o;
}


