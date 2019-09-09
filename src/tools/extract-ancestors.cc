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

#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include "util/myexception.H"
#include "util/log-level.H"
#include "util/string/strip.H"
#include "util/string/split.H"
#include "util/io.H"
#include "util/cmdline.H"
#include "alignment/alignment.H"
#include "partition.H"
#include "tree-align/link.H"
#include "alignment/load.H"

#include "util/string/split.H"
#include "tree/tree-util.H"
#include "alignment/alignment-util.H"
#include "distance-methods.H"
#include "joint-A-T.H"
#include "partition.H"
#include "dp/A2_states.H"
#include <optional>
#include <map>

#include <boost/dynamic_bitset.hpp>
#include "range/v3/all.hpp"

using std::cin;
using std::cout;
using std::cerr;
using std::pair;
using std::istream;
using std::ifstream;
using std::vector;
using std::string;
using std::endl;
using std::optional;
using std::map;
using boost::dynamic_bitset;
//using namespace optimize;

namespace po = boost::program_options;
using po::variables_map;
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
    auto Q_splits = get_splits(Q);
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

vector<pair<string,dynamic_bitset<>>> load_groups_from_file(const string& filename, const vector<string>& leaf_names)
{
    namespace view = ranges::view;
    vector<pair<string,dynamic_bitset<>>> groups;

    istream_or_ifstream groups_file(std::cin,"-",filename,"groups file");

    for(auto& line_: read_lines(groups_file))
    {
        auto line = lstrip(rstrip(line_," \t")," \t");
        if (not line.size()) continue;
        if (line[0] == '#') continue;

        auto words = resplit(line,R"([ \t]+)");
        if (words.size() < 3 or words[1] != "=")
        {
            throw myexception()<<"In groups file '"<<filename<<"':\n  Badly formed line:\n    |"<<line_;
        }

        auto name = words[0];
        auto taxon_names = words | view::drop(2);
        dynamic_bitset<> group = group_from_names(leaf_names, taxon_names);
        groups.push_back({name,group});
    }

    return groups;
}

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


struct profile
{
    const alignment& template_A;
    vector<vector<int>> observations; // observations[site][i]

    void count_alignment(const alignment& A, int row, const vector<pair<int,int>>& corresponding_columns);
    int max_for_position(int i, bool gap_must_be_half=true) const;
    profile(const alignment& A): template_A(A), observations(A.length()) {}
};


int argmax(const map<int,int>& counts)
{
    auto max_x = counts.begin()->first;
    auto max_count = counts.begin()->second;

    for(auto& [x,count]: counts)
    {
        if (count > max_count)
        {
            max_x = x;
            max_count = count;
        }
    }

    return max_x;
}



// NOTE: `gap_must_be_half` means that we first consider "-" versus "N",
// and then secondly consider which letter the N will be.
//
// For example, if we have {"-":0.45, "A":35, "T":"0.2"} then we would
// first reject "-" because "N" is more probable, and then choose "A"
// as the most likely letter value.

int profile::max_for_position(int pos, bool gap_must_be_half) const
{
    map<int,int> counts;
    if (observations[pos].size() < 1)
        return alphabet::unknown;

    // 1. Find the counts - and each letter.
    int total = 0;
    int total_gap = 0;
    for(int letter: observations[pos])
    {
        total++;
        if (letter == alphabet::gap and gap_must_be_half)
            total_gap++;
        else if (counts.count(letter))
            counts.at(letter)++;
        else
            counts[letter] = 1;
    }

    // 2. Return gap only if its a 50% probability or higher
    if (total_gap*2 >= total and gap_must_be_half)
        return alphabet::gap;

    // 3. Otherwise return the most frequent letter.
    return argmax(counts);
}

void profile::count_alignment(const alignment& A, int row, const vector<pair<int,int>>& corresponding_columns)
{
    for(auto [template_c, sample_c]: corresponding_columns)
    {
        observations[template_c].push_back( A(sample_c,row) );
    }
}


vector<int> get_index_column(const matrix<int>& m, int c)
{
    int n_taxa = m.size2();
    vector<int> column(n_taxa);
    for(int i=0;i<n_taxa;i++)
        column[i] = m(c,i);
    return column;
}
   

vector<pair<int,int>> map_columns(const alignment& template_A, const alignment& sample_A)
{
    int n_leaves = template_A.n_sequences();

    // 1. Construct map of indices -> template alignment column
    map<vector<int>,int> indices_to_col;

    auto template_m = M(template_A);
    for(int c=0; c<template_A.length(); c++)
    {
        auto col = get_index_column(template_m, c);
        indices_to_col.insert({col, c});
    }

    // 2. Construct list of (template column, sample column)
    vector<pair<int,int>> columns;
    auto sample_m = M(sample_A);
    for(int c=0; c<sample_A.length(); c++)
    {
        auto sample_col = get_index_column(sample_m, c);
        sample_col.resize(n_leaves);
        if (indices_to_col.count(sample_col))
        {
            int template_c = indices_to_col.at(sample_col);
            columns.push_back({template_c,c});
        }
    }

    return columns;
}


optional<SequenceTree> get_node_queries(const variables_map& args, const joint_A_T& samples)
{
    optional<SequenceTree> Q;
    if (args.count("nodes"))
    {
        Q = load_tree_from_file(args["nodes"].as<string>());
        remap_T_leaf_indices(*Q, samples.leaf_names());
    }
    return Q;
}


optional<vector<pair<string,dynamic_bitset<>>>> get_branch_queries(const variables_map& args, const joint_A_T& samples)
{
    optional<vector<pair<string,dynamic_bitset<>>>> groups;
    if (args.count("groups"))
        groups = load_groups_from_file(args["groups"].as<string>(), samples.leaf_names());
    return groups;
}

optional<vector<pair<string,dynamic_bitset<>>>> get_branch_from_tree_queries(const variables_map& args, const joint_A_T& samples)
{
    if (args.count("groups-from-tree"))
    {
        // 1. Read tree from file
        auto Q = load_tree_from_file(args["groups-from-tree"].as<string>());
        remap_T_leaf_indices(Q, samples.leaf_names());

        // 2. Check that all nodes are labelled.
        for(auto& label: Q.get_labels())
            if (label.empty())
                throw myexception()<<"groups-from-tree: tree contains unlabelled node!";

        // 3. Construct groups from branches
        vector<pair<string,dynamic_bitset<>>> groups;
        for(int b=0;b<2*Q.n_branches();b++)
        {
            int source = Q.directed_branch(b).source();
            int target = Q.directed_branch(b).target();

            if (Q.node(target).is_leaf_node()) continue;

            string name = Q.get_labels()[source] + "=>" + Q.get_labels()[target];

            auto split = branch_partition(Q,b);
            groups.push_back({name, split});
            std::cerr<<name<<" = ";
            for(int i=0;i<split.size();i++)
            {
                if (split[i])
                    std::cerr<<Q.get_label(i)<<" ";
            }
            std::cerr<<"\n";
        }
        return groups;
    }
    else
        return {};
}

std::ostream& write_alignment_row(std::ostream& o, const string& name, const alignment& A, int row)
{
    o<<">"<<name<<"\n";
    auto& a = A.get_alphabet();
    for(int i=0;i<A.length();i++)
        o<<a.lookup(A(i,row));
    o<<"\n";
    return o;
}

pair<vector<profile>,vector<profile>> extract_sequence(const joint_A_T& samples, const optional<alignment>& template_A,
                                                       bool show_ancestors,
                                                       const optional<SequenceTree>& node_queries,
                                                       const optional<vector<pair<string,dynamic_bitset<>>>> branch_queries)
{
    vector<int> node_counts;
    vector<profile> node_profiles;
    if (node_queries)
    {
        node_counts.resize(node_queries->n_nodes());
        if (template_A)
            node_profiles = vector<profile>(node_queries->n_nodes(), profile(*template_A));
    }

    vector<profile> group_profiles;
    vector<int> group_counts;
    if (branch_queries)
    {
        group_counts.resize(branch_queries->size());
        if (template_A)
            group_profiles = vector<profile>(branch_queries->size(), profile(*template_A));
    }

    for(auto& [A,T]: samples)
    {
        // 1. Find out which columns of the alignment template are in the alignment sample
        optional<vector<pair<int,int>>> corresponding_columns;
        if (template_A)
            corresponding_columns = map_columns(*template_A, A);

        map<string,int> internal_nodes;
        
        // 2. Find ancestors from node queries
        if (node_queries)
        {
            for(auto& [q_node, t_node]: find_and_name_nodes(T, *node_queries))
            {
                node_counts[q_node]++;

                if (corresponding_columns)
                    node_profiles[q_node].count_alignment(A, t_node, *corresponding_columns);

                auto name = node_queries->get_labels()[q_node];
                internal_nodes.insert({name, t_node});
            }
        }

        // 3. Find ancestors from branch queries.
        if (branch_queries)
        {
            for(auto& [g, t_node]: find_and_name_nodes(T,*branch_queries))
            {
                group_counts[g]++;

                if (corresponding_columns)
                    group_profiles[g].count_alignment(A, t_node, *corresponding_columns);

                auto& [name,_] = (*branch_queries)[g];
                internal_nodes.insert({name, t_node});
            }
        }

        if (log_verbose or (show_ancestors and not internal_nodes.empty()))
        {
            // 4. Write leaf sequences 
            for(int node=0;node<samples.leaf_names().size();node++)
                write_alignment_row(std::cout, samples.leaf_names()[node], A, node);

            // 5. Write internal node sequences for debugging purposes
            if (log_verbose)
                for(auto& [name,node]: internal_nodes)
                    write_alignment_row(std::cout, name, A, node);

            // 6. Write named ancestors sequences corresponding to node and branch queries
            for (auto& [name, node]: internal_nodes)
                write_alignment_row(std::cout, name, A, node);
            std::cout<<"\n\n";
        }


    }

    // Report statistics on the frequency of queried nodes
    if (node_queries)
    {
        for(int q_node=node_queries->n_leaves(); q_node<node_queries->n_nodes(); q_node++)
        {
            auto q_node_name = node_queries->get_label(q_node);
            if (q_node_name.empty()) continue;

            std::cerr<<"Node '"<<q_node_name<<"': present in "<<node_counts[q_node]<<"/"<<samples.size()<<" = "<<double(node_counts[q_node])/samples.size()*100<<"% of samples.\n";
        }
    }

    // Report statistics on the frequency of queried branches
    if (branch_queries)
    {
        for(int i=0;i<branch_queries->size();i++)
        {
            auto& [name,_] = (*branch_queries)[i];

            std::cerr<<"Group '"<<name<<"': present in "<<group_counts[i]<<"/"<<samples.size()<<" = "<<double(group_counts[i])/samples.size()*100<<"% of samples.\n";
        }
    }

    return {node_profiles,group_profiles};
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    // named options
    options_description invisible("Invisible options");
    invisible.add_options()
        ("alignments", value<string>(),"File of alignment samples")
        ("trees", value<string>(), "File of corresponding tree samples")
        ;

    // named options
    options_description general("General options");
    general.add_options()
        ("help,h", "produces help message")
        ("verbose,V",value<int>()->implicit_value(1),"Show more log messages on stderr.")
        ;

    options_description input("Allowed options");
    input.add_options()
        ("alphabet,a",value<string>(),"set to 'Codons' to prefer codon alphabets")
        ("subsample,x",value<unsigned>()->default_value(10),"factor by which to sub-sample trees")
        ;

    options_description ancestors("Ancestor options");
    ancestors.add_options()
        ("nodes,n",value<string>(),"Newick tree with labelled ancestors")
        ("groups,g",value<string>(),"File with named groups")
        ("groups-from-tree,t",value<string>(),"Newick tree with labelled ancestors")
        ;

    options_description output("Output options");
    output.add_options()
        ("template-alignment,T", value<string>(), "File with template alignment")
        ("show-ancestors,A",value<bool>()->default_value(false),"Write alignments with labelled ancestors")
        ;

    options_description visible("All options");
    visible.add(general).add(input).add(ancestors).add(output);

    options_description all("All options");
    all.add(visible).add(invisible);

    // positional options
    positional_options_description p;
    p.add("alignments", 1);
    p.add("trees", 1);
  
    variables_map args;     
    store(command_line_parser(argc, argv).
          options(all).positional(p).run(), args);
    // store(parse_command_line(argc, argv, desc), args);
    notify(args);    

    if (args.count("help")) {
        cout<<"Construct alignments with internal sequences for labeled nodes in query tree.\n\n";
        cout<<"Usage: extract-ancestors <alignments file> <trees file> <alignment file> [--nodes=<query tree>] [--groups=<groups file>] [OPTIONS]\n";
        cout<<visible<<"\n";
        cout<<"Examples:\n\n";
        cout<<"   % extract-ancestors C1.P1.fastas C1.trees P1-max.fasta --nodes=query.tree\n\n";
        cout<<"   % extract-ancestors C1.P1.fastas C1.trees P1-max.fasta --groups=groups.txt\n\n";
        exit(0);
    }

    if (args.count("verbose")) log_verbose = args["verbose"].as<int>();

    return args;
}

int main(int argc,char* argv[])
{ 
    try {
        variables_map args = parse_cmd_line(argc,argv);
        //Arguments args;
        //args.read(argc,argv);
        //args.print(cerr);

        // 1. Load alignment and tree samples
        if (log_verbose) cerr<<"extract-ancestors: Loading alignments and trees...\n";
        joint_A_T samples = get_joint_A_T(args,true);

        if (samples.size() == 0)
            throw myexception()<<"No (A,T) read in!";
        else
            if (log_verbose) cerr<<"read "<<samples.size()<<" (A,T) pairs.\n\n";

        // 2. Load template alignment
        optional<alignment> A;
        if (auto filename = get_arg<string>(args,"template-alignment"))
        {
            if (log_verbose) cerr<<"extract-ancestors: Loading template alignment...\n";

            vector<sequence> sequences = sequence_format::load_from_file(*filename);
            auto a_name = get_arg_default<string>(args,"alphabet", "");
            A = load_alignment(sequences, a_name);

            A = remap_A_indices(*A, samples.leaf_names(), samples.T(0).n_leaves(), samples.T(0).n_nodes());

            if (log_verbose) cerr<<"done.\n";
        }

        bool show_ancestors = get_arg_default<bool>(args,"show-ancestors",false);
        if (not show_ancestors and not args.count("template-alignment"))
            throw myexception()<<"Neither --show-ancestors not --template-align=<FASTA file> is set!";
        
        // 3. Load queries to find ancestor nodes on the tree
        auto node_queries = get_node_queries(args, samples);
        auto branch_queries = get_branch_queries(args, samples);
        auto branch_from_tree_queries = get_branch_from_tree_queries(args, samples);
        if (not branch_queries)
            branch_queries = branch_from_tree_queries;
        else if (branch_from_tree_queries)
            branch_queries->insert(branch_queries->end(), branch_from_tree_queries->begin(), branch_from_tree_queries->end());

        if (not node_queries and not branch_queries)
            std::cerr<<"WARNING: no ancestors defined!\n";
        
        // 4. Extract profiles
        auto [node_profiles, branch_profiles] = extract_sequence(samples, A, show_ancestors, node_queries, branch_queries);

        // 5. Write profiles for template alignments
        if (A)
        {
            auto& a = A->get_alphabet();

            for(int node=0;node<samples.leaf_names().size();node++)
                write_alignment_row(std::cout, samples.leaf_names()[node], *A, node);

            for(int i=0;i < branch_queries->size(); i++)
            {
                auto& [name,_] = (*branch_queries)[i];
                std::cout<<">"<<name<<"\n";
                for(int col=0; col<A->length(); col++)
                    std::cout<<a.lookup(branch_profiles[i].max_for_position(col));
                std::cout<<"\n";
            }
        }
    }
    catch (std::exception& e) {
        cerr<<"joint-indels: Error! "<<e.what()<<endl;
        exit(1);
    }
    return 0;
}
