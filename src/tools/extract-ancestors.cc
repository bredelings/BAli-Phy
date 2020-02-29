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
#include "util/rng.H"
#include "util/string/strip.H"
#include "util/string/split.H"
#include "util/io.H"
#include "util/cmdline.H"
#include "alignment/alignment.H"
#include "alignment/index-matrix.H"
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

//TODO: add flag to affect gap_must_be_half.

//TODO: sort rows, including internal nodes, by tree... how?
//      -- see alignment-cat --reorder-by-tree
//      We could sort the leaf and internal nodes I think.
//      We could also sort tree branches, like node1,node1=>node2,node2=>node1,node2
//      Group queries not directly derived from the tree would be more complicated to sort.

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
        std::cerr<<"Warning: Tree contains "<<n_unlabelled<<" nodes!\n";

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

vector<pair<string,dynamic_bitset<>>> load_groups_from_file(const string& filename, const vector<string>& leaf_names)
{
    namespace view = ranges::view;
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
            auto taxon_names = words | view::drop(2);
            dynamic_bitset<> group = group_from_names(leaf_names, taxon_names);
            groups.push_back({name,group});
        }
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

std::ostream& write_alignment_row(std::ostream& o, const string& name, const alignment& A, int row)
{
    o<<">"<<name<<"\n";
    auto& a = A.get_alphabet();
    for(int i=0;i<A.length();i++)
        o<<a.lookup(A(i,row));
    o<<"\n";
    return o;
}


void extract_and_name_ancestors(const joint_A_T& samples,
                                const optional<SequenceTree>& node_queries,
                                const optional<vector<pair<string,dynamic_bitset<>>>> branch_queries)
{
    vector<int> node_counts;
    if (node_queries)
        node_counts.resize(node_queries->n_nodes());

    vector<int> group_counts;
    if (branch_queries)
        group_counts.resize(branch_queries->size());

    for(auto& [A,T]: samples)
    {
        map<string,int> internal_nodes;
        
        // 2. Find ancestors from node queries
        if (node_queries)
        {
            for(auto& [q_node, t_node]: find_and_name_nodes(T, *node_queries))
            {
                node_counts[q_node]++;

                auto name = node_queries->get_labels()[q_node];
                if (not name.empty())
                    internal_nodes.insert({name, t_node});
            }
        }

        // 3. Find ancestors from branch queries.
        if (branch_queries)
        {
            for(auto& [g, t_node]: find_and_name_nodes(T,*branch_queries))
            {
                group_counts[g]++;

                auto& [name,_] = (*branch_queries)[g];
                internal_nodes.insert({name, t_node});
            }
        }

        // 4. Write leaf sequences
        for(int node=0;node<samples.leaf_names().size();node++)
            write_alignment_row(std::cout, samples.leaf_names()[node], A, node);

        // 5. Write named ancestors sequences corresponding to node and branch queries
        for(auto& [name,node]: internal_nodes)
            write_alignment_row(std::cout, name, A, node);

        std::cout<<"\n\n";
    }

    // Report statistics on the frequency of queried nodes
    if (node_queries)
    {
        for(int q_node=node_queries->n_leaves(); q_node<node_queries->n_nodes(); q_node++)
        {
            auto q_node_name = node_queries->get_label(q_node);
            if (q_node_name.empty()) continue;

            double fraction = double(node_counts[q_node])/samples.size();
            std::cerr<<"Node '"<<q_node_name<<"': present in "<<node_counts[q_node]<<"/"<<samples.size()<<" = "<<fraction*100<<"% of samples.\n";
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
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    // invisible options
    options_description invisible("Invisible options");
    invisible.add_options()
        ("alphabet",value<string>(),"set to 'Codons' to prefer codon alphabets")
        ;
    // named options
    options_description general("General options");
    general.add_options()
        ("help,h", "produces help message")
        ("verbose,V",value<int>()->implicit_value(1),"Show more log messages on stderr.")
        ;

    options_description input("Input options");
    input.add_options()
        ("alignments,A", value<vector<string>>()->composing(),"File of alignment samples")
        ("trees,T", value<vector<string>>()->composing(), "File of corresponding tree samples")
        ("subsample,x",value<unsigned>()->default_value(10),"factor by which to sub-sample trees")
        ;

    options_description ancestors("Ancestor query options");
    ancestors.add_options()
        ("nodes,n",value<string>(),"Newick tree with labelled ancestors")
        ("groups,g",value<string>(),"File with named groups")
        ("nodes-min",value<double>()->default_value(0.34,"0.34"),"Minimum fraction to include a node.")
        ("groups-min",value<double>()->default_value(0.34,"0.34"),"Minimum fraction to include a group.")
        ;

    options_description all("All options");
    all.add(invisible).add(general).add(input).add(ancestors);

    options_description visible("All options");
    visible.add(general).add(input).add(ancestors);

    variables_map args;     
    store(command_line_parser(argc, argv).options(all).run(), args);
    // store(parse_command_line(argc, argv, desc), args);
    notify(args);    

    if (args.count("help")) {
        cout<<"Extract and name ancestral sequences according to node- and branch- queries.\n\n";
        cout<<"Usage: extract-ancestors [OPTIONS]\n";

        cout<<visible<<"\n";
        cout<<"Examples:\n\n";
        cout<<" Add named ancestral sequences to alignments where they are present:\n";
        cout<<"   % extract-ancestors -A C1.P1.fastas -T C1.trees --nodes query.tree --groups query.tree\n\n";
        exit(0);
    }

    if (args.count("verbose")) log_verbose = args["verbose"].as<int>();

    if (not args.count("alignments"))
        throw myexception()<<"No alignments given! (Use -A alignments_file)";

    if (not args.count("trees"))
        throw myexception()<<"No trees given! (Use -T trees_file)";

    return args;
}

int main(int argc,char* argv[])
{
    myrand_init();

    try {
        variables_map args = parse_cmd_line(argc,argv);
        //Arguments args;
        //args.read(argc,argv);
        //args.print(cerr);

        // 1. Load alignment and tree samples
        if (log_verbose) cerr<<"extract-ancestors: Loading alignments and trees...\n";
        joint_A_T samples = get_multiple_joint_A_T(args,true);

        if (samples.size() == 0)
            throw myexception()<<"No (A,T) read in!";
        else
            if (log_verbose) cerr<<"read "<<samples.size()<<" (A,T) pairs.\n\n";

        // 2. Load queries to find ancestor nodes on the tree
        auto node_queries = get_node_queries(args, samples);
        auto branch_queries = get_branch_queries(args, samples);
        if (not node_queries and not branch_queries)
            std::cerr<<"WARNING: no ancestors defined!\n";
        
        // 4. Extract profiles
        extract_and_name_ancestors(samples, node_queries, branch_queries);
    }
    catch (std::exception& e) {
        cerr<<"extract-ancestors: Error! "<<e.what()<<endl;
        exit(1);
    }
    return 0;
}
