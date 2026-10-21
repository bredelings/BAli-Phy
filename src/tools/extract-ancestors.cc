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

#include <CLI/CLI.hpp>
#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include "util/myexception.hh"
#include "util/log-level.hh"
#include "util/rng.hh"
#include "util/io.hh"
#include "alignment/alignment.hh"
#include "alignment/index-matrix.hh"
#include "tree-align/link.hh"
#include "alignment/load.hh"

#include "tree/tree-util.hh"
#include "alignment/alignment-util.hh"
#include "distance-methods.hh"
#include "joint-A-T.hh"
#include "ancestor-queries.hh"
#include <optional>
#include <map>

#include <boost/dynamic_bitset.hpp>

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


//TODO: add flag to affect gap_must_be_half.

//TODO: sort rows, including internal nodes, by tree... how?
//      -- see alignment-cat --reorder-by-tree
//      We could sort the leaf and internal nodes I think.
//      We could also sort tree branches, like node1,node1=>node2,node2=>node1,node2
//      Group queries not directly derived from the tree would be more complicated to sort.

void extract_and_name_ancestors(const joint_A_T& samples,
                                const optional<SequenceTree>& node_queries,
                                const optional<vector<pair<string,dynamic_bitset<>>>>& branch_queries)
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

int main(int argc,char* argv[])
{
    myrand_init();

    try {
        CLI::App app{"Extract named ancestral sequences from sampled alignments and trees.", "extract-ancestors"};
        app.usage("Usage: extract-ancestors [OPTIONS]");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        vector<string> alignment_files, tree_files;
        string alphabet_name;
        optional<string> nodes_file, groups_file;
        unsigned tree_stride = 10;
        app.add_option("-A,--alignments", alignment_files, "File of alignment samples; repeat for each chain")
            ->type_name("FILE")->type_size(1)->expected(1)->allow_extra_args(false)->take_all()->required();
        app.add_option("-T,--trees", tree_files, "Corresponding tree samples; repeat in alignment-file order")
            ->type_name("FILE")->type_size(1)->expected(1)->allow_extra_args(false)->take_all()->required();
        app.add_option("--alphabet", alphabet_name, "Sequence alphabet (default: infer from input)")->type_name("ALPHABET");
        app.add_option("-x,--subsample", tree_stride, "Tree stride used to match alignment records (1 for paired files)")
            ->capture_default_str();
        app.add_option("-n,--nodes", nodes_file, "Newick tree with labelled ancestors")->type_name("FILE");
        app.add_option("-g,--groups", groups_file, "File with named groups")->type_name("FILE");
        auto* verbosity = app.add_option("-V,--verbose", log_verbose, "Diagnostic verbosity (1 if no value is given)")
            ->type_name("LEVEL")->expected(0, 1);
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }
        // CLI11 represents a value-less optional argument as an empty result; -V means level 1.
        if (verbosity->count() and verbosity->results().front().empty())
            log_verbose = 1;

        // 1. Load alignment and tree samples
        if (log_verbose) cerr<<"extract-ancestors: Loading alignments and trees...\n";
        joint_A_T samples = get_multiple_joint_A_T(
            alignment_files, tree_files,
            alphabet_name, true, tree_stride, 0, {}, 1, {});

        if (samples.size() == 0)
            throw myexception()<<"No (A,T) read in!";
        else
            if (log_verbose) cerr<<"read "<<samples.size()<<" (A,T) pairs.\n\n";

        // 2. Load queries to find ancestor nodes on the tree
        auto node_queries = get_node_queries(nodes_file, samples.leaf_names());
        auto branch_queries = get_branch_queries(groups_file, samples.leaf_names());
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
