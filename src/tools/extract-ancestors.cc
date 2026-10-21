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
#include "util/myexception.hh"
#include "util/log-level.hh"
#include "util/rng.hh"
#include "util/io.hh"
#include "util/cmdline.hh"
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

namespace po = boost::program_options;
using po::variables_map;

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
        auto node_queries = get_node_queries(get_arg<string>(args, "nodes"), samples.leaf_names());
        auto branch_queries = get_branch_queries(get_arg<string>(args, "groups"), samples.leaf_names());
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
