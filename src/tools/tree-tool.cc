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

#include <iostream>
#include <list>
#include <set>
#include <utility>
#include "tree/tree.H"
#include "tree/sequencetree.H"
#include "tree/tree-util.H"
#include "tree-dist.H"
#include "util/myexception.H"
#include "util/assert.hh"
#include "util/string/split.H"
#include "util/cmdline.H"
#include "findroot.H"
#include "tools/read-trees.H"

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
using std::set;
using std::valarray;
using std::pair;

using std::optional;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    // named options
    options_description invisible("Invisible options");
    invisible.add_options()
        ("tree", value<string>(),"tree to operate on");

    options_description general("General options");
    general.add_options()
        ("help,h", "produce help message")
        ("verbose,V","Output more log messages on stderr.")
        ;

    options_description commands("Modification options");
    commands.add_options()
        ("prune",value<string>(),"Comma-separated taxa to remove")
        ("root","Find a root position")
        ("resolve","Resolve polytomies")
        ("remove-root-branch","Remove single branch from root.")
        ("remove-root-branches","Ensure root is not a tip.")
        ("remove-knuckles","Remove degree-2 nodes.")
        ("scale", value<double>(), "Scale branch-lengths by factor")
        ("strip-internal-names","Remove internal node names")
        ("name-all-nodes","Add node names")
        ;

    options_description output("Output options");
    output.add_options()
        ("length","Report the total tree length")
        ("diameter","Report the tree diameter")
        ("count-leaves","Show the number of leaves")
        ("count-nodes","Show the number of nodes")
        ("show-leaves","Show the leaf names")
        ("show-nodes","Show the node names")
        ;
    options_description visible("All options");
    visible.add(general).add(commands);
    options_description all("All options");
    all.add(general).add(commands).add(output).add(invisible);

    // positional options
    positional_options_description p;
    p.add("tree", 1);
  
    variables_map args;     
    store(command_line_parser(argc, argv).
          options(all).positional(p).run(), args);
    notify(args);    

    if (args.count("help")) {
        cout<<"Perform various operations on Newick trees.\n\n";
        cout<<"Usage: tree-tool <tree-file> [OPTIONS]\n\n";
        cout<<general<<"\n";
        cout<<commands<<"\n";
        cout<<output<<"\n";
        exit(0);
    }

    if (args.count("verbose")) log_verbose = 1;

    return args;
}

void resolve(Tree& T, int node)
{
    while(true)
    {
        auto neighbors = T.neighbors(node);
        if (neighbors.size() <= 3) return;

        int new_node = T.create_node_on_branch(T.directed_branch(neighbors[0],node)).name();
        if (not T.directed_branch(new_node,node).has_length())
            T.directed_branch(new_node,node).set_length(0.0);
        if (not T.directed_branch(new_node,neighbors[0]).has_length())
            T.directed_branch(new_node,neighbors[0]).set_length(0.0);
        
        T.reconnect_branch(neighbors[1],node,new_node);
        assert(T.node(node).degree() < neighbors.size());
    }
}

void scale(Tree& T, double f)
{
    for(int b=0;b<T.n_branches();b++)
        T.branch(b).set_length(f * T.branch(b).length());
}

vector<int> polytomies(const Tree& T)
{
    vector<int> p;
    for(int n=0;n<T.n_nodes();n++)
        if (T.node(n).degree() > 3)
            p.push_back(n);
    return p;
}

bool remove_root_branch(RootedTree& T)
{
    if (T.root().degree() == 1)
    {
        int old_root = T.root();
        int new_root = T.neighbors(old_root)[0];
        T.reroot(new_root);
        // This ALSO removes degree-2 nodes that are created, which should maybe be separate.
        T.prune_leaf(old_root);
        return true;
    }
    else
        return false;
}


int main(int argc,char* argv[]) 
{ 
    try {
        //----------- Parse command line  ----------//
        variables_map args = parse_cmd_line(argc,argv);

        vector<string> prune = get_string_list(args, "prune");

        //----------- Read the topology -----------//
        if (not args.count("tree"))
            throw myexception()<<"Tree file not specified! (--tree <filename>)";

        RootedSequenceTree T;
        {
            auto filename = args["tree"].as<string>();
            using namespace trees_format;
            istream_or_ifstream file(std::cin, "-", filename, "tree file");
            std::shared_ptr<reader_t> trees_in(new Newick_or_NEXUS(file));
            if (not prune.empty())
                trees_in = std::shared_ptr<reader_t>(new Prune(prune,*trees_in));
            if (not trees_in->next_tree(T))
                throw myexception()<<"No tree in file '"<<filename<<"'";
        }

        if (args.count("remove-root-branch"))
        {
            remove_root_branch(T);
        }
        if (args.count("remove-root-branches"))
        {
            while(remove_root_branch(T))
                ;
        }
        if (args.count("resolve"))
        {
            for(int n: polytomies(T))
                resolve(T,n);
            assert(polytomies(T).empty());
        }
        if (args.count("scale"))
        {
            double factor = args["scale"].as<double>();
            scale(T, factor);
        }
        if (args.count("strip-internal-names"))
        {
            for(int n=T.n_leaves(); n<T.n_nodes(); n++)
                T.set_label(n,"");
        }
        if (args.count("name-all-nodes"))
        {
            // FIXME - avoid using existing names!
            set<string> all_names;
            for(int n=0; n<T.n_nodes(); n++)
                if (not T.get_label(n).empty())
                    all_names.insert(T.get_label(n));

            int index = 0;
            for(int n=0; n<T.n_nodes(); n++)
            {
                if (T.get_label(n).empty())
                {
                    string label;
                    do
                    {
                        index++;
                        label = "node_"+std::to_string(index);
                    }
                    while(all_names.count(label));
                    T.set_label(n,label);
                    all_names.insert(label);
                }
            }
        }
        if (args.count("root"))
        {
            T = find_rooted_tree(T);
        }

        if (args.count("length"))
            std::cout<<tree_length(T)<<std::endl;
        else if (args.count("diameter"))
            std::cout<<tree_diameter(T)<<std::endl;
        else if (args.count("count-leaves"))
            std::cout<<T.n_leaves()<<std::endl;
        else if (args.count("count-nodes"))
            std::cout<<T.n_nodes()<<std::endl;
        else if (args.count("show-leaves"))
        {
            for(auto& label: T.get_leaf_labels())
                std::cout<<label<<std::endl;
        }
        else if (args.count("show-nodes"))
        {
            for(auto& label: T.get_labels())
                std::cout<<label<<std::endl;
        }
        else
            std::cout<<T<<std::endl;
    }
    catch (std::exception& e) {
        std::cerr<<"tree-tool: Error! "<<e.what()<<endl;
        exit(1);
    }
    return 0;
}
