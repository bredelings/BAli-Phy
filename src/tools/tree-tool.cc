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
#include "tree/tree.hh"
#include "tree/sequencetree.hh"
#include "tree/tree-util.hh"
#include "tree-dist.hh"
#include "util/myexception.hh"
#include "util/assert.hh"
#include "util/string/split.hh"
#include "util/cmdline.hh"
#include "findroot.hh"
#include "tools/read-trees.hh"

#include <CLI/CLI.hpp>

extern int log_verbose;

using boost::dynamic_bitset;


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
        CLI::App app{"Transform one tree or report its properties.", "tree-tool"};
        app.usage("Usage: tree-tool TREE [OPTIONS]");
        string tree_file, prune_names, knuckles;
        double factor = 1;
        bool verbose = false;
        bool do_root = false, do_resolve = false;
        bool do_remove_root_branch = false, remove_root_branches = false;
        bool strip_internal_names = false, name_all_nodes = false;
        bool length = false, diameter = false, count_leaves = false, count_nodes = false;
        bool node_degrees = false, show_leaves = false, show_nodes = false;
        app.add_option("--tree,TREE", tree_file, "Tree file ('-' reads stdin)")->required()->type_name("");
        app.add_flag("-V,--verbose", verbose, "Output diagnostic messages");
        app.add_option("--prune", prune_names, "Taxa to remove (comma-separated list or @filename)");
        app.add_option("--scale", factor, "Multiply branch lengths by this factor");
        app.add_option("--remove-knuckles", knuckles, "Suppress degree-two nodes: nonroot (default) or all")
            ->expected(0, 1)->default_str("nonroot")->check(CLI::IsMember({"nonroot", "all"}));
        app.add_flag("--root", do_root, "Find a root position");
        app.add_flag("--resolve", do_resolve, "Resolve polytomies");
        app.add_flag("--remove-root-branch", do_remove_root_branch, "Remove a degree-one root and its branch");
        app.add_flag("--remove-root-branches", remove_root_branches, "Repeat degree-one root removal");
        app.add_flag("--strip-internal-names", strip_internal_names, "Remove internal node names");
        app.add_flag("--name-all-nodes", name_all_nodes, "Name unnamed nodes");
        app.add_flag("--length", length, "Print total tree length");
        app.add_flag("--diameter", diameter, "Print tree diameter");
        app.add_flag("--count-leaves", count_leaves, "Print leaf count");
        app.add_flag("--count-nodes", count_nodes, "Print node count");
        app.add_flag("--node-degrees", node_degrees, "Print node degrees and labels in descending degree order");
        app.add_flag("--show-leaves", show_leaves, "Print leaf names");
        app.add_flag("--show-nodes", show_nodes, "Print all node names");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
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


        vector<string> prune = get_string_list(prune_names);

        RootedSequenceTree T;
        {
            using namespace trees_format;
            istream_or_ifstream file(std::cin, "-", tree_file, "tree file");
            std::shared_ptr<reader_t> trees_in(new Newick_or_NEXUS(file));
            if (not prune.empty())
                trees_in = std::shared_ptr<reader_t>(new Prune(prune,*trees_in));
            if (not trees_in->next_tree(T))
                throw myexception()<<"No tree in file '"<<tree_file<<"'";
        }

        if (do_remove_root_branch)
        {
            remove_root_branch(T);
        }
        if (remove_root_branches)
        {
            while(remove_root_branch(T))
                ;
        }
        if (knuckles == "all")
            remove_sub_branches(T);
        else if (knuckles == "nonroot")
        {
            // Suppress only intermediate nodes, preserving the designated root. Removal
            // renumbers nodes, so restart the search after each joined pair of branches.
            while (true)
            {
                int node = 0;
                while (node < T.n_nodes() and (node == T.root() or T.node(node).degree() != 2))
                    ++node;
                if (node == T.n_nodes()) break;
                T.remove_node_from_branch(node);
            }
        }
        if (do_resolve)
        {
            for(int n: polytomies(T))
                resolve(T,n);
            assert(polytomies(T).empty());
        }
        if (app.count("--scale"))
        {
            scale(T, factor);
        }
        if (strip_internal_names)
        {
            for(int n=T.n_leaves(); n<T.n_nodes(); n++)
                T.set_label(n,"");
        }
        if (name_all_nodes)
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
        if (do_root)
        {
            T = find_rooted_tree(T);
        }

        if (length)
            std::cout<<tree_length(T)<<std::endl;
        else if (diameter)
            std::cout<<tree_diameter(T)<<std::endl;
        else if (count_leaves)
            std::cout<<T.n_leaves()<<std::endl;
        else if (count_nodes)
            std::cout<<T.n_nodes()<<std::endl;
        else if (node_degrees)
        {
            std::vector<std::pair<int,string>> degrees;
            for(int i=0; i<T.n_nodes(); i++)
                degrees.push_back({T.node(i).degree(), T.get_label(i)});
            std::sort(degrees.begin(), degrees.end(), [](const std::pair<int,string>& p1,const std::pair<int,string>& p2) {return p2.first < p1.first;});
            for(auto& [degree,label]: degrees)
                std::cout<<degree<<"\t"<<label<<"\n";
        }
        else if (show_leaves)
        {
            for(auto& label: T.get_leaf_labels())
                std::cout<<label<<std::endl;
        }
        else if (show_nodes)
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
