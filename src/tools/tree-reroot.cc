/*
  Copyright (C) 2004-2009 Benjamin Redelings

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
#include "tree/tree.hh"
#include "tree-dist.hh"
#include "tree/sequencetree.hh"
#include "util/string/split.hh"
#include "util/cmdline.hh"
#include "util/mapping.hh"
#include "tree/tree-util.hh"
#include "util/myexception.hh"

#include <CLI/CLI.hpp>

using namespace std;

using std::cout;
using std::cerr;
using std::endl;
using std::string;

using boost::dynamic_bitset;



int find_leaf(const SequenceTree& T,const string& name)
{
    int index = T.index(name);
    if (index == -1)
	throw myexception()<<"Can't find taxon '"<<name<<"' in tree";

    return index;
}

int split_branch(Tree& T,int b)
{
    // Partition lookup can select either orientation of the same branch.
    double L = T.directed_branch(b).length();

    int n = T.create_node_on_branch(b);

    // even the lengths here.
    vector<branchview> branches;
    append(T.node(n).branches_out(),branches);
  
    (branches[0]).set_length(L/2);
    (branches[1]).set_length(L/2);

    return n;
}


int main(int argc,char* argv[]) 
{ 
    try {
	//---------- Parse command line  -------//
	CLI::App app{"Place the root at a selected node or branch.", "tree-reroot"};
        app.usage("Usage: tree-reroot TREE --outgroup NAME [OPTIONS]\n"
                  "       tree-reroot TREE {--taxa LIST|--leaf NAME|--parent-of NAME|--branch SPLIT}");
        string tree_file, outgroup, taxa, leaf_name_option, parent, branch;
        app.add_option("--tree,TREE", tree_file, "Tree file (Newick or NEXUS)")->required()->type_name("");
        app.add_option("--outgroup", outgroup, "Place root halfway along this leaf's branch");
        app.add_option("--taxa", taxa, "Three taxa whose connecting paths meet at the root (list or @filename)");
        app.add_option("--leaf", leaf_name_option, "Root at this leaf");
        app.add_option("--parent-of", parent, "Root at the node adjacent to this leaf");
        app.add_option("--branch", branch, "Place root halfway along a partition: 'A B | C D'");
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

	RootedSequenceTree T = load_tree_from_file(tree_file);
    
	int root=-1;
	if (app.count("--outgroup"))
	{
	    int leaf = find_leaf(T,outgroup);

	    root = split_branch(T,leaf);
	}
	else if (app.count("--taxa"))
	{
	    vector<string> taxon = get_string_list(taxa);
      
	    if (taxon.size() != 3)
		throw myexception()<<"You must supply exactly 3 taxa, but you supplied "<<taxon.size();

	    int n1 = find_leaf(T,taxon[0]);
	    int n2 = find_leaf(T,taxon[1]);
	    int n3 = find_leaf(T,taxon[2]);

	    T.reroot(n1);

	    root = T.common_ancestor(n2,n3);
	}
	else if (app.count("--leaf"))
	{
	    string leaf_name = leaf_name_option;

	    root = find_leaf(T,leaf_name);
	}
	else if (app.count("--parent-of"))
	{
	    string leaf_name = parent;

	    int leaf = find_leaf(T,leaf_name);

	    root = T.branch(leaf).target();
	}
	else if (app.count("--branch"))
	{
	    string p = branch;
	    vector<string> taxa = split(p,' ');

	    dynamic_bitset<> mask(T.n_leaves());
	    dynamic_bitset<> group1(T.n_leaves());

	    auto separator = find_index(taxa,string("|"));
	    if (not separator)
		throw myexception()<<"Partition is missing a separator";

	    for(int i=0;i<*separator;i++) 
	    {
		int ii = find_leaf(T,taxa[i]);
		mask[ii] = true;
		group1[ii] = true;
	    }
	    for(int i=*separator+1;i<taxa.size();i++) 
	    {
		int ii = find_leaf(T,taxa[i]);
		mask[ii] = true;
	    }

	    Partition P(T.get_leaf_labels(),group1,mask);
	    cerr<<P<<endl;
	    int b = which_branch(T,P);
	    if (b == -1) throw myexception()<<"Can't find branch in tree!";
	    cerr<<partition_from_branch(T,b)<<endl;
	    root = split_branch(T,b);
	}
	else
	    throw myexception("Specify --outgroup, --taxa, --leaf, --parent-of, or --branch.");

    
	T.reroot(root);
	std::cout<<T<<endl;
    }
    catch (std::exception& e) {
	std::cerr<<"tree-reroot: Error! "<<e.what()<<endl;
	exit(1);
    }
    return 0;

}
