/*
   Copyright (C) 2006,2008 Benjamin Redelings

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
#include <string>
#include <vector>
#include <fstream>

#include "tree/sequencetree.hh"
#include "tree-dist.hh"
#include "tree/tree-util.hh"

#include <CLI/CLI.hpp>


using namespace std;


void describe_tree(const SequenceTree& T)
{
  cout<<"Nodes = "<<T.n_nodes()
      <<"   internal = "<<T.n_nodes() - T.n_leaves()
      <<"   leaves = "<<T.n_leaves()<<"\n";
  cout<<"Branches = "<<T.n_branches()
      <<"   internal = "<<T.n_branches() - T.n_leafbranches()
      <<"   leaf = "<<T.n_leafbranches()<<"\n";
  cout<<"Labels = "<<T.get_leaf_labels().size()<<"\n";
  int D2 = 0;
  int N13 = 0;
  for(int i=0;i<T.n_nodes();i++)
  {
    int d = T.node(i).degree();
    if (d == 2)
      D2++;
    if (d != 1 and d != 3)
      N13++;
  }
  cout<<"degree 2 nodes = "<<D2<<endl;
  cout<<"degree 4+ nodes = "<<N13-D2<<endl;
  cout<<"Simple bifurcating: ";
  if (N13 == 0 and T.n_branches() == 2*T.n_leaves()-3)
    cout<<"yes\n";
  else
    cout<<"no\n";
  cout<<"Multifurcating: ";
  if (N13 > D2)
    cout<<"yes";
  else
    cout<<"no";
  cout<<endl;
}

int main(int argc,char* argv[]) 
{ 
  try 
  {
    //------------- Parse command line  --------------//
    CLI::App app{"Print branch partitions or information about one tree.", "tree-partitions"};
    app.usage("Usage: tree-partitions TREE [OPTIONS]");
    string tree_file;
    double scale = 1;
    bool all = false, info = false;
    app.add_option("TREE", tree_file, "Tree file (Newick or NEXUS)")->required()->type_name("");
    app.add_flag("--all", all, "Include leaf partitions as well as internal partitions");
    app.add_flag("--info", info, "Print tree information instead of partitions");
    app.add_option("--scale", scale, "Multiply branch lengths and print the tree");
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


    SequenceTree T = load_tree_from_file(tree_file);
    standardize(T);

    //-------------- Load Partitions -----------------//
    vector<Partition> partitions;
    int start = 0;
    if (info)
    {
      describe_tree(T);
      exit(0);
    }
    else if (app.count("--scale"))
    {
      for(int b=0;b<T.n_branches();b++)
	T.branch(b).set_length(scale*T.branch(b).length());
      cout<<T.write()<<endl;
      exit(0);
    }

    if (not all)
      start = T.n_leafbranches();

    for(int b=start;b<T.n_branches();b++)
      partitions.push_back(partition_from_branch(T,b));

    //----- Print out support for each partition -----//
    for(int p=0;p<partitions.size();p++) 
      cout<<partitions[p]<<endl<<endl;
  }
  catch (std::exception& e) {
    cerr<<"tree-partitions: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
