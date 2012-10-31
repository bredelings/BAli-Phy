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
#include "tree/tree.H"
#include "tree-dist.H"
#include "tree/sequencetree.H"
#include "util.H"
#include "tree/tree-util.H"
#include "myexception.H"

#include <boost/program_options.hpp>

using namespace std;
namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;
using std::string;

using boost::dynamic_bitset;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("tree", value<string>(),"tree to re-root")
    ("outgroup", value<string>(),"Add root in middle of branch to this leaf")
    ("leaf", value<string>(),"Root tree at this leaf")
    ("taxa",value<string>(),"3 sequences to triangulate node position")
    ("parent-of",value<string>(),"Root at parent of this leaf taxon")
    ("branch",value<string>(),"Add root in middle of this branch (partition)")
    ;

  // positional options
  positional_options_description p;
  p.add("tree", 1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: tree-reroot <tree-file> <outgroup>\n";
    cout<<"Placed the root on the external branch leading to <outgroup>.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}


int find_leaf(const SequenceTree& T,const string& name)
{
  int index = T.index(name);
  if (index == -1)
    throw myexception()<<"Can't find taxon '"<<name<<"' in tree";

  return index;
}

int split_branch(Tree& T,int b)
{
  double L = T.branch(b).length();

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
    variables_map args = parse_cmd_line(argc,argv);

    RootedSequenceTree T = load_T(args);
    
    int root=-1;
    if (args.count("outgroup")) 
    {
      string outgroup = args["outgroup"].as<string>();

      int leaf = find_leaf(T,outgroup);

      root = split_branch(T,leaf);
    }
    else if (args.count("taxa"))
    {
      string taxa = args["taxa"].as<string>();

      vector<string> taxon = split(taxa,',');
      
      if (taxon.size() != 3)
	throw myexception()<<"You must supply exactly 3 taxa, but you supplied "<<taxon.size();

      int n1 = find_leaf(T,taxon[0]);
      int n2 = find_leaf(T,taxon[1]);
      int n3 = find_leaf(T,taxon[2]);

      T.reroot(n1);

      root = T.common_ancestor(n2,n3);
    }
    else if (args.count("leaf"))
    {
      string leaf_name = args["leaf"].as<string>();

      root = find_leaf(T,leaf_name);
    }
    else if (args.count("parent-of"))
    {
      string leaf_name = args["parent-of"].as<string>();

      int leaf = find_leaf(T,leaf_name);

      root = T.branch(leaf).target();
    }
    else if (args.count("branch"))
    {
      string p = args["branch"].as<string>();
      vector<string> taxa = split(p,' ');

      dynamic_bitset<> mask(T.n_leaves());
      dynamic_bitset<> group1(T.n_leaves());

      int separator = find_index(taxa,string("|"));
      if (separator == -1)
	throw myexception()<<"Partition is missing a separator";

      for(int i=0;i<separator;i++) 
      {
	int ii = find_leaf(T,taxa[i]);
	mask[ii] = true;
	group1[ii] = true;
      }
      for(int i=separator+1;i<taxa.size();i++) 
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
      throw myexception("neither --outgroup nor --taxa nor --leaf specified!");

    
    T.reroot(root);
    std::cout<<T<<endl;
  }
  catch (std::exception& e) {
    std::cerr<<"tree-reroot: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
