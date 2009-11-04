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

#include "sequencetree.H"
#include "tree-dist.H"
#include "tree-util.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using namespace std;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description input("Input options");
  input.add_options()
    ("help", "produce help message")
    ("all","show only informative partitions")
    ("tree",value<string>(),"tree file")
    ;
  
  options_description all("All options");
  all.add(input);

  // positional options
  positional_options_description p;
  p.add("tree", -1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: tree-partitions <file> [OPTIONS]\n";
    cout<<"Get partitions from a tree.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}

int main(int argc,char* argv[]) 
{ 
  try 
  {
    //------------- Parse command line  --------------//
    variables_map args = parse_cmd_line(argc,argv);

    SequenceTree T = load_T(args);
    standardize(T);

    //-------------- Load Partitions -----------------//
    vector<Partition> partitions;
    int start = 0;
    if (not args.count("all"))
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
