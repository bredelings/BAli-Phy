#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <cmath>
#include <fstream>
#include <boost/numeric/ublas/io.hpp>

#include "mytypes.H"
#include "myexception.H"
#include "sequencetree.H"
#include "util.H"
#include "bootstrap.H"
#include "tree-dist.H"
#include "mctree.H"

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
    ("file",value<string>(),"predicates to examine")
    ;
  
  options_description all("All options");
  all.add(input);

  // positional options
  positional_options_description p;
  p.add("file", -1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: draw-graph <file1>\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (not args.count("file"))
    throw myexception()<<"No file supplied.";

  return args;
}

int main(int argc,char* argv[]) 
{ 
  try {

    cout.precision(3);
    cout.setf(ios::fixed);

    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //----------- Load Partitions ---------------//
    vector<vector<Partition> > partitions1;
    string filename = args["file"].as<string>();
    load_partitions(filename, partitions1);

    vector<Partition> partitions = partitions1[0];

    // check that the taxon names are all the same
    vector<string> names = partitions[0].names;
    for(int i=0;i<partitions.size();i++) {

      if (partitions[i].size() != names.size())
	throw myexception()<<"Partition "<<i+1<<" has "<<partitions[i].size()<<" taxa instead of "<<partitions[0].size();

      if (partitions[i].names != names)
	throw myexception()<<"Partition "<<i+1<<" has different taxa than partition 1!";
      if (not informative(partitions[i]))
	throw myexception()<<"Partition "<<i+1<<" is not informative.";
    }

    //---- Throw out conflicting partitions ----//
    vector<Partition> partitions_old = partitions;
    partitions = get_moveable_tree(partitions);
    // check that the tree is an MC tree
    if (partitions.size() != partitions_old.size())
      cerr<<"Removing "<<partitions_old.size() - partitions.size()<<"/"<<partitions_old.size()<<" partitions to yield an MC  tree."<<endl;

    // add leaf branches
    for(int i=0;i<names.size();i++) {
      valarray<bool> m(true,names.size());
      m[i] = false;
      partitions.insert(partitions.begin()+i,Partition(names,m));
    }

    MC_tree T(partitions);

    // remove the pathname 
    while(filename.find('/') != -1) 
      filename = filename.substr(filename.find('/')+1);

    // remove the extension
    int dot = filename.find('.');
    string name = filename;
    if (dot != -1)
      name = filename.substr(0,dot);

    //draw the graph
    draw_graph(T,name);
  }
  catch (std::exception& e) {
    cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}

