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
    cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
