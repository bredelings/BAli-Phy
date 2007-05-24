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

    //----------- Parse command line  -----------//
    variables_map args = parse_cmd_line(argc,argv);

    //------------ Load Partitions --------------//
    string filename = args["file"].as<string>();
    MC_tree T = load_MC_tree(filename);
    
    //------------ Draw the graph ---------------//
    string name = get_graph_name(filename);
    draw_graph(T,name);
  }
  catch (std::exception& e) {
    cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}

