#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <list>
#include <ext/hash_map>
#include <map>
#include <cmath>

#include "myexception.H"
#include "sequencetree.H"
#include "tree-dist.H"
#include "tree-util.H"
#include "statistics.H"

using namespace std;
#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;
using std::string;

string topology(const string& t) {
  SequenceTree T = standardized(t);
  return T.write(false);
}

string topology(const SequenceTree& T) {
  SequenceTree T2 = T;
  standardize(T2);
  return T2.write(false);
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("tree", value<string>(),"tree of partitions to consider")
    ("skip",value<int>()->default_value(0),"number of trees to skip")
    ("max",value<int>(),"maximum number of trees to read")
    ("mode", value<string>()->default_value("SRQ"),"SRQ or sum")
    ("invert","consider the inverse of each event instead")
    ("no-scale-x","don't scale X")
    ("no-scale-y","don't scale Y")
    ;

  // positional options
  positional_options_description p;
  p.add("tree", 1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: tree-to-srq <tree-file> < in-file\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}

int main(int argc,char* argv[]) 
{ 
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    // Load the partitions that we are considering
    SequenceTree T = load_T(args);
    standardize(T);
    vector<Partition> partitions;
    for(int b=T.n_leafbranches();b<T.n_branches();b++) {
      std::valarray<bool> p1 = branch_partition(T,b);
      partitions.push_back(Partition(T.get_sequences(),p1));
    }

    // Read in the trees
    int skip = args["skip"].as<int>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<int>();

    tree_sample tree_dist(std::cin,skip,max);


    // Compute info for plots
    vector<vector<int> > plots(partitions.size());
    for(int i=0;i<partitions.size();i++) {
      valarray<bool> support = tree_dist.supports_partition(partitions[i]);

      if (args.count("invert")) support = not support;

      if (args["mode"].as<string>() == "SRQ")
	plots[i] = statistics::regeneration_times(support);
      else 
	plots[i] = statistics::total_times(support);
    }

    // write out plots
    for(int i=0;i<plots.size();i++) {
      double scale_x = plots[i].size()-1;
      double scale_y = plots[i].back();
      if (scale_y == 0) scale_y = 1;
      
      if (args.count("no-scale-x"))
	scale_x = 1;

      if (args.count("no-scale-y"))
	scale_y = 1;

      for(int j=0;j<plots[i].size();j++) 
	cout<<j/scale_x<<"   "<<plots[i][j]/scale_y<<endl;
      cout<<endl;
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
