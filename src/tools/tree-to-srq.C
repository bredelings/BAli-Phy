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

using namespace std;

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;
using std::string;

double moment(const vector<double>& v,int n) {
  double total=0.0;
  for(int i=0;i<v.size();i++) {
    double temp = 1.0;
    for(int j=0;j<n;j++)
      temp *= v[i];
    total += temp;
  }
  return total/v.size();
}

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
    ("tree", value<string>(),"tree to re-root")
    ;

  // positional options
  positional_options_description p;
  p.add("tree", 1);
  p.add("outgroup", 2);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: tree-to-srq <tree-file> < in-file\n";
    //    cout<<all<<"\n";
    exit(0);
  }

  return args;
}

int main(int argc,char* argv[]) 
{ 
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    // Load the target tree
    SequenceTree target = load_T(args);

    string target_string = topology(target);

    string line;
    while(getline(cin,line)) {
      SequenceTree T = standardized(line);
      if (T.write(false) == target_string)
	cout<<"1\n";
      else
	cout<<"0\n";
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
