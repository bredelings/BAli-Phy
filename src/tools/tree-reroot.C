#include <iostream>
#include "tree.H"
#include "sequencetree.H"
#include "tree-util.H"
#include "myexception.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;
using std::string;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("tree", value<string>(),"tree to re-root")
    ("outgroup", value<string>(),"sequence to use as the outgroup")
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
    cout<<"Usage: tree-reroot <tree-file> <outgroup>\n";
    cout<<"Placed the root on the external branch leading to <outgroup>.\n\n";
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

    SequenceTree T = load_T(args);
    
    if (not args.count("outgroup"))
      throw myexception("outgroup not specified!");

    int leaf=-1;
    for(int i=0;i<T.n_leaves();i++) {
      if (T.seq(i) == args["outgroup"].as<string>()) {
	leaf=i;
	break;
      }
    }

    if (leaf == -1)
      throw myexception()<<"outgroup '"<<args["outgroup"].as<string>()<<"' not found in tree!";

    std::cout<<add_root(T,leaf)<<endl;
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
