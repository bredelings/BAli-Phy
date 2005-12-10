#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include <boost/program_options.hpp>

#include "myexception.H"
#include "alignment.H"

#include "mytypes.H"
#include "logsum.H"
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "setup.H"

#include "alignment-util.H"
#include "tree-util.H"
#include "parsimony.H"
#include "joint-A-T.H"

using std::cin;
using std::cout;
using std::cerr;
using std::istream;
using std::ifstream;
using std::vector;

namespace po = boost::program_options;
using po::variables_map;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description invisible("Invisible options");
  invisible.add_options()
    ("alignments", value<string>(),"file of alignment samples")
    ("trees", value<string>(), "file of corresponding tree samples")
    ;

  options_description visible("All options");
  visible.add_options()
    ("help", "produces help message")
    ("subsample",value<unsigned>()->default_value(10),"factor by which to sub-sample trees")
    ("alphabet",value<string>(),"set to 'Codons' to prefer codon alphabets")
    ("data-dir", value<string>()->default_value("Data"),"subdirectory that contains genetic code files")
    ;

  options_description all("All options");
  all.add(visible).add(invisible);

  // positional options
  positional_options_description p;
  p.add("alignments", 1);
  p.add("trees", 2);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: joint-parsimony <alignments file> <trees file> [OPTIONS]\n";
    cout<<visible<<"\n";
    exit(0);
  }

  return args;
}


int main(int argc,char* argv[]) { 
  try {
    variables_map args = parse_cmd_line(argc,argv);

    joint_A_T J = get_joint_A_T(args,false);

    for(int i=0;i<J.size();i++)
      cout<<n_mutations(J.A[i],J.T[i])<<endl;
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
