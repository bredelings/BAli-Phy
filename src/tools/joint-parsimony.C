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

#include "2way.H"
using namespace A2;

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

// If we could read back in the indel-model state we could
// estimate the number of indels in an indel block.
unsigned n_indels(const alignment& A,const Tree& T, int b)
{
  vector<int> pairwiseA = get_path(A, T.branch(b).target(), T.branch(b).source());

  unsigned indels = 0;
  int last_state = states::M;
  for(unsigned i=0; i<pairwiseA.size(); i++) 
  {
    int current_state = pairwiseA[i];
    
    if (last_state != current_state)
      if ((current_state == states::G1) or (current_state == states::G2))
	indels++;
  }
  return indels;
}

unsigned n_indels(const alignment& A,const Tree& T)
{
  unsigned indels = 0;
  for(unsigned b=0;b<T.n_branches();b++)
    indels += n_indels(A,T,b);

  return indels;
}


int main(int argc,char* argv[]) { 
  try {
    variables_map args = parse_cmd_line(argc,argv);

    joint_A_T J = get_joint_A_T(args,false);

    for(int i=0;i<J.size();i++) {
      const alignment& A = J.A[i];
      const SequenceTree& T = J.T[i];

      cout<<"length = "<<n_mutations(A,T)<<"   ";
      
      cout<<" n_indels = "<<n_indels(A,T)<<endl;
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
