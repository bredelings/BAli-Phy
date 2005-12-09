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

using std::cin;
using std::cout;
using std::cerr;
using std::istream;
using std::ifstream;
using std::vector;

namespace po = boost::program_options;
using po::variables_map;

//--------------------------- Class joint_A_T ----------------------------//
struct joint_A_T {
  int size() {return A.size();}

  vector<alignment> A;
  vector<SequenceTree> T;

  joint_A_T(const vector<alignment>& A1,const vector<SequenceTree>& T1,bool internal);
};

joint_A_T::joint_A_T(const vector<alignment>& A1,const vector<SequenceTree>& T1,bool internal)
  :A(A1),T(T1)
{
  unsigned s = min(A.size(),T.size());
  if (s != A.size())
    std::cerr<<"Warning: only using "<<s<<"/"<<A.size()<<" alignments to match number of trees."<<endl;

  if (s != T.size())
    std::cerr<<"Warning: only using "<<s<<"/"<<T.size()<<" trees to match number of alignments."<<endl;

  A.resize(s);
  T.resize(s);

  for(int i=0;i<size();i++) {
    link(A[i],T[i],true);
    link(A[i],T[i],internal);
  }
}


joint_A_T get_joint_A_T(const variables_map& args,unsigned subsample,bool internal)
{
  std::cerr<<"Loading alignments and trees...\n";

  std::ifstream a_file(args["alignments"].as<string>().c_str());
  if (not a_file)
    throw myexception() << "Couldn't open file '" << args["alignments"].as<string>() << "'\n";

  std::ifstream t_file(args["trees"].as<string>().c_str());
  if (not t_file)
    throw myexception() << "Couldn't open file '" << args["trees"].as<string>() << "'\n";

  vector<alignment> A = load_alignments(a_file, load_alphabets(args));
  vector<SequenceTree> T = load_trees(t_file, 0, subsample);

  return joint_A_T(A,T,internal);
}


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

    joint_A_T J = get_joint_A_T(args,args["subsample"].as<unsigned>(),false);

    for(int i=0;i<J.size();i++)
      cout<<n_mutations(J.A[i],J.T[i])<<endl;
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
