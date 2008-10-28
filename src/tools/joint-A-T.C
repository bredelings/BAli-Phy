#include "joint-A-T.H"

#include <fstream>
#include <cmath>

#include "myexception.H"
#include "alignment.H"

#include "mytypes.H"
#include "logsum.H"
#include "util.H"
#include "setup.H"

#include "alignment-util.H"
#include "tree-util.H"

namespace po = boost::program_options;
using po::variables_map;

using std::vector;

//--------------------------- Class joint_A_T ----------------------------//

joint_A_T::joint_A_T(const vector<alignment>& A1,const vector<SequenceTree>& T1,bool internal)
  :A(A1),T(T1)
{
  unsigned s = min(A.size(),T.size());
  if (s != A.size())
    std::cerr<<"joint-A-T: Warning! only using "<<s<<"/"<<A.size()<<" alignments to match number of trees."<<endl;

  if (s != T.size())
    std::cerr<<"joint-A-T: Warning! only using "<<s<<"/"<<T.size()<<" trees to match number of alignments."<<endl;

  A.resize(s);
  T.resize(s);

  for(int i=0;i<size();i++) {
    link(A[i],T[i],true);
    link(A[i],T[i],internal);
  }
}


joint_A_T get_joint_A_T(const variables_map& args,bool internal)
{
  std::ifstream a_file(args["alignments"].as<string>().c_str());
  if (not a_file)
    throw myexception() << "Couldn't open file '" << args["alignments"].as<string>() << "'\n";

  std::ifstream t_file(args["trees"].as<string>().c_str());
  if (not t_file)
    throw myexception() << "Couldn't open file '" << args["trees"].as<string>() << "'\n";

  unsigned subsample = args["subsample"].as<unsigned>();

  vector<alignment> A = load_alignments(a_file, load_alphabets(args));
  vector<SequenceTree> T = load_trees(t_file, 0, subsample);

  return joint_A_T(A,T,internal);
}
