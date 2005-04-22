#include "tree-util.H"
#include "myexception.H"

using boost::program_options::variables_map;
using std::string;

/// Load a tree from command line args "--tree filename"
SequenceTree load_T(const variables_map& args) {
  if (not args.count("tree"))
    throw myexception()<<"Tree file not specified! (--tree <filename>)";
    
  RootedSequenceTree RT;
  RT.read(args["tree"].as<string>());

  // FIXME - but what if I WANT the node there?
  SequenceTree T;
  if (RT.root().neighbors().size() == 2)
    T = remove_root(RT);
  else
    T = RT;
  return T;
}


