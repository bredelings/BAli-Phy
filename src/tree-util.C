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
  SequenceTree T = remove_root( RT );
  return T;
}


