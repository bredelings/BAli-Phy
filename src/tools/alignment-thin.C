#include <iostream>
#include <fstream>
#include <string>
#include "tree.H"
#include "alignment.H"
#include "alignment-util.H"
#include "util.H"
#include "setup.H"

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
    ("align", value<string>(),"file with sequences and initial alignment")
    ("tree",value<string>(),"file with initial tree")
    ("cutoff",value<double>(),"only leave taxa w/ leaf branches longer than this")
    ("longer-than",value<unsigned>(),"only leave taxa w/ sequences longer than this")
    ("keep",value<int>(),"number of taxa to keep")
    ;

  // positional options
  positional_options_description p;
  p.add("align", 1);
  p.add("tree", 2);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-thin <alignment-file> <tree-file>\n";
    cout<<"Remove the most closely related sequences from an alignment.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (not args.count("cutoff") and not args.count("keep") and not args.count("longer-than"))
    throw myexception()<<"neither keep nor cutoff nor longer-than specified";

  return args;
}


int main(int argc,char* argv[]) 
{
  try {
    cerr.precision(10);
    cout.precision(10);
    
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //----------- Load alignment and tree ---------//
    alignment A;
    SequenceTree T;
    load_A_and_T(args,A,T,false);
    
    for(int i=0;i<T.n_branches();i++)
      if (T.branch(i).length() < 0)
	T.branch(i).set_length(-T.branch(i).length());

    //----- Standardize order by alphabetical order of names ----//
    vector<string> names;

    if (args.count("longer-than")) {
      int cutoff = args["longer-than"].as<unsigned>();
      
      for(int i=0;i<A.n_sequences();i++)
      {
	if (A.seqlength(i) > cutoff) continue;

	if (T.n_leaves() <= 3)
	  throw myexception()<<"Trying to remove too many tree leaves!";

	string name = A.seq(i).name;

	names.push_back(name);
	delete_node(T,name);	
      }
    }

    if (args.count("keep") or args.count("cutoff"))
    while(true) 
    {
      if (T.n_leaves() <= 3)
	throw myexception()<<"Trying to remove too many tree leaves!";

      if (args.count("keep") and T.n_leaves() <= args["keep"].as<int>())
	break;

      double min = T.branch(0).length();
      int argmin = 0;
      for(int i=1;i<T.n_leaves();i++) {
	if (T.branch(i).length() < min) {
	  min = T.branch(i).length();
	  argmin = i;
	}
      }

      if (args.count("cutoff") and min > args["cutoff"].as<double>())
	break;

      string name = T.seq(argmin);

      names.push_back(name);
      delete_node(T,name);
    }

    cerr<<T.write()<<"\n";

    //------- Print out the alignment -------//
    vector<sequence> sequences = A.get_sequences();
    alignment A2(A.get_alphabet());
    for(int i=0;i<A.n_sequences();i++)
      if (not includes(names,sequences[i].name))
	A2.add_sequence(sequences[i]);

    std::cout<<A2;

  }
  catch (std::exception& e) {
    cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
