#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include "myexception.H"
#include "alignment.H"

#include "mytypes.H"
#include "logsum.H"
#include "util.H"
#include "setup.H"

#include "alignment-util.H"
#include "distance-methods.H"
#include "joint-A-T.H"
#include "tree-dist.H"
#include "2way.H"

using std::cin;
using std::cout;
using std::cerr;
using std::istream;
using std::ifstream;
using std::vector;
//using namespace optimize;

namespace po = boost::program_options;
using po::variables_map;
using namespace A2;

void run_analysis(const variables_map& args) {

  // Handle Partition name
  if (not args.count("partition"))
    throw myexception() << "Must specify a unique partition of taxa by name.\n";
  vector<string> pnames = split(args["partition"].as<string>(),':');

  //--------------------- Load (A,T) ------------------------//
  cerr<<"Loading alignments and trees...\n";
  joint_A_T J = get_joint_A_T(args,true);

  if (J.size() == 0) 
    throw myexception()<<"No (A,T) read in!";
  else
    cerr<<"Loaded "<<J.size()<<" (A,T) pairs."<<endl;;

  std::cout << "Iter\tPart\tLen\tIndels" << endl;
  
  int consistentsamples = 0;
  int numindels = 0;

  string line;
  for(int i=0;i<J.size();i++) {

    const alignment& A = J.A[i];
    const SequenceTree& T = J.T[i];

    Partition part = full_partition_from_names(T.get_sequences(),pnames);

    bool exists = implies(T,part);
    //cerr << part << "\n";
    //cerr << "Does tree contain partition: " << exists << "\n";

    std::cout << i+1 << "\t" << exists << "\t";
    if( exists ) {
      consistentsamples++;
      int b = which_partition(T,part);
      //cerr << "Branch number = " << b << endl;
      vector<int> pairwiseA = get_path(A,T.branch(b).target(),T.branch(b).source());
      //cerr << pairwiseA << endl;

      int uniqueindels = 0;
      int laststate = states::M;
      for(int i=0; i<pairwiseA.size(); i++) {
	//cerr << pairwiseA[i] << " ";
	int currentstate = pairwiseA[i];
	if( (laststate != currentstate) and ((currentstate == states::G1) or (currentstate == states::G2)) ) { // This is correct - BEN
	  uniqueindels++;
	}
	laststate = currentstate;
      }
      //cerr << " l = " << pairwiseA.size() << " u =  " << uniqueindels << endl;
      if( uniqueindels > 0 ) {
	numindels++;
      }
      std::cout << pairwiseA.size() << "\t" <<  uniqueindels << endl;
      //int nstart = T.branch(b).target();
      //int nend   = T.branch(b).source();
      //cerr << "Target: " << (A.seq(nstart)).name << endl;
      //cerr << "Source: " << (A.seq(nend)).name   << endl;
    } else {
      std::cout << "NA\t0" << endl;
    }

    //cerr<<A<<"\n";
    //cerr<<T<<"\n";
    //exit(0);
  }


  cerr<<"Total # samples      = " << J.size() << endl;
  cerr<<"# Consistent samples = " << consistentsamples << endl;
  cerr<<"# Indel samples      = " << numindels << endl;
  cerr<<"Posterior prob       = " << ((double)numindels/(double)J.size()) << endl;

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

  // named options
  options_description visible("Allowed options");
  visible.add_options()
    ("help", "produces help message")
    ("subsample",value<unsigned>()->default_value(10),"factor by which to sub-sample trees")
    ("partition", value<string>(), "find indels along internal branch that bi-partitions given taxa (<taxa1>:<taxa2>:...)")
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
    cout<<"Usage: joint-indels <alignments file> <trees file> [OPTIONS]\n";
    cout<<visible<<"\n";
    exit(0);
  }

  return args;
}

int main(int argc,char* argv[]) { 
  try {
    variables_map args = parse_cmd_line(argc,argv);
    //Arguments args;
    //args.read(argc,argv);
    //args.print(cerr);
    run_analysis(args);
  }
  catch (std::exception& e) {
    cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
