#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include "myexception.H"
#include "alignment.H"
//#include "arguments.H"
#include "mytypes.H"
#include "logsum.H"
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "setup.H"
//#include "alignmentutil.H"
#include "alignment-util.H"
#include "distance-methods.H"
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

  string tag = "align[sample";
  if( args.count("tag") ) 
    tag = args["tag"].as<string>();

  vector< OwnedPointer<alphabet> > alphabets = load_alphabets(args);
  std::cerr<<"Loading alignments and trees...\n";

  //states hmmstate;
  // namespace states HMMstate;

  std::ifstream a_file(args["align"].as<string>().c_str());
  if (not a_file)
    throw myexception() << "Couldn't open file '" << args["align"].as<string>() << "'\n";
  else
    std::cerr << "Opened '" << args["align"].as<string>() << "'\n";

  std::ifstream t_file(args["tree"].as<string>().c_str());
  if (not t_file)
    throw myexception() << "Couldn't open file '" << args["tree"].as<string>() << "'\n";
  else
    std::cerr << "Opened '" << args["tree"].as<string>()  << "'\n";
  std::cerr << "Beginning analysis..." << std::endl;

  std::cout << "Iter\tPart\tLen\tIndels" << std::endl;
  
  // we are using every 'skip-th' tree to mesh with alignments
  int skip = 10;

  int totalsamples = 0;
  int consistentsamples = 0;
  int numindels = 0;

  cerr << tag << endl;

  string line;
  for(int nth=0;getline(a_file,line);) {
    //cerr << "Readline" << endl;
    // Continue with the next line IF no alignment begins here
    if (not match_tag(line,tag)) continue;

    // Increment the counter SINCE we saw an alignment
    nth++;

    // Skip this alignment IF it isn't the right multiple
    //if (nth%skip != 0) continue;

    // READ the next alignment
    alignment A;
    try {
	A.load(alphabets,sequence_format::read_guess,a_file);
    }
    catch (std::exception& e) {
      std::cerr<<"Warning: Error loading alignments, Ignoring unread alignments."<<endl;
      std::cerr<<"  Exception: "<<e.what()<<endl;
      break;
    }

    // strip out empty columns
    remove_empty_columns(A);

    // complain if there are no sequences in the alignment
    if (A.num_sequences() == 0) 
      throw myexception()<<"Alignment didn't contain any sequences!";
    
    // STORE the alignment if we're not going to skip it
    //alignments.push_back(A);

    // Read in corresponding tree and then do tree_skip
    if (!getline(t_file,line))
      throw myexception()<< "Tree file ran out of lines before alignment file.\n";
    SequenceTree T;
    T.parse(line);
    //std::cerr << T << "\n";
    for(int i=1; i<skip; i++)
      getline(t_file,line);
    
    // Start trying to link tree and alignment
    //alignment SA = standardize(A,T);  // don't standardize A to T
    alignment SA = A;
    link(SA,T,true);   // standardize T to A

    check_alignment(SA,T,"Alignment and topology are inconsistent!");

    Partition part = partition_from_names(T.get_sequences(),pnames);

    bool exists = implies(T,part);
    //std::cerr << part << "\n";    
    //std::cerr << "Does tree contain partition: " << exists << "\n";
    totalsamples++;
    std::cout << totalsamples << "\t" << exists << "\t";
    if( exists ) {
      consistentsamples++;
      int b = which_partition(T,part);
      //std::cerr << "Branch number = " << b << std::endl;
      vector<int> pairwiseA = get_path(A,T.branch(b).target(),T.branch(b).source());
      //std::cerr << pairwiseA << std::endl;
      int uniqueindels = 0;
      int laststate = states::M;
      for(int i=0; i<pairwiseA.size(); i++) {
	//std::cerr << pairwiseA[i] << " ";
	int currentstate = pairwiseA[i];
	if( (laststate != currentstate) && ((currentstate == states::G1) || (currentstate == states::G2)) ) { // BEN PLEASE CHECK THIS LINE.
	  uniqueindels++;
	}
	laststate = currentstate;
      }
      //std::cerr << " l = " << pairwiseA.size() << " u =  " << uniqueindels << std::endl;
      if( uniqueindels > 0 ) {
	numindels++;
      }
      std::cout << pairwiseA.size() << "\t" <<  uniqueindels << std::endl;
      //int nstart = T.branch(b).target();
      //int nend   = T.branch(b).source();
      //std::cerr << "Target: " << (A.seq(nstart)).name << std::endl;
      //std::cerr << "Source: " << (A.seq(nend)).name   << std::endl;
    } else {
      std::cout << "NA\t0" << std::endl;
    }

    //std::cerr<<A<<"\n";
    //std::cerr<<T<<"\n";
    //exit(0);
  }

  //alignments = load_alignments(a_file,tag,alphabets,maxalignments);
  //std::cerr<<"done. ("<<alignments.size()<<" alignments)"<<std::endl;

  std::cerr<<"Total # samples      = " << totalsamples << std::endl;
  std::cerr<<"# Consistent samples = " << consistentsamples << std::endl;
  std::cerr<<"# Indel samples      = " << numindels << std::endl;
  std::cerr<<"Posterior prob       = " << ((double)numindels/(double)totalsamples) << std::endl;

}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produces help message")
    ("align", value<string>(),"file of alignment samples")
    ("tree", value<string>(), "file of corresponding tree samples")
    ("partition", value<string>(), "find indels along internal branch that bi-partitions given taxa (<taxa1>:<taxa2>:...)")
    ("alphabet",value<string>(),"set to 'Codons' to prefer codon alphabets")
    ("with-stop","include stop codons in amino-acid alphabets")
    ("tag", value<string>()->default_value("align[sample"),"only read alignments preceded by 'align[<tag>'")
    ("datadir", value<string>()->default_value("Data"),"subdirectory that contains genetic code files")
    ;

  // positional options
  positional_options_description p;
  p.add("align", 1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: joint-A-T [OPTIONS]\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}

int main(int argc,char* argv[]) { 
  try {
    variables_map args = parse_cmd_line(argc,argv);
    //Arguments args;
    //args.read(argc,argv);
    //args.print(std::cerr);
    run_analysis(args);
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
