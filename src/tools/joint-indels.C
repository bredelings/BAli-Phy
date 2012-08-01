/*
   Copyright (C) 2005,2008-2009 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

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
#include "partition.H"
#include "dp/2way.H"

using std::cin;
using std::cout;
using std::cerr;
using std::istream;
using std::ifstream;
using std::vector;
using std::string;
using std::endl;
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
  if (log_verbose) cerr<<"joint-indels: Loading alignments and trees...\n";
  joint_A_T J = get_joint_A_T(args,true);

  if (J.size() == 0) 
    throw myexception()<<"No (A,T) read in!";
  else
    if (log_verbose) cerr<<"joint-indels: Loaded "<<J.size()<<" (A,T) pairs."<<endl;;

  std::cout << "Iter\tPart\tLen\tIndels" << endl;
  
  int consistentsamples = 0;
  int numindels = 0;

  string line;
  for(int i=0;i<J.size();i++) {

    const alignment& A = J.A[i];
    const SequenceTree& T = J.T[i];

    Partition part = full_partition_from_names(T.get_leaf_labels(),pnames);

    bool exists = implies(T,part);
    //cerr << part << "\n";
    //cerr << "Does tree contain partition: " << exists << "\n";

    std::cout << i+1 << "\t" << exists << "\t";
    if( exists ) {
      consistentsamples++;
      int b = which_branch(T,part);
      if (b == -1) throw myexception()<<"Can't find branch in tree!";
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


  if (log_verbose) {
    cerr<<"joint-indels: Total # samples      = " << J.size() << endl;
    cerr<<"joint-indels: # Consistent samples = " << consistentsamples << endl;
    cerr<<"joint-indels: # Indel samples      = " << numindels << endl;
    cerr<<"joint-indels: Posterior prob       = " << ((double)numindels/(double)J.size()) << endl;
  }
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
    ("verbose","Output more log messages on stderr.")
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

  if (args.count("verbose")) log_verbose = 1;

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
    cerr<<"joint-indels: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
