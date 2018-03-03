/*
  Copyright (C) 2004-2006,2008 Benjamin Redelings

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

#include <iostream>
#include <fstream>
#include <string>
#include "tree/tree.H"
#include "tree/tree-util.H"
#include "alignment/load.H"
#include "sequence/sequence-format.H"
#include "util.H"
#include "setup.H"
#include "findroot.H"

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
	("help,h", "produce help message")
	("align", value<string>(),"file with sequences and initial alignment")
	("alphabet",value<string>(),"set to 'Codons' to prefer codon alphabets")
	("tree",value<string>(),"file with tree that specifies leaves to keep")
	("nleaves,N",value<int>(),"number of sequences to keep")
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
	cout<<"Remove ancestral sequences from an alignment.\n\n";
	cout<<"Usage: alignment-chop-internal [OPTIONS] < <alignments-file>\n\n";
	cout<<all<<"\n";
	exit(0);
    }

    return args;
}


int main(int argc,char* argv[]) 
{ 

    try {
	//---------- Parse command line  -------//
	variables_map args = parse_cmd_line(argc,argv);

	//------- Determine number of leaf sequences to keep --------//
	if (args.count("nleaves") and args.count("tree"))
	    throw myexception()<<"You can't specify both 'nleaves' and 'tree'!";

	int N = -1;
	if (args.count("nleaves"))
	    N = args["nleaves"].as<int>();
	else if (args.count("tree"))
	    N = load_T(args).n_leaves();
	else
	    throw myexception()<<"Both 'n_leaves' nor 'tree' unspecified!";
	
	//------ Read sequences and chop off non-leaf sequences -----//
	while (auto sequences = find_load_next_sequences(std::cin))
	{
	    if (sequences->size() < N)
		throw myexception()<<"Trying to keep "<<N<<" leaf sequences, but only got "<<sequences->size()<<"!";

	    sequences->resize(N);

	    sequence_format::write_fasta(std::cout, *sequences);
	}
    }
    catch (std::exception& e) {
	std::cerr<<"alignment-chop-internal: Error! "<<e.what()<<endl;
	exit(1);
    }
    return 0;

}
