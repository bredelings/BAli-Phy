/*
  Copyright (C) 2007-2009 Benjamin Redelings

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

// FIXME -  Try to put the variance and stuff on one line:
//   0.656 (56/100)   +- 0.007 -> 0.070

#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <list>
#include <set>
#include <map>
#include <cmath>
#include <fstream>
#include <sstream>
#include <map>
#include <list>

#include "tree/sequencetree.H"
#include "util.H"
#include "statistics.H"
#include "tree-dist.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;
using std::ios;


using namespace statistics;

// What if everything in 'split' is true?
// What if everything in 'split' is true, but 1 taxa?
//  These are true by definition...

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    // named options
    options_description invisible("Invisible options");
    invisible.add_options()
	("predicates",value<string>(),"predicates to examine")
	("file",value<string>(),"tree samples to examine");

    options_description input("Input options");
    input.add_options()
	("help", "produce help message")
	("skip",value<unsigned>()->default_value(0),"number of trees to skip")
	("max",value<unsigned>(),"maximum number of trees to read")
	("sub-sample",value<unsigned>(),"factor by which to sub-sample")
	("verbose","Output more log messages on stderr.")
	;
  
    options_description reporting("Reporting options");
    reporting.add_options()
	("not,n", "invert the results")
	("below,b", value<double>(),"only report partitions with PP < arg")
	("above,a",value<double>(), "only report partitions with PP > arg")
	;
  
    options_description all("All options");
    all.add(invisible).add(input).add(reporting);

    // positional options
    positional_options_description p;
    p.add("predicates", 1);
    p.add("file", 2);
  
    variables_map args;     
    store(command_line_parser(argc, argv).
	  options(all).positional(p).run(), args);
    // store(parse_command_line(argc, argv, desc), args);
    notify(args);    

    if (args.count("help")) {
	cout<<"Usage: trees-bootstrap <partitions-file> <trees-file> [OPTIONS]\n";
	cout<<"Select only partitions with support in the specified range.\n\n";
	cout<<input<<reporting<<"\n";
	exit(0);
    }

    if (not args.count("predicates"))
	throw myexception()<<"No predicates supplied.";

    if (args.count("verbose")) log_verbose = 1;

    return args;
}

tree_sample load_tree_file(const variables_map& args, const string& filename)
{
    int skip = args["skip"].as<unsigned>();

    int max = -1;
    if (args.count("max"))
	max = args["max"].as<unsigned>();

    int subsample=1;
    if (args.count("sub-sample"))
	subsample = args["sub-sample"].as<unsigned>();

    if (filename == "-") {
	if (log_verbose) cerr<<"partitions-supported: Loading trees from STDIN...\n";
	return tree_sample(cin,skip,subsample,max);
    }

    checked_ifstream file(filename,"tree samples file");
  
    cerr<<"partitions-supported: Loading trees from '"<<filename<<"'...\n";
    return tree_sample(file,skip,subsample,max);
}

int main(int argc,char* argv[]) 
{ 
    try {

	cout.precision(3);
	cout.setf(ios::fixed);

	//---------- Parse command line  -------//
	variables_map args = parse_cmd_line(argc,argv);

	bool flip = false;
	if (args.count("not")) flip = true;

	//-------------- Read in tree distribution --------------//
	string filename = "-";
	if (args.count("file"))
	    filename = args["file"].as<string>();

	tree_sample trees = load_tree_file(args,filename);

	//--------- compute upper and lower bounds -----------------//
	unsigned S = trees.size();
	unsigned upper = S;
	unsigned lower = S;
	if (args.count("below")) {
	    upper = (unsigned)floor(S * args["below"].as<double>());
	    upper = std::min(upper,S);
	}
	if (args.count("above")) {
	    lower = (unsigned) ceil(S * args["above"].as<double>());
	    lower = std::max(lower,0U);
	}

	//----------- Load Partitions ---------------//
	auto predicates = load_partitions(args["predicates"].as<string>());

	predicates = remove_duplicates(predicates);

	//------- evaluate/cache predicate for each topology -------//
	vector<unsigned> support;

	for(const auto& predicate: predicates)
	{
	    unsigned c = count(trees.support(predicate.partitions));
	    support.push_back(c);
	}

	for(int p=0;p<predicates.size();p++) 
	{
	    bool match = (lower <= support[p] and support[p] <= upper);
	    if (flip) match = not match;
	    if (match) {
		for(int i=0;i<predicates[p].partitions.size();i++) 
		    cout<<Partition(trees.names(), predicates[p].partitions[i])<<endl;
		cout<<endl;
	    }
	}
    }
    catch (std::exception& e) {
	cerr<<"partitions-supported: Error! "<<e.what()<<endl;
	exit(1);
    }
    return 0;
}
