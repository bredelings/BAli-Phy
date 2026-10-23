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

#include <CLI/CLI.hpp>
#include <optional>

#include "tree/sequencetree.hh"
#include "util/set.hh"
#include "util/myexception.hh"
#include "statistics.hh"
#include "tree-dist.hh"

extern int log_verbose;


using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;
using std::ios;
using std::optional;

using namespace statistics;

// What if everything in 'split' is true?
// What if everything in 'split' is true, but 1 taxa?
//  These are true by definition...



// Load the selected trees from a file or stdin, keeping the existing selection order.
tree_sample load_tree_file(const string& filename, int skip, optional<int> last, int subsample, optional<int> max)
{
    if (filename == "-") {
	if (log_verbose) cerr<<"partitions-supported: Loading trees from STDIN...\n";
	return tree_sample(cin,skip,last,subsample,max);
    }

    checked_ifstream file(filename,"tree samples file");
  
    cerr<<"partitions-supported: Loading trees from '"<<filename<<"'...\n";
    return tree_sample(file,skip,last,subsample,max);
}

int main(int argc,char* argv[]) 
{ 
    try {

	cout.precision(3);
	cout.setf(ios::fixed);

	//---------- Parse command line  -------//
        CLI::App app{"Select partitions with support in the specified range.", "partitions-supported"};
        app.usage("Usage: partitions-supported <partitions-file> [trees-file] [OPTIONS]");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        string predicates_file, filename = "-";
        unsigned skip = 0, subsample = 1;
        optional<unsigned> last, max;
        optional<double> below, above;
        bool flip = false, verbose = false;
        app.add_option("--predicates,PREDICATES", predicates_file, "predicates to examine")->required();
        app.add_option("--file,FILE", filename, "tree samples to examine")->capture_default_str();
        app.add_option("--skip", skip, "Number of trees to skip.")->capture_default_str()->group("Input options");
        app.add_option("--until", last, "Read until this number of trees.")->group("Input options");
        app.add_option("--max", max, "Thin down to this number of trees.")->group("Input options");
        app.add_option("--subsample", subsample, "Factor by which to sub-sample.")
            ->capture_default_str()->group("Input options");
        app.add_flag("--verbose", verbose, "Output more log messages on stderr.")->group("Input options");
        app.add_flag("-n,--not", flip, "invert the results")->group("Reporting options");
        app.add_option("-b,--below", below, "only report partitions with PP < arg")->group("Reporting options");
        app.add_option("-a,--above", above, "only report partitions with PP > arg")->group("Reporting options");
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }
        if (verbose) log_verbose = 1;

	//-------------- Read in tree distribution --------------//
	tree_sample trees = load_tree_file(filename, skip, last, subsample, max);

	//----------- Load Partitions ---------------//
	auto predicates = load_partitions(predicates_file);

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
	    const double probability = double(support[p])/trees.size();
	    bool match = (not above or probability > *above)
	             and (not below or probability < *below);
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
