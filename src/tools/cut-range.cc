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

#include "util/assert.hh"

#include <string>
#include <sstream>
#include <iostream>
#include "util/string/convert.H"
#include "util/io.H"
#include "util/log-level.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::string;
using std::vector;
using std::cout;
using std::optional;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    // named options
    options_description invisible("Invisible options");
    invisible.add_options()
	("filenames", value<vector<string>>()->composing(),"Filenames to read")
	;

    options_description visible("All options");
    visible.add_options()
	("help,h", "produce help message")
	("key", value<string>()->default_value("iterations"),"cut based on values of <key>=value")
	("skip",value<int>(),"the number of samples to skip")
	("until",value<int>(),"last sample to use")
	("size",value<int>(),"maximum number of samples to use")
	("verbose","Output more log messages on stderr.")
	;

    options_description all("All options");
    all.add(invisible).add(visible);

    // positional options
    positional_options_description p;
    p.add("filenames", -1);

    variables_map args;     
    store(command_line_parser(argc, argv).options(all).positional(p).run(), args);
    notify(args);    

    if (args.count("help")) {
	cout<<"Select lines from multiple input files based on lines containing `key = value`.\n\n";
	cout<<"Usage: cut-range filename1.fastas [filename2.fastas ...] [OPTIONS]\n\n";
	cout<<visible;
        cout<<"\n";
        cout<<"The selection options (e.g. --skip) apply to each input file separately.\n";
        cout<<"\n";
	cout<<"Examples:\n";
	cout<<"\n";
	cout<<" Select alignments after the first 100 iterations from two different runs:\n";
        cout<<"   % cut-range --skip=100 run-1/C1.P1.fastas run-2/C1.P1.fastas > P1.fastas\n\n";
	exit(0);
    }

    if (args.count("verbose")) log_verbose = 1;

    if (not args.count("key")) 
	throw myexception()<<"argument 'key' not set.";

    return args;
}


void cut_range(std::istream& in, std::ostream& out,
	       const string& key, optional<int> min, optional<int> max)
{
    string pattern = key + " = ";

    string line;

    bool in_interval = not min;
    while(portable_getline(in,line))
    {
	// look for the pattern
	int where = line.find(pattern);

	// if no pattern, then
	if (where != -1)
	{
	    // move PAST the pattern
	    where += pattern.size();
	    double value = convertTo<double>(line.substr(where));
	    in_interval = true;
	    if (min and value <= *min)
		in_interval = false;

	    if (max and value > *max)
		return;

	    //    std::cerr<<line<<std::endl;
	    //    std::cerr<<"where = "<<where<<std::endl;
	    //    std::cerr<<value<<std::endl;
	}

	if (in_interval)
	    out<<line<<std::endl;
    }
}




// assumptions: the value of 'key' is increasing

int main(int argc,char* argv[])
{
    try {

	//---------- Parse command line  -------//
	variables_map args = parse_cmd_line(argc,argv);

	vector<string> filenames;
	if (args.count("filenames"))
	    filenames = args["filenames"].as<vector<string>>();
	else
	    filenames = {"-"};

	auto key = args["key"].as<string>();
	optional<int> min;
	if (args.count("skip"))
	    min = args["skip"].as<int>();

	optional<int> max;
	if (args.count("until"))
	    max = args["until"].as<int>();

	optional<int> size;
	if (args.count("size"))
	    size = args["size"].as<int>();

	if (args.count("size") and args.count("until"))
	    throw myexception()<<"cannot set both arguments 'size' and 'until'.";
	else if (size)
	    max = (min?*min:0) + *size;

	if (min and max and *max < *min)
	    throw myexception()<<"error: maximum value ("<<*max<<") < minimum value ("<<*min<<")";

	// FIXME: we can only read from stdin one time - the next time it will be empty.
	for(auto& filename: filenames)
	{
	    istream_or_ifstream in(std::cin,"-", filename, "alignments file");
	    cut_range(in, std::cout, key, min, max);
	}
    }
    catch (std::exception& e) {
	std::cerr<<"cut-range: Error! "<<e.what()<<std::endl;
	exit(1);
    }
}
