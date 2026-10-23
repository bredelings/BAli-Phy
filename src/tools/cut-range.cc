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
#include "util/string/convert.hh"
#include "util/io.hh"
#include "util/log-level.hh"

#include <CLI/CLI.hpp>
#include <optional>

using std::string;
using std::vector;
using std::cout;
using std::optional;

// Copy selected keyed records, applying the stride independently to each input stream.
void cut_range(std::istream& in, std::ostream& out,
	       const string& key, optional<int> min, optional<int> max, int subsample)
{
    string pattern = key + " = ";

    string line;

    bool in_interval = not min;
    int eligible_record = 0;
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
	    if (max and value > *max)
		return;

	    bool eligible = not min or value >= *min;
	    in_interval = eligible and eligible_record % subsample == 0;
	    if (eligible)
		eligible_record++;

	    //    std::cerr<<line<<std::endl;
	    //    std::cerr<<"where = "<<where<<std::endl;
	    //    std::cerr<<value<<std::endl;
	}

	if (in_interval)
	    out<<line<<'\n';
    }
}




// assumptions: the value of 'key' is increasing

int main(int argc,char* argv[])
{
    try {

        CLI::App app{"Select lines from files based on lines containing 'key = value'.", "cut-range"};
        app.usage("Usage: cut-range [FILE ...] [OPTIONS]");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        app.footer("The selection options apply to each input file separately.\n"
                   "Example: cut-range --skip=100 run-1/C1.P1.fastas run-2/C1.P1.fastas > P1.fastas");
        vector<string> filenames;
        string key = "iterations";
        optional<int> min, max, size;
        int subsample = 1;
        bool verbose = false;
        app.add_option("--filenames,FILE", filenames, "Filenames to read ('-' for STDIN)");
        app.add_option("--key", key, "cut based on values of key = value")->capture_default_str();
        app.add_option("--skip", min, "minimum key value to retain (inclusive)");
        auto until_option = app.add_option("--until", max, "maximum key value to retain (inclusive)");
        app.add_option("--subsample", subsample, "keep only every NUM eligible samples")->capture_default_str();
        app.add_option("--size", size, "maximum number of samples to use")
            ->excludes(until_option);
        app.add_flag("--verbose", verbose, "Output more log messages on stderr.");
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
        if (filenames.empty()) filenames = {"-"};

	if (subsample < 1)
	    throw myexception()<<"subsample must be at least 1";

	if (size)
	    max = (min?*min:0) + *size;

	if (min and max and *max < *min)
	    throw myexception()<<"error: maximum value ("<<*max<<") < minimum value ("<<*min<<")";

	// FIXME: we can only read from stdin one time - the next time it will be empty.
	for(auto& filename: filenames)
	{
	    istream_or_ifstream in(std::cin,"-", filename, "alignments file");
	    cut_range(in, std::cout, key, min, max, subsample);
	}
    }
    catch (std::exception& e) {
	std::cerr<<"cut-range: Error! "<<e.what()<<std::endl;
	exit(1);
    }
}
