/*
  Copyright (C) 2004,2006-2008 Benjamin Redelings

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

#include <cmath>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <iostream>
#include <string>
#include "util/io.hh"

#include <CLI/CLI.hpp>

using namespace std;

int main(int argc,char* argv[]) 
{ 
    try {

        CLI::App app{"Subsample lines in a file.", "bpy-subsample"};
        app.usage("Usage: bpy-subsample [OPTIONS] [FACTOR] < in-file");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        unsigned skip = 0;
        double subsample = 1.0;
        optional<unsigned> max;
        bool header = false;
        app.add_option("-x,--factor,FACTOR", subsample, "Factor by which to subsample.")->capture_default_str();
        app.add_option("--skip", skip, "Throw out some lines at the beginning.")->capture_default_str();
        app.add_option("--max", max, "Maximum number of samples to keep");
        app.add_flag("--header", header, "This file has a header - don't throw it out.");
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }

        if (!std::isfinite(subsample) || subsample <= 0)
            throw std::invalid_argument("factor must be finite and positive");

        string line;
        if (header && portable_getline(cin, line))
            cout<<line<<'\n';

        std::uint64_t lines = 0;
        std::uint64_t count = 0;
        while ((!max || count < *max) && portable_getline(cin, line))
        {
            if (skip > 0)
                --skip;
            else
            {
                // For nonnegative indices, floor(lines/factor) >= count is equivalent to
                // this comparison. Avoid narrowing the quotient to int, preserving fractional
                // spacing and keeping every line for factors below one.
                if (lines/subsample >= count)
                {
                    cout<<line<<'\n';
                    ++count;
                }
                ++lines;
            }
        }
    }
    catch (exception& e) {
	cerr<<"bpy-subsample: Error! "<<e.what()<<std::endl;
	exit(1);
    }
}
