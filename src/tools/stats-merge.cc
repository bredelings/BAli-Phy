/*
   Copyright (C) 2006,2008 Benjamin Redelings

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
#include <string>
#include "util/assert.hh"
#include "util/myexception.hh"
#include <vector>
#include <valarray>
#include <cmath>
#include <fstream>
#include <memory>

#include "util/string/join.hh"
#include "statistics.hh"
#include "util/io.hh"

#include <CLI/CLI.hpp>

using namespace std;

// If the files share a field (such as iter) then we should MERGE and CHECK
int main(int argc,char* argv[]) 
{ 
  try {
    CLI::App app{"Combine columns from different Tracer-format data files.", "stats-merge"};
    app.usage("Usage: stats-merge FILE [FILE ...]");
    app.get_formatter()->long_option_alignment_ratio(0.2f);
    vector<string> filenames;
    app.add_option("FILE", filenames, "Input statistics files")->required()->type_name("");
    try
    {
      app.parse(argc, argv);
    }
    catch (const CLI::ParseError& error)
    {
      app.exit(error);
      return error.get_exit_code() == 0 ? 0 : 1;
    }

    //-------------- Open Files  ----------------//
    vector<unique_ptr<checked_ifstream>> filestreams(filenames.size());
    for(int i=0;i<filenames.size();i++) 
      filestreams[i] = make_unique<checked_ifstream>(filenames[i],"statistics file");


    //------------- Parse Headers ---------------//
    bool ok = true;
    vector<string> sublines(filestreams.size());
    while (ok) {
      for(int i=0;i<filestreams.size();i++)
	portable_getline((*filestreams[i]),sublines[i]);

      ok = (bool)*filestreams[0];
      bool error = false;
      for(int i=1;i<filestreams.size();i++)
	if (ok != bool(*filestreams[i]))
	  error = true;

      if (ok) cout<<join(sublines,'\t')<<"\n";

      if (error) throw myexception()<<"Files have different length!";
    }
  }
  catch (std::exception& e) {
    std::cerr<<"stats-merge: Error! "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}


