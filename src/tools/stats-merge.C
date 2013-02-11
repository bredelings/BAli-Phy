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
#include <cassert>
#include <vector>
#include <valarray>
#include <cmath>
#include <fstream>

#include "util.H"
#include "statistics.H"
#include "io.H"

#include <boost/program_options.hpp>

using namespace std;

namespace po = boost::program_options;
using po::variables_map;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description invisible("Invisible options");
  invisible.add_options()
    ("filenames", value<vector<string> >(),"files to merge")
    ;

  options_description visible("All options");
  visible.add_options()
    ("help", "Produce help message")
    ;

  options_description all("All options");
  all.add(invisible).add(visible);

  // positional options
  positional_options_description p;
  p.add("filenames", -1);

  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: stats-merge <file1> [<file2> ... ]\n";
    cout<<"Combine columns from different Tracer-format data files.\n\n";
    cout<<visible<<"\n";
    exit(0);
  }

  return args;
}

vector<string> parse_header(const string& line)
{
  vector<string> headers = split(line,'\t');

  if (headers.size() == 0)
    throw myexception()<<"No column names provided!";

  for(int i=0;i<headers.size();i++)
    if (headers[i].size() == 0)
      throw myexception()<<"The "<<i<<"th column name is blank!";

  return headers;
}


// If the files share a field (such as iter) then we should MERGE and CHECK
int main(int argc,char* argv[]) 
{ 
  try {
    //----------- Parse command line  -----------//
    variables_map args = parse_cmd_line(argc,argv);

    //-------------- Open Files  ----------------//
    if (not args.count("filenames"))
      throw myexception()<<"No filenames provided.";

    vector<string> filenames = args["filenames"].as<vector<string> >();

    vector<istream*> filestreams(filenames.size(),NULL);
    for(int i=0;i<filenames.size();i++) 
      filestreams[i] = new checked_ifstream(filenames[i],"statistics file");


    //------------- Parse Headers ---------------//
    bool ok = true;
    vector<string> sublines(filestreams.size());
    while (ok) {
      for(int i=0;i<filestreams.size();i++)
	getline((*filestreams[i]),sublines[i]);

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


