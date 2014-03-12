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

#include <iostream>
#include <string>
#include "util.H"
#include "io.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using namespace std;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("factor",value<double>()->default_value(1.0),"Factor by which to subsample.")
    ("skip",value<unsigned>()->default_value(0),"Throw out some lines at the beginning.")
    ("max",value<unsigned>(),"Maximum number of samples to keep")
    ("header","This file has a header - don't throw it out.")
    ;

  // positional options
  positional_options_description p;
  p.add("factor", 1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: subsample [OPTIONS] < in-file\n";
    cout<<"Subsample lines in a file.\n\n";
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

    string line;

    if (args.count("header")) {
      // print header
      portable_getline(cin,line);
      cout<<line<<endl;
    }

    int skip = args["skip"].as<unsigned>();
    double subsample = args["factor"].as<double>();
    int max = -1;
    if (args.count("max"))
      max = args["max"].as<unsigned>();

    // print selected lines
    int lines=0;
    int count=0;
    while(portable_getline(cin,line)) 
    {
      if (skip > 0)
	skip--;
      else if (max > 0 and count >= max)
	break;
      else {
	if (int(lines/subsample) > (count-1)) {
	  cout<<line<<endl;
	  count++;
	}
	lines++;
      }
    }
  }
  catch (exception& e) {
    cerr<<"subsample: Error! "<<e.what()<<std::endl;
    exit(1);
  }
}
