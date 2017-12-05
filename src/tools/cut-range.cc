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

#include <cassert>

#include <string>
#include <sstream>
#include <iostream>
#include "util.H"
#include "io.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::string;
using std::cout;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("key", value<string>()->default_value("iterations"),"cut based on values of <key>=value")
    ("skip",value<int>(),"the number of samples to skip")
    ("size",value<int>(),"maximum number of samples to use")
    ("until",value<int>(),"last sample to use")
    ("verbose","Output more log messages on stderr.")
    ;

  variables_map args;     
  store(parse_command_line(argc, argv, all), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: cut-range [OPTIONS]\n";
    cout<<"Select lines between certain values of a key. (e.g. iterations)\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (args.count("verbose")) log_verbose = 1;

  return args;
}





// assumptions: the value of 'key' is increasing

int main(int argc,char* argv[]) {
  try {

    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    if (not args.count("key")) 
      throw myexception()<<"argument 'key' not set.";
    if (args.count("size") and args.count("until"))
      throw myexception()<<"cannot set both arguments 'size' and 'until'.";

    bool is_min = args.count("skip");
    double min = 0;
    if (is_min) min = args["skip"].as<int>();

    bool is_max = args.count("size") or args.count("until");
    double max = min;
    if (args.count("size"))
      max = min + args["size"].as<int>();

    if (args.count("until"))
      max = std::max(max,(double)args["until"].as<int>());
    if (is_min and is_max and max < min)
      throw myexception()<<"error: maximum value < minimum value";

    string pattern = args["key"].as<string>() + " = ";

    string line;

    bool in_interval = not is_min;
    while(portable_getline(std::cin,line)) {

      // look for the pattern
      int where = line.find(pattern);

      // if no pattern, then
      if (where != -1) {

	// move PAST the pattern
	where += pattern.size();
	double value = convertTo<double>(line.substr(where));
	in_interval = true;
	if (is_min and value <= min)
	  in_interval = false;
	if (is_max and value > max) {
	  in_interval = false;
	  exit(0);
	}

	//    std::cerr<<line<<std::endl;
	//    std::cerr<<"where = "<<where<<std::endl;
	//    std::cerr<<value<<std::endl;
      }

      if (in_interval)
	std::cout<<line<<std::endl;
    }
  }
  catch (std::exception& e) {
    std::cerr<<"cut-range: Error! "<<e.what()<<std::endl;
    exit(1);
  }
}
