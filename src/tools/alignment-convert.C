/*
   Copyright (C) 2004-2006,2008-2009 Benjamin Redelings

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

#include <string>
#include "myexception.H"
#include "alignment.H"
#include "sequence/sequence-format.H"
#include "alignment-util.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::string;
using std::endl;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("align", value<string>()->default_value("-"),"file with alignment to convert (default STDIN)")
    ("output", value<string>(),"which output format: fasta or phylip?")
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
    std::cout<<"Usage: alignment-convert <alignment-file> [OPTIONS]\n";
    std::cout<<"Convert the alignment to FASTA or PHYLIP.\n\n";
    std::cout<<all<<"\n";
    exit(0);
  }

  return args;
}


int main(int argc,char* argv[]) { 
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    alignment A = load_A(args);
    
    if (not args.count("output"))
      throw myexception()<<"Output format not specified";

    if (args["output"].as<string>() == "phylip")
      A.print_phylip(std::cout);
    else if (args["output"].as<string>() == "fasta")
      A.print_fasta(std::cout);
    else
      throw myexception()<<"Don't recognized requested format '"<<args["output"].as<string>()<<"'";
  }
  catch (std::exception& e) {
    std::cerr<<"alignment-convert: Error! "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}
