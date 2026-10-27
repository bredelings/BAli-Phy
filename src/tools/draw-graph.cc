/*
   Copyright (C) 2006-2008 Benjamin Redelings

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

#include "util/myexception.hh"
#include "mctree.hh"

#include <CLI/CLI.hpp>


using namespace std;


int main(int argc,char* argv[]) 
{ 
  try {

    cout.precision(3);
    cout.setf(ios::fixed);

    //----------- Parse command line  -----------//
    CLI::App app{"Write a multiconnected tree as Graphviz DOT on standard output.", "draw-graph"};
    app.get_formatter()->long_option_alignment_ratio(0.2f);
    string filename;
    app.add_option("FILE", filename, "File containing a tree and/or taxon partitions")->required()->type_name("");
    try
    {
      app.parse(argc, argv);
    }
    catch (const CLI::ParseError& error)
    {
      app.exit(error);
      return error.get_exit_code() == 0 ? 0 : 1;
    }

    //------------ Load Partitions --------------//
    MC_tree T = load_MC_tree(filename);
    
    //------------ Draw the graph ---------------//
    string name = get_graph_name(filename);
    draw_graph(T,name);
  }
  catch (std::exception& e) {
    cerr<<"draw-graph: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}

