/*
  Copyright (C) 2004-2009 Benjamin Redelings

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
#include <algorithm>
#include <string>
#include <vector>
#include <list>
#include <map>
#include <cmath>

#include "myexception.H"
#include "tree/sequencetree.H"
#include "tree-dist.H"
#include "tree/tree-util.H"
#include "statistics.H"

using namespace std;
#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;
using std::string;

string topology(const string& t) {
    SequenceTree T = standardized(t);
    return T.write(false);
}

string topology(const SequenceTree& T) {
    SequenceTree T2 = T;
    standardize(T2);
    return T2.write(false);
}

vector<int> convert_bitvector(const valarray<bool>& b)
{
    vector<int> v(b.size());
    for(int i=0;i<b.size();i++)
	if (b[i])
	    v[i] = 1;
	else
	    v[i] = 0;
    return v;
}


variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    // named options
    options_description all("Allowed options");
    all.add_options()
	("help,h", "produce help message")
	("predicates",value<string>(),"predicates to examine")
	("skip,s",value<int>()->default_value(0),"number of trees to skip")
	("sub-sample,x",value<int>()->default_value(1),"factor by which to sub-sample")
	("max,m",value<int>(),"maximum number of trees to read")
	("max-points",value<int>(),"maximum number of points to record")
	("mode", value<string>()->default_value("SRQ"),"SRQ, sum, or values")
	("invert","consider the inverse of each event instead")
	("no-scale-x","don't scale X")
	("no-scale-y","don't scale Y")
	;
    // MIN REVERSALS!


    // positional options
    positional_options_description p;
    p.add("predicates", 1);
  
    variables_map args;     
    store(command_line_parser(argc, argv).
	  options(all).positional(p).run(), args);
    notify(args);    

    if (args.count("help")) {
	cout<<"Usage: tree-to-srq <predicates-file> < in-file\n";
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

	// Load the partitions that we are considering
	auto trees = load_partitions(args["predicates"].as<string>());

	// Read in the trees
	int skip = args["skip"].as<int>();

	int subsample=args["sub-sample"].as<int>();

	int max = -1;
	if (args.count("max"))
	    max = args["max"].as<int>();

	int max_points = -1;
	if (args.count("max-points"))
	    max_points = args["max-points"].as<int>();

	tree_sample tree_dist(std::cin,skip,subsample,max);


	const int L = tree_dist.size();

	// Compute info for plots
	vector<vector<int> > plots(trees.size());
	for(int i=0;i<trees.size();i++) 
	{
	    valarray<bool> support = tree_dist.support(trees[i].partitions);

	    if (args.count("invert")) support = not support;

	    if (args["mode"].as<string>() == "SRQ")
		plots[i] = statistics::regeneration_times(support);
	    else if (args["mode"].as<string>() == "sum")
		plots[i] = statistics::total_times(support);
	    else
		plots[i] = convert_bitvector(support);
	}


	if (args["mode"].as<string>() == "values") 
	{
	    vector<string> names(plots.size());
	    for(int i=0;i<names.size();i++)
		names[i] = string("P") + convertToString(i+1);
	    cout<<join(names,'\t')<<endl;

	    for(int i=0;i<L;i++)
		for(int j=0;j<plots.size();j++) {
		    cout<<plots[j][i];
		    if (j == plots.size() - 1)
			cout<<"\n";
		    else
			cout<<"\t";
		}
	}
	else 
	{
	    // write out plots

	    const double delta = 1.0/max_points;
	    for(int i=0;i<plots.size();i++) {
		double scale_x = plots[i].size()-1;
		double scale_y = plots[i].back();
		if (scale_y == 0) scale_y = 1;
	
		if (args.count("no-scale-x"))
		    scale_x = 1;
	
		if (args.count("no-scale-y"))
		    scale_y = 1;
	
		double x1 = 0;
		double y1 = 0;
		for(int j=0;j<plots[i].size();j++) 
		{
		    double x2 = double(j)/(plots[i].size()-1);
		    double y2 = plots[i][j]/(plots[i].back());
	  
		    if ((x2-x1 > delta or y2-y1 > delta) or j==0 or j==plots[i].size()-1) {
			cout<<j/scale_x<<"   "<<plots[i][j]/scale_y<<endl;
			x1 = x2;
			y1 = y2;
		    }
		}
		cout<<endl;
	    }
	}
    }
    catch (std::exception& e) {
	std::cerr<<"trees-to-SRQ: Error! "<<e.what()<<endl;
	exit(1);
    }
    return 0;
}
