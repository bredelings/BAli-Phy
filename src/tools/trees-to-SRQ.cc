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
#include <string>
#include <vector>

#include "util/myexception.hh"
#include "tree-dist.hh"
#include "statistics.hh"
#include "util/string/join.hh"
#include "util/string/convert.hh"

#include <CLI/CLI.hpp>
#include <optional>


using std::cout;
using std::endl;

using std::string;
using std::vector;
using std::valarray;

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




int main(int argc,char* argv[]) 
{ 
    try {
	//---------- Parse command line  -------//
        CLI::App app{"Generate Scaled Regeneration Quantile (SRQ) plots.", "trees-to-SRQ"};
        app.usage("Usage: trees-to-SRQ <predicates-file> <sampled-trees> [<sampled-trees> ...]");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        string predicates_file, skip = "10%", mode = "SRQ";
        vector<string> files;
        std::optional<int> last;
        int subsample = 1, max_points = 1000;
        bool invert = false, no_scale_x = false, no_scale_y = false;
        app.add_option("--predicates,PREDICATES", predicates_file, "predicates to examine")->required();
        app.add_option("--files,FILE", files, "tree samples to examine")->required();
        app.add_option("-s,--skip", skip, "Number of trees to skip.")->capture_default_str();
        app.add_option("-x,--subsample", subsample, "factor by which to sub-sample")->capture_default_str();
        app.add_option("-u,--until", last, "Read until this number of trees.");
        app.add_option("--max-points", max_points, "Approximate number of points per curve.")->capture_default_str();
        app.add_option("--mode", mode, "SRQ, sum, or values")->capture_default_str();
        app.add_flag("--invert", invert, "consider the inverse of each event instead");
        app.add_flag("--no-scale-x", no_scale_x, "don't scale X");
        app.add_flag("--no-scale-y", no_scale_y, "don't scale Y");
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }

	// Load the partitions that we are considering
	auto trees = load_partitions(predicates_file);

	auto tree_dist = read_trees(files, skip, last, subsample, {});

	const int L = tree_dist.size();

	// Compute info for plots
	vector<vector<int> > plots(trees.size());
	for(int i=0;i<trees.size();i++) 
	{
	    match_predicate_taxa(trees[i], tree_dist.names());
	    valarray<bool> support = tree_dist.support(trees[i].partitions);

	    if (invert) support = not support;

	    if (mode == "SRQ")
		plots[i] = statistics::regeneration_times(support);
	    else if (mode == "sum")
		plots[i] = statistics::total_times(support);
	    else
		plots[i] = convert_bitvector(support);
	}


	if (mode == "values") 
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

	    if (max_points <= 0)
		throw myexception()<<"--max-points must be positive";
	    const double delta = 1.0/max_points;
	    for(int i=0;i<plots.size();i++) {
		double scale_x = plots[i].size()-1;
		double scale_y = plots[i].back();
		if (scale_y == 0) scale_y = 1;
	
		if (no_scale_x)
		    scale_x = 1;
	
		if (no_scale_y)
		    scale_y = 1;
	
		double x1 = 0;
		double y1 = 0;
		for(int j=0;j<plots[i].size();j++) 
		{
		    // Thin in normalized coordinates regardless of displayed units. A zero total
		    // in a cumulative curve means every ordinate is zero, so it stays flat.
		    double x2 = double(j)/(plots[i].size()-1);
		    double y2 = plots[i].back() ? double(plots[i][j])/plots[i].back() : 0;
	  
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
