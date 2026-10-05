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

#include <iostream>
#include <fstream>
#include <string>
#include <set>
#include <vector>
#include "tree/tree.hh"
#include "tree/tree-util.hh"
#include "alignment/load.hh"
#include "sequence/sequence-format.hh"
#include "findroot.hh"

#include <CLI/CLI.hpp>

using std::cout;
using std::cerr;
using std::endl;

using std::string;
using std::vector;
using std::set;


// Select leaf sequences from each FASTA alignment on stdin and write the resulting stream.
int main(int argc,char* argv[]) 
{ 

    try {
	//---------- Parse command line  -------//
        CLI::App app{"Remove ancestral sequences from a stream of FASTA alignments.", "alignment-chop-internal"};
        app.usage("Usage: alignment-chop-internal [OPTIONS] < alignments-file > leaves.fastas");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        string tree_file;
        int N = 0;
        auto* nleaves_option = app.add_option("-N,--nleaves", N, "Keep the first N sequences")
            ->type_name("N");
        app.add_option("--tree", tree_file, "Keep sequences named by the tree's leaves")
            ->type_name("FILE");
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            // Let CLI11 print help or diagnostics, retaining the tool's 0/1 exit statuses.
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }

        if (nleaves_option->count() and app.count("--tree"))
            throw myexception()<<"Cannot give both --nleaves and --tree.";
        if (not nleaves_option->count() and not app.count("--tree"))
            throw myexception()<<"Specify either --nleaves or --tree.";

	//------- Determine number of leaf sequences to keep --------//
	std::function<void(vector<sequence>&)> chop_fn;

	if (nleaves_option->count())
	{
	    chop_fn = [N](vector<sequence>& S)
	    {
		if (S.size() < N)
		    throw myexception()<<"Trying to keep "<<N<<" leaf sequences, but only got "<<S.size()<<"!";

		S.resize(N);
	    };
	}
	else
	{
	    set<string> non_empty_leaf_labels;
	    for(auto& leaf_label: load_tree_from_file(tree_file).get_leaf_labels())
		if (leaf_label.empty())
		    std::cerr<<"Warning: ignoring empty leaf label!\n";
		else if (non_empty_leaf_labels.count(leaf_label))
		    throw myexception()<<"Leaf label '"<<leaf_label<<"' occurs twice!";
		else
		    non_empty_leaf_labels.insert(leaf_label);

	    chop_fn = [non_empty_leaf_labels](vector<sequence>& S)
	    {
		if (S.size() < non_empty_leaf_labels.size())
		    throw myexception()<<"Trying to keep "<<non_empty_leaf_labels.size()<<" leaf sequences, but only got "<<S.size()<<"!";

		vector<sequence> S2;
		for(auto&& s: S)
		    if (non_empty_leaf_labels.count(s.name))
			S2.push_back(std::move(s));

		std::swap(S, S2);
	    };
	}
	
	//------ Read sequences and chop off non-leaf sequences -----//
	while (auto sequences = find_load_next_sequences(std::cin))
	{
	    chop_fn(*sequences);

	    sequence_format::write_fasta(std::cout, *sequences);
	}
    }
    catch (std::exception& e) {
	std::cerr<<"alignment-chop-internal: Error! "<<e.what()<<endl;
	exit(1);
    }
    return 0;

}
