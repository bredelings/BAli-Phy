/*
  Copyright (C) 2005-2009 Benjamin Redelings

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
#include <optional>
#include <cmath>
#include "tree/tree.hh"
#include "tree/sequencetree.hh"
#include "tree/tree-util.hh"
#include "tree-dist.hh"
#include "mctree.hh"
#include <CLI/CLI.hpp>
#include <boost/dynamic_bitset.hpp>
#include "util/rng.hh"
#include "util/myexception.hh"
#include "util/cmdline.hh"

extern int log_verbose;

using boost::dynamic_bitset;

using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;
using std::valarray;

bool update_lengths(const MC_tree& Q,const SequenceTree& T,
		    const vector<dynamic_bitset<> >& node_masks,
		    const vector<partition>& partitions2,
		    valarray<double>& branch_lengths, 
		    valarray<double>& node_lengths)
{
    // check that this tree is consistent with the MC Tree Q
    for(int i=0;i<Q.branch_order.size();i++) 
    {
	int b = Q.branch_order[i];
	if (not implies(T,Q.partitions[b]))
	    return false;
    }

    // map branches of the input tree
    for(int b=0;b<T.n_branches();b++)
    {
	auto P = partition_from_branch(T,b);

	// Find mc tree branches implied by branch b
	vector<int> branches;
	for(int i=0;i<Q.branch_order.size();i++) 
	{
	    if (implies(P,partitions2[i]))
		branches.push_back(i);
	}

	// Find out which nodes this branch is inside of 
	vector<int> nodes;
	for(int n=0;n<Q.n_nodes();n++) 
	{
	    if (Q.degree(n) == 0) continue;

	    auto P2 = P;
	    P2.group1 = P2.group1 & node_masks[n];
	    P2.group2 = P2.group2 & node_masks[n];
	    if (P2.group1.none() or P2.group2.none())
		continue;

	    bool ok = true;
	    for(int b=0;b<2*Q.n_branches() and ok;b++)
	    {
		if (Q.mapping[b] != n)  continue;
		auto P3 = Q.partitions[b];
		P3.group1 = P3.group1 & node_masks[n];
		P3.group2 = P3.group2 & node_masks[n];
		if (partition_less_than(P3,P2) or partition_less_than(P3,reverse(P2)))
		    ;
		else
		    ok=false;
	    }

	    if (ok) {
		nodes.push_back(n);
		assert(Q.degree(n) > 3);
	    }
	}


	// This branch should be inside only one node, if any.
	assert(nodes.size() < 2);

	// This branch should not be inside a node, if it implies an mc tree branch.
	if (branches.size()) assert(not nodes.size());

	// But this branch should be inside a node if it doesn't imply a branch.
	assert(branches.size() + nodes.size() > 0);

	const double L = T.branch(b).length();

	// Divide the branch length evenly between the branches it implies.
	for(int i=0;i<branches.size();i++)
	    branch_lengths[branches[i]] += L/branches.size();

	// Divide the branch length evenly between the nodes (node?) it implies.
	for(int i=0;i<nodes.size();i++)
	    node_lengths[nodes[i]] += L/nodes.size();
    }

    return true;
}


struct accum_branch_lengths: public accumulator<SequenceTree>
{
    int n_samples;
    int n_matches;

    MC_tree Q;

    vector<dynamic_bitset<> > node_masks;
    vector<partition> partitions2;

    valarray<double> m1;
    valarray<double> n1;
    valarray<double> branch_m2;
    valarray<double> node_m2;

    void operator()(const SequenceTree&);

    void finalize() override
    {
        if (n_samples == 0)
            throw myexception()<<"No trees were read in!";
        if (n_matches == 0)
            throw myexception()<<"No trees matched the specified topology!";
    }

    accum_branch_lengths(const MC_tree& T);
};

accum_branch_lengths::accum_branch_lengths(const MC_tree& T)
    :
    n_samples(0),
    n_matches(0),
    Q(T),
    m1(0.0, Q.n_branches()),
    n1(0.0, Q.n_nodes()),
    branch_m2(0.0, Q.n_branches()),
    node_m2(0.0, Q.n_nodes())
{
    // compute node masks
    for(int n=0;n<Q.n_nodes();n++)
    {
	dynamic_bitset<> mask(Q.n_leaves()); mask.flip();

	for(int i=0;i<2*Q.n_branches();i++)
	{
	    // Taxa that wander over any branch pointing to this node wander over this node too.
	    // So, remove them from this node mask.
	    if (Q.mapping[i] == n and not Q.directly_wanders[i])
		mask &= Q.partitions[i].mask();
	}
	node_masks.push_back(mask);
    }

    // incorporate lengths of branches that map to Q
    for(int i=0;i<Q.branch_order.size();i++) 
    {
	int b = Q.branch_order[i];

	auto P = Q.partitions[b];
	dynamic_bitset<> mask = P.mask();

	// find non-wandering branches directly left of me
	for(int j=0;j<2*Q.n_branches();j++)
	{
	    if (Q.directly_left_of(j,b)) 
	    {
		if (Q.directly_wanders[j])
		    mask &= ~Q.partitions[j].group1;
		else
		    mask &= Q.partitions[j].mask();
	    }
	}

	// find non-wandering branches directly right of me
	for(int j=0;j<2*Q.n_branches();j++)
	{
	    if (Q.directly_left_of(b,j)) 
	    {
		if (Q.directly_wanders[Q.reverse(j)])
		    mask &= ~Q.partitions[j].group2;
		else
		    mask &= Q.partitions[j].mask();
	    }
	}

	// partition masks should be computable from the masks
	//    of both endpoint nodes.
	// but how about degree=0 nodes? this shouldn't work then
	//    assert(equal(mask,node_masks[n1] and node_masks[n2]));

	P.group1 &= mask;
	P.group2 &= mask;


	partitions2.push_back(P);
    }

}

void accum_branch_lengths::operator()(const SequenceTree& T)
{
    n_samples++;
    valarray<double> branches(0.0, Q.n_branches()), nodes(0.0, Q.n_nodes());
    if (not update_lengths(Q,T,node_masks,partitions2,branches,nodes)) return;
    n_matches++;

    // Each observation is a matching tree's total contribution, including zeros. Welford's
    // update keeps the mean and sum of squared deviations from it without subtracting large
    // nearly equal moments. The product uses deviations from the old and new means.
    for (int b = 0; b < branches.size(); b++)
    {
        double delta = branches[b] - m1[b];
        m1[b] += delta / n_matches;
        branch_m2[b] += delta * (branches[b] - m1[b]);
    }
    for (int n = 0; n < nodes.size(); n++)
    {
        double delta = nodes[n] - n1[n];
        n1[n] += delta / n_matches;
        node_m2[n] += delta * (nodes[n] - n1[n]);
    }
}

int main(int argc,char* argv[]) 
{ 
    try {
        CLI::App app{"Compute branch and node lengths for a query topology.", "mctree-mean-lengths"};
        app.usage("Usage: mctree-mean-lengths TREE [OPTIONS] < in-file");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        string tree_file, prune_names;
        int skip = 0, subsample = 1;
        std::optional<int> last;
        std::optional<unsigned long> seed;
        bool var = false, no_node_lengths = false, safe = false, drop_partial = false, verbose = false;
        app.add_option("TREE", tree_file, "Query topology file")->required()->type_name("");
        app.add_option("-s,--skip", skip, "Number of tree samples to skip")->capture_default_str();
        app.add_option("-u,--until", last, "Read until this number of trees");
        app.add_option("--prune", prune_names, "Taxa to remove (comma-separated list or @filename)");
        app.add_option("-x,--subsample", subsample, "Factor by which to subsample")->capture_default_str();
        app.add_flag("--var", var, "Report standard deviations of branch and node lengths instead of means");
        app.add_flag("--no-node-lengths", no_node_lengths, "Omit node-length records");
        app.add_flag("--safe", safe, "Report topology without estimated lengths if no sampled trees match");
        app.add_flag("--drop-partial", drop_partial, "Remove partial partitions before matching trees");
        app.add_option("--seed", seed, "Random seed");
        app.add_flag("-V,--verbose", verbose, "Output more log messages on stderr");
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }
        if (verbose) log_verbose = 1;

        //---------- Initialize random seed -----------//
        if (seed)
            myrand_init(*seed);
        else
            myrand_init();

        vector<string> prune = get_string_list(prune_names);

	//----------- Read the topology -----------//
        auto queries = load_partitions(tree_file);
        if (queries.empty())
            throw myexception()<<"No query topology was read!";
        auto& query = queries.front();
        if (drop_partial)
            std::erase_if(query.partitions, [](const partition& p) { return not p.full(); });
        MC_tree Q(check_MC_partitions(query));

	//-------- Read in the tree samples --------//
	accum_branch_lengths A(Q);

        scan_trees(std::cin,skip,last,subsample,prune,Q.T.get_leaf_labels(),A);
        // The tree reader records parse failures in badbit rather than throwing them.
        if (std::cin.bad())
            throw myexception()<<"Failed to read tree samples.";
        if (A.n_samples > 0 and A.n_matches == 0 and safe)
        {
            cout<<Q<<endl;
            cerr<<"mctree-mean-lengths: No trees matched; reporting topology without estimated lengths.\n";
            return 0;
        }
        A.finalize();

	if (log_verbose) std::cerr<<"mctree-mean-lengths: "<<A.n_matches<<" out of "<<A.n_samples<<" trees matched the topology";
	if (log_verbose) std::cerr<<"mctree-mean-lengths: "<<" ("<<double(A.n_matches)/A.n_samples*100<<"%)"<<std::endl;

        // Divide by the number of matching trees to describe their empirical distribution.
        valarray<double> branch_lengths = A.m1;
        valarray<double> node_lengths = A.n1;
        if (var)
        {
            branch_lengths = std::sqrt(A.branch_m2 / A.n_matches);
            node_lengths = std::sqrt(A.node_m2 / A.n_matches);
        }

        // Full partitions come first in branch_order, in the ordinary tree's branch order.
        for(int b=0;b<Q.T.n_branches();b++)
            Q.T.branch(b).set_length(branch_lengths[b]);
	cout<<Q.T<<endl;

	for(int i=0;i<Q.branch_order.size();i++)
	{
	    int b = Q.branch_order[i];
	    if (not Q.partitions[b].full()) {
		cout<<"branch "<<branch_lengths[i]<<endl;
		cout<<Partition(Q.names(), Q.partitions[b])<<endl;
	    }
	}

	for(int n=0;n<Q.n_nodes();n++) 
	{
	    if (not no_node_lengths and A.n1[n] > 0) {
		cout<<"node "<<node_lengths[n]<<endl;
		int b = Q.branch_to_node(n);
		cout<<Partition(Q.names(), Q.partitions[b])<<endl;
	    }
	}
    }
    catch (std::exception& e) {
	std::cerr<<"mctree-mean-lengths: Error! "<<e.what()<<endl;
	exit(1);
    }
    return 0;
}
