/*
  Copyright (C) 2005,2008-2009 Benjamin Redelings

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

#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include "util/myexception.H"
#include "alignment/alignment.H"

#include "util/string/split.H"
#include "tree/tree-util.H"
#include "alignment/alignment-util.H"
#include "distance-methods.H"
#include "joint-A-T.H"
#include "partition.H"
#include "dp/A2_states.H"
#include <optional>
#include <map>

#include <boost/dynamic_bitset.hpp>

extern int log_verbose;

using std::cin;
using std::cout;
using std::cerr;
using std::istream;
using std::ifstream;
using std::vector;
using std::string;
using std::endl;
using std::optional;
using std::map;
using boost::dynamic_bitset;
//using namespace optimize;

namespace po = boost::program_options;
using po::variables_map;
using namespace A2;

// OK, what does a complete conflict analysis look like?
// For summaries, we want to map branches in the sampled tree
//  to either branches or nodes of the consensus tree.
// A branch can then map to a branch (aligned), a node (extends), or
//  nothing (conflict)

// For extracting sequences corresponding to a node, we want to
// map nodes on the query tree to nodes in the sampled tree.

map<dynamic_bitset<>,int> get_splits(const Tree& T)
{
    map<dynamic_bitset<>,int> split_to_branch;

    for(int b=0;b<2*T.n_branches();b++)
    {
	auto s = branch_partition(T,b);
	split_to_branch.insert({s,b});
	int b2 = T.directed_branch(b).reverse();
	split_to_branch.insert({~s,b2});
    }
    return split_to_branch;
}


vector<optional<int>> get_partial_branches_map(const Tree& Q, const Tree& T)
{
    auto Q_splits = get_splits(Q);
    auto T_splits = get_splits(T);

    vector<optional<int>> Q_to_T_branches(2*Q.n_branches());

    for(int b=0;b<Q_to_T_branches.size();b++)
    {
	auto qs = branch_partition(Q,b);
	if (T_splits.count(qs))
	    Q_to_T_branches[b] = T_splits[qs];
    }
    return Q_to_T_branches;
}


optional<int> get_corresponding_node(int q_node, const Tree& Q, const Tree& T, const vector<optional<int>>& Q_to_T_branches)
{
    optional<int> t_node;
    for(auto q_branch=Q.node(q_node).branches_out(); q_branch; q_branch++)
    {
	auto t_branch = Q_to_T_branches[*q_branch];
	if (not t_branch) return {};

	int t_node2 = (int)T.directed_branch(*t_branch).source();
	if (not t_node)
	    t_node = t_node2;
	else if (t_node2 != *t_node)
	    return {};
    }
    return *t_node;
}


vector<optional<int>> get_nodes_map(const Tree& Q, const Tree& T)
{
    // For trees with no branches, we can't use branch correspondence.
    if (Q.n_nodes() == 1) return { { 0 } };

    vector<optional<int>> Q_to_T_nodes(Q.n_nodes());

    auto Q_to_T_branches = get_partial_branches_map(Q,T);

    for(int q_node=0; q_node<Q.n_nodes(); q_node++)
	Q_to_T_nodes[q_node] = get_corresponding_node(q_node, Q, T, Q_to_T_branches);

    return Q_to_T_nodes;
}

void extract_sequence(const variables_map& args, const joint_A_T& J)
{
    auto Q = load_tree_from_file(args["extract-sequences"].as<string>());
    remap_T_leaf_indices(Q, J.T[0].get_leaf_labels());
    for(int i=0; i<J.size();i++)
    {
	auto& A = J.A[i];
	auto& T = J.T[i];
	auto Q_to_T_nodes = get_nodes_map(Q,T);

	for(int q_node=Q.n_leaves(); q_node < Q.n_nodes(); q_node++)
	{
	    auto t_node = Q_to_T_nodes[q_node];
	    if (not t_node) continue;

	    string q_node_name = Q.get_label(q_node);
	    if (q_node_name.empty()) continue;

	    string t_node_name = T.get_label(*t_node);
	    std::cerr<<"Mapped "<<q_node_name<<" -> "<<t_node_name<<"\n";
	    std::cout<<">"<<q_node_name<<"\n";
	    auto& a = A.get_alphabet();
	    for(int i=0;i<A.length();i++)
		std::cout<<a.lookup(A(i,*t_node));
	    std::cout<<"\n";
	}
	std::cout<<"\n\n";
    }
}

void run_analysis(const variables_map& args, const joint_A_T& J) {

    // Handle Partition name
    if (not args.count("partition"))
	throw myexception() << "Must specify a unique partition of taxa by name.\n";
    vector<string> pnames = split(args["partition"].as<string>(),':');

    //--------------------- Load (A,T) ------------------------//
    std::cout << "Iter\tPart\tLen\tIndels" << endl;
  
    int consistentsamples = 0;
    int numindels = 0;

    string line;
    for(int i=0;i<J.size();i++) {

	const alignment& A = J.A[i];
	const SequenceTree& T = J.T[i];

	Partition part = full_partition_from_names(T.get_leaf_labels(),pnames);

	bool exists = implies(T,part);
	//cerr << part << "\n";
	//cerr << "Does tree contain partition: " << exists << "\n";

	std::cout << i+1 << "\t" << exists << "\t";
	if( exists ) {
	    consistentsamples++;
	    int b = which_branch(T,part);
	    if (b == -1) throw myexception()<<"Can't find branch in tree!";
	    //cerr << "Branch number = " << b << endl;
	    vector<int> pairwiseA = get_path(A,T.branch(b).target(),T.branch(b).source());
	    //cerr << pairwiseA << endl;

	    int uniqueindels = 0;
	    int laststate = states::M;
	    for(int i=0; i<pairwiseA.size(); i++) {
		//cerr << pairwiseA[i] << " ";
		int currentstate = pairwiseA[i];
		if( (laststate != currentstate) and ((currentstate == states::G1) or (currentstate == states::G2)) ) { // This is correct - BEN
		    uniqueindels++;
		}
		laststate = currentstate;
	    }
	    //cerr << " l = " << pairwiseA.size() << " u =  " << uniqueindels << endl;
	    if( uniqueindels > 0 ) {
		numindels++;
	    }
	    std::cout << pairwiseA.size() << "\t" <<  uniqueindels << endl;
	    //int nstart = T.branch(b).target();
	    //int nend   = T.branch(b).source();
	    //cerr << "Target: " << (A.seq(nstart)).name << endl;
	    //cerr << "Source: " << (A.seq(nend)).name   << endl;
	} else {
	    std::cout << "NA\t0" << endl;
	}

	//cerr<<A<<"\n";
	//cerr<<T<<"\n";
	//exit(0);
    }


    if (log_verbose) {
	cerr<<"joint-indels: Total # samples      = " << J.size() << endl;
	cerr<<"joint-indels: # Consistent samples = " << consistentsamples << endl;
	cerr<<"joint-indels: # Indel samples      = " << numindels << endl;
	cerr<<"joint-indels: Posterior prob       = " << ((double)numindels/(double)J.size()) << endl;
    }
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    // named options
    options_description invisible("Invisible options");
    invisible.add_options()
	("alignments", value<string>(),"file of alignment samples")
	("trees", value<string>(), "file of corresponding tree samples")
	;

    // named options
    options_description visible("Allowed options");
    visible.add_options()
	("help", "produces help message")
	("subsample",value<unsigned>()->default_value(10),"factor by which to sub-sample trees")
	("partition", value<string>(), "find indels along internal branch that bi-partitions given taxa (<taxa1>:<taxa2>:...)")
	("alphabet",value<string>(),"set to 'Codons' to prefer codon alphabets")
	("verbose","Output more log messages on stderr.")
	("extract-sequences",value<string>(),"Extract sequences corresponding to tree")
	;

    options_description all("All options");
    all.add(visible).add(invisible);

    // positional options
    positional_options_description p;
    p.add("alignments", 1);
    p.add("trees", 2);
  
    variables_map args;     
    store(command_line_parser(argc, argv).
	  options(all).positional(p).run(), args);
    // store(parse_command_line(argc, argv, desc), args);
    notify(args);    

    if (args.count("help")) {
	cout<<"Usage: joint-indels <alignments file> <trees file> [OPTIONS]\n";
	cout<<visible<<"\n";
	exit(0);
    }

    if (args.count("verbose")) log_verbose = 1;

    return args;
}

int main(int argc,char* argv[]) { 
    try {
	variables_map args = parse_cmd_line(argc,argv);
	//Arguments args;
	//args.read(argc,argv);
	//args.print(cerr);

	if (log_verbose) cerr<<"joint-indels: Loading alignments and trees...\n";
	joint_A_T J = get_joint_A_T(args,true);

	if (J.size() == 0)
	    throw myexception()<<"No (A,T) read in!";
	else
	    if (log_verbose) cerr<<"joint-indels: Loaded "<<J.size()<<" (A,T) pairs."<<endl;;

	if (args.count("extract-sequences"))
	    extract_sequence(args,J);
	else
	    run_analysis(args,J);
    }
    catch (std::exception& e) {
	cerr<<"joint-indels: Error! "<<e.what()<<endl;
	exit(1);
    }
    return 0;
}
