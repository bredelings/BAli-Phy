/*
  Copyright (C) 2005,2008-2009,2019 Benjamin Redelings

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
#include "util/log-level.H"
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

/// Group consecutive identical elements: [M,M,G1,G1,M] -> [[M,M],[G1,G1],[M]]
template <typename T>
std::vector<std::vector<T>> group_consecutive(const std::vector<T>& items)
{
    std::vector<std::vector<T>> groups;
    if (items.empty()) return groups;

    std::vector<T> current_group = {items[0]};
    for (size_t i = 1; i < items.size(); i++)
    {
	if (items[i] == items[i-1])
	    current_group.push_back(items[i]);
	else
	{
	    groups.push_back(std::move(current_group));
	    current_group = {items[i]};
	}
    }
    groups.push_back(std::move(current_group));
    return groups;
}

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

    remap_T_leaf_indices(Q, J.leaf_names());

    vector<int> node_counts(Q.n_nodes());

    for(auto& [A,T]: J)
    {
	auto Q_to_T_nodes = get_nodes_map(Q,T);

	if (log_verbose > 1)
	{
	    std::cerr<<Q.write(false)<<"\n";
	    std::cerr<<T.write(false)<<"\n";
	}
	for(int q_node=Q.n_leaves(); q_node < Q.n_nodes(); q_node++)
	{
	    auto t_node = Q_to_T_nodes[q_node];
	    if (not t_node) continue;

	    string q_node_name = Q.get_label(q_node);
	    if (q_node_name.empty()) continue;

	    node_counts[q_node]++;

	    string t_node_name = T.get_label(*t_node);
	    if (log_verbose > 0)
		std::cerr<<"Mapped "<<q_node_name<<" -> "<<t_node_name<<"\n";
	    std::cout<<">"<<q_node_name<<"\n";
	    auto& a = A.get_alphabet();
	    for(int i=0;i<A.length();i++)
		std::cout<<a.lookup(A(i,*t_node));
	    std::cout<<"\n";
	}
	std::cout<<"\n\n";
    }
    for(int q_node=Q.n_leaves(); q_node<Q.n_nodes(); q_node++)
    {
	auto q_node_name = Q.get_label(q_node);
	if (q_node_name.empty()) continue;

	std::cerr<<"Node '"<<q_node_name<<"': present in "<<node_counts[q_node]<<"/"<<J.size()<<" = "<<double(node_counts[q_node])/J.size()*100<<"% of samples.\n";
    }
}

void run_analysis(const variables_map& args, const joint_A_T& J) {

    // Handle Partition name
    if (not args.count("partition"))
	throw myexception() << "Must specify a unique partition of taxa by name.\n";
    vector<string> pnames = split(args["partition"].as<string>(),':');

    bool output_details = args.count("details");

    // Struct to collect indel info
    // Start1/Start2: chars emitted in each sequence BEFORE the indel
    // End1/End2: chars emitted in each sequence AFTER the indel
    struct IndelInfo {
	int start1, start2;  // chars in seq1/seq2 before indel
	int end1, end2;      // chars in seq1/seq2 after indel
	int type;
	int length;
    };

    //--------------------- Load (A,T) ------------------------//
    if (output_details)
	std::cout << "Sample\tStart1\tStart2\tEnd1\tEnd2\tType\tLength\tLen1\tLen2" << endl;
    else
	std::cout << "Iter\tPart\tLen\tIndels" << endl;

    int consistentsamples = 0;
    int numindels = 0;
    int sample_num = 0;

    string line;
    for(auto& [A,T]: J)
    {
	Partition part = full_partition_from_names(T.get_leaf_labels(),pnames);

	bool exists = implies(T,part);

	if( exists ) {
	    consistentsamples++;
	    int b = which_branch(T,part);
	    if (b == -1) throw myexception()<<"Can't find branch in tree!";
	    vector<int> pairwiseA = get_path(A,T.branch(b).target(),T.branch(b).source());

	    // Group consecutive identical states
	    auto groups = group_consecutive(pairwiseA);

	    int uniqueindels = 0;
	    int pos1 = 0;  // chars emitted in sequence 1
	    int pos2 = 0;  // chars emitted in sequence 2

	    std::vector<IndelInfo> sample_indels;

	    for (const auto& group : groups)
	    {
		int state = group[0];
		int length = group.size();

		if (state == states::G1 or state == states::G2)
		{
		    uniqueindels++;

		    int start1 = pos1;
		    int start2 = pos2;

		    // Update positions for this indel group
		    if (state == states::G1)
			pos2 += length;  // G1: chars added to seq2
		    else
			pos1 += length;  // G2: chars added to seq1

		    if (output_details)
			sample_indels.push_back({start1, start2, pos1, pos2, state, length});
		}
		else if (state == states::M)
		{
		    pos1 += length;
		    pos2 += length;
		}
		// states::S and states::E don't emit characters
	    }

	    // Output all indels with total lengths
	    if (output_details) {
		int len1 = pos1;
		int len2 = pos2;
		for (const auto& indel : sample_indels) {
		    std::cout << sample_num << "\t"
			      << indel.start1 << "\t" << indel.start2 << "\t"
			      << indel.end1 << "\t" << indel.end2 << "\t"
			      << (indel.type == states::G1 ? "G1" : "G2") << "\t"
			      << indel.length << "\t"
			      << len1 << "\t" << len2 << endl;
		}
	    }

	    if( uniqueindels > 0 ) {
		numindels++;
	    }

	    if (not output_details)
		std::cout << pairwiseA.size() << "\t" <<  uniqueindels << endl;
	} else {
	    if (output_details)
		std::cout << sample_num << "\tNA" << endl;
	    else
		std::cout << "NA\t0" << endl;
	}

	sample_num++;
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
	("help,h", "produces help message")
	("subsample",value<unsigned>()->default_value(10),"factor by which to sub-sample trees")
	("partition", value<string>(), "find indels along internal branch that bi-partitions given taxa (<taxa1>:<taxa2>:...)")
	("alphabet",value<string>(),"set to 'Codons' to prefer codon alphabets")
	("verbose,V",value<int>()->implicit_value(1),"Show more log messages on stderr.")
	("extract-sequences",value<string>(),"Extract sequences corresponding to tree")
	("details", "output individual indel positions instead of counts")
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

    if (args.count("verbose")) log_verbose = args["verbose"].as<int>();

    return args;
}

int main(int argc,char* argv[])
{ 
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
