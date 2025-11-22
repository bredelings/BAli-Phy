/*
  Copyright (C) 2005-2007,2009 Benjamin Redelings

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
#include <map>
#include <list>
#include <cmath>
#include "tree/tree-util.H"
#include "util/myexception.H"
#include "util/io.H"
#include "tools/read-trees.H" // split out trees_format::reader_t

using boost::program_options::variables_map;
using std::map;
using std::string;
using std::string_view;
using std::vector;
using std::list;
using std::istream;
using std::cout;
using std::cerr;
using std::endl;

using boost::dynamic_bitset;

namespace fs = std::filesystem;

/// Load a tree from command line args "--tree filename"
RootedSequenceTree load_tree_from_file(const fs::path& filename)
{
    using namespace trees_format;

    std::shared_ptr<reader_t> trees_in(new Newick_or_NEXUS(filename));
    RootedSequenceTree RT;
    if (not trees_in->next_tree(RT))
	throw myexception()<<"No tree in file "<<filename;

    return RT;
}

/// Load a tree from command line args "--tree filename"
RootedSequenceTree load_T(const variables_map& args) {
    if (not args.count("tree"))
	throw myexception()<<"Tree file not specified! (--tree <filename>)";
    
    auto filename = args["tree"].as<string>();
    return load_tree_from_file(filename);
}

std::optional<SequenceTree> parse_sequence_tree(const string& text)
{
    RootedSequenceTree T;
    try {
        T.parse(text);
        return T;
    }
    catch (...)
    {
        return {};
    }
}

vector<SequenceTree> load_trees(const vector<string>& lines) 
{
    if (lines.size() == 0)
	throw myexception()<<"No trees were read in!";
  
    vector<SequenceTree> trees;

    for(int i=0;i<lines.size();i++) 
    {
	RootedSequenceTree T;
	try {
	    T.parse(lines[i]);
	}
	catch (std::exception& e) {
	    cerr<<"Exception: "<<e.what()<<endl;
	    cerr<<" Quitting read of tree file"<<endl;
	    break;
	}

	trees.push_back(T);
    }

    return trees;
}

vector<SequenceTree> load_trees(istream& file,int skip,int subsample,int max)
{
    vector<string> lines = load_lines(file,skip,subsample,max);
    return load_trees(lines);
}

bool 
compare_complete_partitions::operator()(const dynamic_bitset<>& p1,
					const dynamic_bitset<>& p2) const
{
    assert(p1.size() == p2.size());

    for(int i=0;i<p1.size();i++) {
	if (p2[i] and not p1[i])
	    return true;
	if (p1[i] and not p2[i])
	    return false;
    }
    return false;
}

bool is_subset(const list<int>& L1,const list<int>& L2)
{
    list<int>::const_iterator i1 = L1.begin();
    list<int>::const_iterator i2 = L2.begin();

    while(i1 != L1.end())
    {
	if (i2 == L2.end()) return false;

	if (*i1 > *i2) 
	    i2++;
	else if (*i1 == *i2) {
	    i1++;
	    i2++;
	}
	else 
	    return false;
    }
    return true;
}


struct node_info
{
    bool deleted;
    int degree;
    int leaf_degree;
    list<int> adjacent_leaves;
    node_info():deleted(false),degree(0),leaf_degree(0) { }
};


vector<int> extends_map(const Tree& T,const Tree& Q)
{
    if (T.n_branches() < Q.n_branches())
	return vector<int>();

    // set up the branch map
    vector<int> branch_map(Q.n_branches()*2,-1);
    for(int i=0;i<Q.n_leafbranches();i++) 
    {
	const_branchview Qb = Q.directed_branch(Q.leaf_branch(i));
	const_branchview Tb = T.directed_branch(T.leaf_branch(i));

	assert(Qb.name() == i);
    
	branch_map[Qb.name()] = Tb.name();
	branch_map[Qb.reverse().name()] = Tb.reverse().name();
    }
    
    // collect pointers to leaves of both trees
    vector<BranchNode*> leaves_Q(Q.n_leaves());

    vector<BranchNode*> leaves_T(T.n_leaves());

    vector<node_info> nodes_Q(Q.n_nodes());
    vector<node_info> nodes_T(T.n_nodes());

    // compute degree of each node
    for(int i=0;i<Q.n_nodes();i++)
	nodes_Q[i].degree = Q.node(i).degree();

    for(int i=0;i<T.n_nodes();i++)
	nodes_T[i].degree = T.node(i).degree();

    // compute leaf degree and leaf neighbors of each node
    for(int i=0;i<Q.n_leaves();i++) 
    {
	leaves_Q[i] = Q.nodes_[i];
	int q = Q.branch(i).target();
	nodes_Q[q].leaf_degree++;
	nodes_Q[q].adjacent_leaves.push_back(i);
    }

    for(int i=0;i<T.n_leaves();i++) 
    {
	leaves_T[i] = T.nodes_[i];
	int p = T.branch(i).target();
	nodes_T[p].leaf_degree++;
	nodes_T[p].adjacent_leaves.push_back(i);
    }

    // we just need to have BN->out == parent
    for(int leaf=0;leaf<leaves_T.size();leaf++)
    {
	if (not leaves_T[leaf]) continue;

	while (nodes_T[leaves_T[leaf]->out->node_attributes->name].leaf_degree >= 2)
	{
	    int p = leaves_T[leaf]->out->node_attributes->name;
	    int q = leaves_Q[leaf]->out->node_attributes->name;

	    if (nodes_Q[q].leaf_degree == nodes_Q[q].degree)
		goto out;

	    list<int>& LT = nodes_T[p].adjacent_leaves;
	    list<int>& LQ = nodes_Q[q].adjacent_leaves;

	    if (not is_subset(LT,LQ)) return vector<int>();

	    // remove all except first leaf node
	    int L = nodes_T[p].leaf_degree;

	    nodes_T[p].degree -= (L-1);
	    nodes_Q[q].degree -= (L-1);

	    nodes_T[p].leaf_degree -= (L-1);
	    nodes_Q[q].leaf_degree -= (L-1);

	    for(list<int>::iterator j = LT.begin(); j!= LT.end();) 
		if (*j == leaf)
		    j++;
		else {
		    int n1 = leaves_T[*j]->node_attributes->name;
		    int n2 = leaves_Q[*j]->node_attributes->name;
		    leaves_T[*j] = NULL;
		    leaves_Q[*j] = NULL;
		    nodes_T[n1].deleted = true;
		    nodes_Q[n2].deleted = true;

		    list<int>::iterator j2 = j;
		    j++;
		    LT.erase(j2);
		}
      
	    for(list<int>::iterator j = LQ.begin(); j!= LQ.end();) {
		list<int>::iterator j2 = j;
		j++;
		if (not leaves_Q[*j2])
		    LQ.erase(j2);
	    }

	    assert(leaves_T[leaf]);
	    assert(leaves_Q[leaf]);

	    if (nodes_Q[q].degree == 2) 
		assert(nodes_T[p].degree == 2);

	    if (nodes_T[p].degree == 2) 
	    {
		BranchNode* N = leaves_T[leaf];
		nodes_T[N->node_attributes->name].deleted = true;
		N = N->out;
		while(nodes_T[N->out->node_attributes->name].deleted)
		    N = N->next;
		leaves_T[leaf] = N;
		p = N->out->node_attributes->name;
		nodes_T[p].adjacent_leaves.merge(LT);
		nodes_T[p].leaf_degree++;
	    }

	    if (nodes_Q[q].degree == 2) 
	    {
		BranchNode* N = leaves_Q[leaf];
		nodes_Q[N->node_attributes->name].deleted = true;
		N = N->out;
		while(nodes_Q[N->out->node_attributes->name].deleted)
		    N = N->next;
		leaves_Q[leaf] = N;
		q = N->out->node_attributes->name;
		nodes_Q[q].adjacent_leaves.merge(LQ);
		nodes_Q[q].leaf_degree++;

		branch_map[leaves_Q[leaf]->directed_branch_attributes->name] = leaves_T[leaf]->directed_branch_attributes->name;
		branch_map[leaves_Q[leaf]->out->directed_branch_attributes->name] = leaves_T[leaf]->out->directed_branch_attributes->name;
	    }
	}
    }
    std::abort();
out:

    for(int i=0;i<branch_map.size();i++)
	assert(branch_map[i] != -1);
  
    return branch_map;
}

bool extends(const Tree& T,const Tree& Q)
{
    if (Q.n_branches() == Q.n_leafbranches())
	return true;

    vector<int> branch_map = extends_map(T,Q);
    return branch_map.size() != 0;
}

void validate_initial_tree_branch_lengths(const SequenceTree& tree)
{
    for (int b = 0; b < tree.n_branches(); b++)
    {
        auto branch = tree.directed_branch(b);
        if (!branch.has_length())
            throw myexception() << "Initial tree branch " << b << " has no branch length.\n"
                               << "  All branches must have lengths.";

        double len = branch.length();
        if (len <= 1e-10)
            throw myexception() << "Initial tree branch " << b << " has invalid length: " << len
                               << "\n  All branch lengths must be positive (> 1e-10).";
        if (std::isnan(len) || std::isinf(len))
            throw myexception() << "Initial tree branch " << b << " has NaN/infinite length";
    }
}


