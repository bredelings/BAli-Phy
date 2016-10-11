/*
  Copyright (C) 2007-2009 Benjamin Redelings

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

#ifndef MCTREE_H
#define MCTREE_H

#include <vector>
#include <string>
#include "tree-dist.H"
#include "matrix.H"

bool partition_wanders_over(const partition& p1,const partition& p2);

bool partition_less_than(const partition& p1,const partition& p2);

std::vector<partition> get_moveable_tree(std::vector<partition> partitions);

struct mc_tree_edge {
    int from;
    int to;
    int type;
    int partition;

    mc_tree_edge(int f,int t,int T,int p)
	:from(f),to(t),type(T),partition(p)
	{}
};

// An MC tree is a collection of partitions
//  * each partition occurs twice - once for each orientation
//  * each orientation correspond to a node - one of the endpoints of the branch
//  * some of these endpoints are identified, forming a condensed node
//    + C is the number of condensed nodes
//    + the mapping from endpoints to condensed nodes is done via mapping[]
// There is also a multifurcating tree.
//  * the full partitions coming first in the list and correspond to the branches on the tree
//  * each condensed node (and therefore each endpoint) maps to a tree node via 
//    the function graph_node_to_tree_node().
// Ideally, we would also make a *second* tree that is an embedded tree of the graph
//  * we may have to handle parallel edges specially in order to make sure that
//    every (type 1) edge in the graph corresponds to a unique tree edge.     


struct MC_tree 
{
    // number of branches
    int N;

    // number of nodes
    int C;

    // leaf names
    std::vector<std::string> names_;

    // index of reversed branch
    std::vector<int> reverse_;

    std::vector<int> graph_node_to_tree_node_;

    std::vector<int> map_graph_nodes_to_tree_nodes();

public:
    SequenceTree T;

    std::vector<partition> partitions;
    std::vector<mc_tree_edge> edges;

    // map (directed) branches to node names
    std::vector<int> mapping;

    // partition properties
    matrix<int> left_of;                // i<j
    matrix<int> wanders_over;           // right end wanders
    matrix<int> directly_left_of;       // i<j
    matrix<int> directly_wanders_over;  // right end (group2)
    matrix<int> connected_to;           // right end
    std::vector<int> directly_wanders;              // right end

    // node properties
    matrix<int> connected;

    // list of unique branches
    std::vector<int> branch_order;

    /// return a score that is higher for a more resolved tree
    int score() const;

    /// find the index of any partition that points to node n
    int branch_to_node(int n) const;
    /// find the index of the given partition
    int find_branch(const partition&) const;
    /// find the degree of a node n
    int degree(int n) const;
    /// check if node n is a leaf node
    bool is_leaf_node(int n) const;
    /// find the name of the leaf node with taxon i
    int leaf(int i) const;

    const std::vector<std::string>& names() const {return names_;}

    int n_leaves() const {return names_.size();}
    int n_branches() const {return N;}
    int n_nodes() const {return C;}
    int reverse(int i) const {return reverse_[i];}

    int graph_node_to_tree_node(int n) const {return graph_node_to_tree_node_[n];}

    MC_tree(const PTree&);
};

void draw_graph(const MC_tree& T,const std::string& name);

MC_tree load_MC_tree(const std::string&);
PTree check_MC_partitions(const PTree&);
PTree check_MC_partitions(const std::vector<std::string>&, const std::vector<partition>&);
void check_partitions_informative(const std::vector<partition>&);
void check_partitions_informative(const PTree&);

std::ostream& operator<<(std::ostream& o, const MC_tree&);

std::string get_graph_name(std::string filename);

// Resolve: resolve an MC tree to an MF tree, either
// (a) making sure that no branches resolve to the same thing, or
// (b) removing duplicate resolved branches, while 
//     maintaining a mapping of old->new branch indices.


// Check: Is C an MC tree?  Given that C is an MC tree, is C+X an MC tree?
//        How can we improve the order on this?
//        If we can improve the order, then we could compute #sub-partitions.
#endif
