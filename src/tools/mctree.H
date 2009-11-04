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
#include "mytypes.H"

bool partition_wanders_over(const Partition& p1,const Partition& p2);

bool partition_less_than(const Partition& p1,const Partition& p2);

std::vector<Partition> get_moveable_tree(std::vector<Partition> partitions);

struct mc_tree_edge {
  int from;
  int to;
  int type;
  int partition;

  mc_tree_edge(int f,int t,int T,int p)
    :from(f),to(t),type(T),partition(p)
  {}
};

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

public:
  SequenceTree T;

  std::vector<Partition> partitions;
  std::vector<mc_tree_edge> edges;

  // map (directed) branches to node names
  std::vector<int> mapping;

  // partition properties
  ublas::matrix<int> left_of;                // i<j
  ublas::matrix<int> wanders_over;           // right end wanders
  ublas::matrix<int> directly_left_of;       // i<j
  ublas::matrix<int> directly_wanders_over;  // right end (group2)
  ublas::matrix<int> connected_to;           // right end
  vector<int> directly_wanders;              // right end

  // node properties
  ublas::matrix<int> connected;

  // list of unique branches
  vector<int> branch_order;

  int score() const;

  int branch_to_node(int n) const;
  int find_branch(const Partition&) const;
  int degree(int n) const;
  bool is_leaf_node(int n) const;
  int leaf(int i) const;

  const std::vector<std::string>& names() const {return names_;}

  int n_leaves() const {return names_.size();}
  int n_branches() const {return N;}
  int n_nodes() const {return C;}
  int reverse(int i) const {return reverse_[i];}

  MC_tree(const std::vector<Partition>&);
};

void draw_graph(const MC_tree& T,const std::string& name);

MC_tree load_MC_tree(const std::string&);
std::vector<Partition> check_MC_partitions(const std::vector<Partition>&);
void check_partitions_informative(const std::vector<Partition>&);

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
