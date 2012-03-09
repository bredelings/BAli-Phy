/*
   Copyright (C) 2004-2007,2009 Benjamin Redelings

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

#ifndef SEQUENCETREE_H
#define SEQUENCETREE_H

#include <vector>
#include <string>
#include <istream>
#include "tree.H"


/// A Tree with leaves labelled by sequences
class SequenceTree: virtual public Tree
{
public:
  /// get a single label
  void  set_label(int i, const std::string& s);
  const std::string& get_label(int i) const;

  /// get all the labels
  std::vector<std::string> get_labels() const;

  /// get the leaf labels
  std::vector<std::string> get_leaf_labels() const;

  /// find the index for a particular sequence
  int index(const std::string&) const;

  /// compute a string representing the tree, w/ or w/o branch lengths
  virtual std::string write(bool print_lengths=true) const;

  /// compute a string representing the tree, w/ or w/o branch lengths
  virtual std::string write_with_bootstrap_fraction(const std::vector<double>& bf, bool print_lengths=true) const;

  /// add a degree 0 , linking to node 'n'.  However, the index will not be a leaf index
  virtual nodeview add_leaf_node(int n);

  /// Add a node onto a branch 'b', splitting it into two sub-neighbors
  virtual nodeview create_node_on_branch(int b);

  /// load this tree from the string \a s
  virtual int parse(const std::string& s);
  virtual int parse_nexus(const std::string& s,const std::vector<std::string>& names);

  /// Parse and load the Newick format string 's', discovering and reporting leaf names in 'names'
  virtual int parse_and_discover_names(const std::string& s);
  /// Parse and load the Newick format string 's', where node names are given in 'names', or numerical starting at 1
  virtual int parse_with_names_or_numbers(const std::string& s, const std::vector<std::string>& names, bool allow_numbers=true);

  /// load this tree from the file \a file
  virtual void read(std::istream& file);

  /// load this tree from the file "\a filename"
  virtual void read(const std::string& filename);

  /// make an empty sequenctree
  SequenceTree() {}

  /// create from  \a file
  explicit SequenceTree(std::istream& file);

  /// create a copy of \a T with leaves named according to \a names
  explicit SequenceTree(const Tree& T,const std::vector<std::string>& names);

  /// create a tree with a single node named \a s
  explicit SequenceTree(const std::string& s);
};


/// A RootedTree with leaves labelled by sequences
class RootedSequenceTree: public RootedTree, public SequenceTree 
{
public:
  /// remove some leaves - SequenceTree variants duplicated  in both {U,R}SequenceTree
  std::vector<int> prune_leaves(const std::vector<int>&);

  /// compute a string representing the tree, w/ or w/o branch lengths
  std::string write(bool=true) const;

  std::string write_with_bootstrap_fraction(const std::vector<double>&, bool print_lengths=true) const;

  /// Parse and load the Newick format string 's', discovering and reporting leaf names in 'names'
  int parse_and_discover_names(const std::string& s);
  /// Parse and load the Newick format string 's', where node names are given in 'names', or numerical starting at 1
  int parse_with_names_or_numbers(const std::string& s, const std::vector<std::string>& names, bool allow_numbers=true);

  /// load this tree from the string \a s
  int parse(const std::string& s);
  int parse_nexus(const std::string& s,const std::vector<std::string>&);

  /// create from  \a file
  RootedSequenceTree() {}

  /// create from  \a file
  explicit RootedSequenceTree(std::istream& file);

  /// create a tree with a single node named \a s
  explicit RootedSequenceTree(const std::string& s);

  /// create a copy of \a RT with leaves named according to \a names
  explicit RootedSequenceTree(const RootedTree& RT,const std::vector<std::string>& names);

  /// Create the tree (T1,T2)
  RootedSequenceTree(const RootedSequenceTree& T1, const RootedSequenceTree& T2);

  /// Create a RootedSequenceTree from a SequenceTree, naming node \a r as the root
  RootedSequenceTree(const SequenceTree&,int r);
};

RootedSequenceTree operator+(const RootedSequenceTree& t1,const RootedSequenceTree& t2);

RootedSequenceTree add_root(SequenceTree,int b);

std::istream& operator >>(std::istream& i,SequenceTree& T);

std::ostream& operator <<(std::ostream& o,const SequenceTree& T);

bool same_topology(const SequenceTree&,const SequenceTree&);

SequenceTree RandomTree(const std::vector<std::string>&,double);

void RandomTree(Tree&,double);

SequenceTree star_tree(const std::vector<std::string>&);

double internal_branch_distance(const Tree& T1, const Tree& T2);
double branch_distance(const Tree& T1, const Tree& T2);
double robinson_foulds_distance(const Tree& T1, const Tree& T2);
unsigned topology_distance(const Tree& T1, const Tree& T2);

void standardize(RootedSequenceTree& T);

void standardize(SequenceTree& T);

RootedSequenceTree standardized(const std::string& t);

#endif
