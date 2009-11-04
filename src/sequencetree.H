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


/// An (ordered) set of sequences
class SequenceSet {
protected:
  /// implements the ordered set of sequences
  std::vector<std::string> sequences;

public:

  /// get a single sequence
  std::string& seq(int i) {return sequences[i];}
  const std::string& seq(int i) const {return sequences[i];}

  /// get all the sequences
  std::vector<std::string>& get_sequences() {return sequences;}
  const std::vector<std::string>& get_sequences() const {return sequences;}

  /// find the index for a particular sequence
  int index(const std::string&) const;

  SequenceSet() {}
  SequenceSet(const std::vector<std::string>& vs):sequences(vs) {}
};


/// A Tree with leaves labelled by sequences
class SequenceTree: virtual public Tree, public SequenceSet {

public:
  /// remove a subtree - SequenceTree variants duplicated  in both {U,R}SequenceTree
  nodeview prune_subtree(int b);

  /// remove a subtree - SequenceTree variants duplicated  in both {U,R}SequenceTree
  std::vector<int> prune_leaves(const std::vector<int>&);

  /// generate standard internal node and internal branch names given current leaf names
  virtual std::vector<int> standardize();

  /// generate standard internal node and internal branch names given specified leaf names
  virtual std::vector<int> standardize(const std::vector<int>& mapping);

  /// compute a string representing the tree, w/ or w/o branch lengths
  virtual std::string write(bool print_lengths=true) const;

  /// compute a string representing the tree, w/ or w/o branch lengths
  virtual std::string write_with_bootstrap_fraction(const std::vector<double>& bf, bool print_lengths=true) const;

  /// load this tree from the string @s
  virtual int parse(const std::string& s);
  virtual int parse_nexus(const std::string& s,const std::vector<std::string>& names);

  /// load this tree from the file @file
  virtual void read(std::istream& file);

  /// load this tree from the file "@filename"
  virtual void read(const std::string& filename);

  /// make an empty sequenctree
  SequenceTree() {}

  /// create from  @file
  explicit SequenceTree(std::istream& file);

  /// create a copy of @T with leaves named according to @names
  explicit SequenceTree(const Tree& T,const std::vector<std::string>& names);

  /// create a tree with a single node named @s
  explicit SequenceTree(const std::string& s);

  /// Create a SequenceTree from a RootedSequenceTree (doesn't delete the root node)
  SequenceTree(const class RootedSequenceTree&);
};


/// A RootedTree with leaves labelled by sequences
class RootedSequenceTree: public RootedTree, public SequenceTree 
{
public:
  /// remove a subtree - SequenceTree variants duplicated  in both {U,R}SequenceTree
  nodeview prune_subtree(int b);

  /// remove a subtree - SequenceTree variants duplicated  in both {U,R}SequenceTree
  std::vector<int> prune_leaves(const std::vector<int>&);

  /// compute a string representing the tree, w/ or w/o branch lengths
  std::string write(bool=true) const;

  std::string write_with_bootstrap_fraction(const std::vector<double>&, bool print_lengths=true) const;

  /// load this tree from the string @s
  int parse(const std::string& s);
  int parse_nexus(const std::string& s,const std::vector<std::string>&);

  RootedSequenceTree& operator=(const RootedSequenceTree&);

  /// create from  @file
  RootedSequenceTree() {}

  /// create from  @file
  explicit RootedSequenceTree(std::istream& file);

  /// create a tree with a single node named @s
  explicit RootedSequenceTree(const std::string& s);

  /// create a copy of @RT with leaves named according to @names
  explicit RootedSequenceTree(const RootedTree& RT,const std::vector<std::string>& names);

  /// Create the tree (T1,T2)
  RootedSequenceTree(const RootedSequenceTree& T1, const RootedSequenceTree& T2);

  /// Create a RootedSequenceTree from a SequenceTree, naming node @r as the root
  RootedSequenceTree(const SequenceTree&,int r);
};

RootedSequenceTree operator+(const RootedSequenceTree& t1,const RootedSequenceTree& t2);

RootedSequenceTree add_root(SequenceTree,int b);

std::istream& operator >>(std::istream& i,SequenceTree& T);

std::ostream& operator <<(std::ostream& o,const SequenceTree& T);

bool same_topology(const SequenceTree&,const SequenceTree&);

SequenceTree RandomTree(const std::vector<std::string>&,double);

void RandomTree(Tree&,double);

void delete_node(SequenceTree& T,const std::string& name);

SequenceTree star_tree(const std::vector<std::string>&);

double internal_branch_distance(const SequenceTree& T1, const SequenceTree& T2);
double branch_distance(const SequenceTree& T1, const SequenceTree& T2);
double robinson_foulds_distance(const SequenceTree& T1, const SequenceTree& T2);
unsigned topology_distance(const SequenceTree& T1, const SequenceTree& T2);

void standardize(RootedSequenceTree& T);

void standardize(SequenceTree& T);

RootedSequenceTree standardized(const std::string& t);

#endif
