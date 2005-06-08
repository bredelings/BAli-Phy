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
class SequenceTree: public Tree, public SequenceSet {

public:
  /// remove a subtree - SequenceTree variants duplicated  in both {U,R}SequenceTree
  nodeview prune_subtree(int b);

  /// generate standard internal node and internal branch names given current leaf names
  std::vector<int> standardize();

  /// generate standard internal node and internal branch names given specified leaf names
  std::vector<int> standardize(const std::vector<int>& mapping);

  /// compute a string representing the tree, w/ or w/o branch lengths
  std::string write(bool print_lengths=true) const;

  /// load this tree from the string @s
  void parse(const std::string& s);

  /// load this tree from the file @file
  void read(std::istream& file);

  /// load this tree from the file "@filename"
  void read(const std::string& filename);

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
class RootedSequenceTree: public RootedTree, public SequenceSet {
  std::string write(const_branchview,bool) const;

public:
  /// remove a subtree - SequenceTree variants duplicated  in both {U,R}SequenceTree
  nodeview prune_subtree(int b);

  /// generate standard internal node and internal branch names given current leaf names
  std::vector<int> standardize();

  /// generate standard internal node and internal branch names given specified leaf names
  std::vector<int> standardize(const std::vector<int>&);

  /// compute a string representing the tree, w/ or w/o branch lengths
  std::string write(bool=true) const;

  /// load this tree from the string @s
  void parse(const std::string& s);

  /// load this tree from the file @file
  void read(std::istream& file);

  /// load this tree from the file "@filename"
  void read(const std::string& filename);

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
SequenceTree remove_root(const RootedSequenceTree&);

std::istream& operator >>(std::istream& i,SequenceTree& T);
std::istream& operator >>(std::istream& i,RootedSequenceTree& T);

std::ostream& operator <<(std::ostream& o,const SequenceTree& T);
std::ostream& operator <<(std::ostream& o,const RootedSequenceTree& T);

bool same_topology(const SequenceTree&,const SequenceTree&);

SequenceTree RandomTree(const std::vector<std::string>&,double=1.0);

void delete_node(SequenceTree& T,const std::string& name);

SequenceTree star_tree(const std::vector<std::string>&);

double branch_distance(const SequenceTree& T1, const SequenceTree& T2);
double topology_distance(const SequenceTree& T1, const SequenceTree& T2);

#endif
