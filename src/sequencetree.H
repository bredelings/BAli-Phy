#ifndef SEQUENCETREE_H
#define SEQUENCETREE_H

#include <vector>
#include <string>
#include <istream>
#include "tree.H"


//----------------- class SequenceTree ----------------//
class SequenceSet {
protected:
  std::vector<std::string> sequences;

public:

  std::string& seq(int i) {return sequences[i];}
  const std::string& seq(int i) const {return sequences[i];}

  std::vector<std::string>& get_sequences() {return sequences;}
  const std::vector<std::string>& get_sequences() const {return sequences;}

  int index(const std::string&) const;
};

class SequenceTree: public Tree, public SequenceSet {

public:
  /// remove a subtree - SequenceTree variants duplicated  in both {U,R}SequenceTree
  nodeview prune_subtree(int b);

  /// standardize - SequenceTree variants duplicated  in both {U,R}SequenceTree
  std::vector<int> standardize();
  std::vector<int> standardize(const std::vector<int>&);

  std::string write(bool=true) const;

  void read(const std::string&);
  void read(std::istream&);
  void parse(const std::string&);

  SequenceTree() {}
  explicit SequenceTree(std::istream&);   //load a tree from a file
  explicit SequenceTree(const std::string& s);
  explicit SequenceTree(const class RootedSequenceTree&);
};

class RootedSequenceTree: public RootedTree, public SequenceSet {
  std::string write(const_branchview,bool) const;

public:
  /// remove a subtree - SequenceTree variants duplicated  in both {U,R}SequenceTree
  nodeview prune_subtree(int b);

  /// standardize - SequenceTree variants duplicated  in both {U,R}SequenceTree
  std::vector<int> standardize();
  std::vector<int> standardize(const std::vector<int>&);

  std::string write(bool=true) const;

  void read(const std::string&);
  void read(std::istream&);
  void parse(const std::string&);

  RootedSequenceTree() {}
  explicit RootedSequenceTree(std::istream&);   //load a tree from a file
  explicit RootedSequenceTree(const std::string& s);
  RootedSequenceTree(const RootedSequenceTree& T1, const RootedSequenceTree& T2);

  //conversions...
  explicit  RootedSequenceTree(const SequenceTree&,int r);
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

SequenceTree remove_root_branch(RootedSequenceTree RT);

#endif
