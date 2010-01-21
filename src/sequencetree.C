/*
   Copyright (C) 2004-2009 Benjamin Redelings

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
#include <fstream>
#include <cmath>
#include "sequencetree.H"
#include "myexception.H"
#include "util.H"

using namespace std;

using boost::dynamic_bitset;

int SequenceSet::index(const string& s) const {
  for(int i=0;i<sequences.size();i++)
    if (sequences[i] == s) return i;
  return -1;
}


//-------------------------- SequenceTree methods ----------------------------//
nodeview SequenceTree::prune_subtree(int branch) {
  // get pointers to current leaves
  vector<BranchNode*> leaves1(n_leaves());
  for(int i=0;i<leaves1.size();i++)
    leaves1[i] = nodes_[i];
  
  // remove the subtree
  nodeview node_remainder = Tree::prune_subtree(branch);
  
  // get pointers to NEW leaves
  vector<BranchNode*> leaves2(n_leaves());
  for(int i=0;i<leaves2.size();i++)
    leaves2[i] = nodes_[i];

  // figure out the mapping
  vector<string> newnames;
  for(int i=0;i<leaves2.size();i++) {
    int index = find_index(leaves1,leaves2[i]);
    if (index == -1)
      break;
    else
      newnames.push_back(sequences[index]);
  }
  assert(newnames.size() == leaves2.size());

  // select the new names
  sequences = newnames;

  return node_remainder;
}

vector<int> SequenceTree::prune_leaves(const vector<int>& remove) 
{
  // remove the subtree
  vector<int> mapping = Tree::prune_leaves(remove);

  // figure out the mapping
  vector<string> newnames(mapping.size());
  for(int i=0;i<mapping.size();i++)
    newnames[i] = sequences[mapping[i]];

  // select the new names
  sequences = newnames;

  return mapping;
}

void SequenceTree::read(const string& filename) {
  ifstream file(filename.c_str());
  if (not file) 
    throw myexception()<<"Couldn't open file '"<<filename<<"'";
  read(file);
  file.close();
}

void SequenceTree::read(std::istream& file) {
  assert(file);

  string total;
  string line;
  while(getline_handle_dos(file,line))
    total += line;
  parse(total);
}

string SequenceTree::write(bool print_lengths) const 
{
  RootedSequenceTree RT(*this,directed_branch(0).target());
  return RT.write(print_lengths);
}

string SequenceTree::write_with_bootstrap_fraction(const vector<double>& bf, bool print_lengths) const 
{
  RootedSequenceTree RT(*this,directed_branch(0).target());
  return RT.write_with_bootstrap_fraction(bf, print_lengths);
}

vector<int> SequenceTree::standardize() {
  return Tree::standardize();
}


vector<int> SequenceTree::standardize(const vector<int>& lnames) {
  assert(lnames.size() == sequences.size());

  vector<string> old = sequences;
  for(int i=0;i<sequences.size();i++)
    sequences[lnames[i]] = old[i];

  return Tree::standardize(lnames);
}

// count depth -> if we are at depth 0, and have
// one object on the stack then we quit
int SequenceTree::parse(const string& line) 
{
  return parse_and_discover_names(line,sequences);
}

int SequenceTree::parse_nexus(const string& s,const vector<string>& names) 
{
  sequences = names;
  int r = parse_with_names_or_numbers(s, sequences);
  return r;
}

SequenceTree::SequenceTree(const std::string& s) {
  add_first_node();
  sequences.push_back(s);
}

SequenceTree::SequenceTree(const Tree& T,const vector<string>& names)
  :Tree(T),SequenceSet(names)
{ }

SequenceTree::SequenceTree(const RootedSequenceTree& RT) 
  :Tree(RT),SequenceSet(RT)
{ }

//-------------------------- SequenceTree methods ----------------------------//
nodeview RootedSequenceTree::prune_subtree(int branch) {
  // get pointers to current leaves
  vector<BranchNode*> leaves1(n_leaves());
  for(int i=0;i<leaves1.size();i++)
    leaves1[i] = nodes_[i];
  
  // remove the subtree
  nodeview node_remainder = RootedTree::prune_subtree(branch);
  
  // get pointers to NEW leaves
  vector<BranchNode*> leaves2(n_leaves());
  for(int i=0;i<leaves2.size();i++)
    leaves2[i] = nodes_[i];

  // figure out the mapping
  vector<string> newnames;
  for(int i=0;i<leaves2.size();i++) {
    int index = find_index(leaves1,leaves2[i]);
    if (index == -1)
      break;
    else
      newnames.push_back(sequences[index]);
  }

  assert( (newnames.size() == n_leaves()) or 
	  ((newnames.size() == n_leaves()-1) and (root_->node == n_leaves())) );

  // select the new names
  sequences = newnames;

  return node_remainder;
}

vector<int> RootedSequenceTree::prune_leaves(const vector<int>& remove) 
{
  root_ = NULL;

  // if we need to do this, virtualize unlink_subtree to complain if the subtree
  // contains the root.

  return SequenceTree::prune_leaves(remove);
}

string write_with_bootstrap_fraction(const vector<string>& names, const_branchview b, 
				     const vector<double>& bf, bool print_lengths)
{
  string output;

  // If this is a leaf node, then print the name
  if (b.target().is_leaf_node())
    output += names[b.target()];
  // If this is an internal node, then print the subtrees
  else {
    vector<const_branchview> branches = sorted_branches_after(b);
    output = "(";
    for(int i=0;i<branches.size();i++) {
      output += write_with_bootstrap_fraction(names,branches[i],bf,print_lengths);

      if (i+1<branches.size())
	output += ",";
    }
    output += ")";
  }

  // print the branch length if requested
  double bfb = bf[b.undirected_name()];
  if (bfb >= 0)
    output += " " + convertToString<double>(bf[b.undirected_name()]);

  if (print_lengths)
    output += ":" + convertToString(b.length());
  else if (bfb >= 0)
    output += ":1.0";

  return output;
}

string write_with_bootstrap_fraction(const RootedTree& T, const vector<string>& names, 
				     const vector<double>& bf, bool print_lengths) 
{
  vector<const_branchview> branches = sorted_neighbors(T.root());

  string output = "(";
  for(int i=0;i<branches.size();i++) {
    output += write_with_bootstrap_fraction(names,branches[i],bf,print_lengths);
    if (i+1 < branches.size())
      output += ',';
  }
  output += ");";
  return output;
}

string RootedSequenceTree::write(bool print_lengths) const 
{
  return ::write(*this, get_sequences(), print_lengths);
}

string RootedSequenceTree::write_with_bootstrap_fraction(const vector<double>& bf, bool print_lengths) const 
{
  return ::write_with_bootstrap_fraction(*this, get_sequences(), bf, print_lengths);
}

int RootedSequenceTree::parse(const string& s) 
{
  int r = SequenceTree::parse(s);

  root_ = nodes_[r];

  return r;
}

int RootedSequenceTree::parse_nexus(const string& s, const vector<string>& names) 
{
  sequences = names;
  int r = parse_with_names_or_numbers(s, sequences);
  return r;
}

RootedSequenceTree& RootedSequenceTree::operator=(const RootedSequenceTree& T)
{
  RootedTree::operator=(T);
  SequenceSet::operator=(T);

  return *this;
}


RootedSequenceTree::RootedSequenceTree(const RootedTree& T,const vector<string>& names)
  :Tree(T),RootedTree(T),SequenceTree(T,names)
{ }

RootedSequenceTree::RootedSequenceTree(const SequenceTree& T,int r) 
  :Tree(T),RootedTree(T,r),SequenceTree(T)
{ }


RootedSequenceTree::RootedSequenceTree(const string& s)
{
  add_first_node();
  sequences.push_back(s);
}

RootedSequenceTree::RootedSequenceTree(istream& file) {
  read(file);
}

RootedSequenceTree::RootedSequenceTree(const RootedSequenceTree& T1, const RootedSequenceTree& T2)
  :RootedTree(T1,T2) 
{
  // We will create new names which will be the same as
  //  T1.order + T2.order
  for(int i=0;i<T1.get_sequences().size();i++) 
    sequences.push_back(T1.seq(i));
  for(int i=0;i<T2.get_sequences().size();i++) 
    sequences.push_back(T2.seq(i));
}

//FIXME T.seq(i) -> T.leafname(i)
//FIXME T.get_sequences -> T.leafnames()
void delete_node(SequenceTree& T,const std::string& name) 
{
  int index = find_index(T.get_sequences(),name);
  nodeview n = T.prune_subtree(T.branch(index).reverse());
  T.remove_node_from_branch(n);
}

RootedSequenceTree add_root(SequenceTree T,int b) {
  int r = T.create_node_on_branch(b);
  return RootedSequenceTree(T,r);
}
  
RootedSequenceTree operator+(const RootedSequenceTree& t1,const RootedSequenceTree& t2) 
{
  RootedSequenceTree t3(t1,t2);
  int new_root = t3.add_node(t3.root());
  t3.reroot(new_root);

  return t3;
}

std::istream& operator >>(std::istream& i,SequenceTree& T) 
{
  string line;
  while(getline_handle_dos(i,line)) {
    if (not line.empty()) {
      T.parse(line);
      return i;
    }
  }
  throw myexception()<<"Failed to read tree: file ended.";
}

std::ostream& operator <<(std::ostream& o,const SequenceTree& T) {
  return o<<T.write();
}

SequenceTree star_tree(const vector<string>& names) 
{
  return SequenceTree(star_tree(names.size()), names);
}

int find_partition(const dynamic_bitset<>& p1, const vector< dynamic_bitset<> >& pv) {
  dynamic_bitset<> np1 = ~p1;
  for(int i=0;i<pv.size();i++) {
    if ((pv[i] == p1) or (pv[i]==np1))
      return i;
  }
  return -1;
}

double branch_distance(const Tree& T1, const Tree& T2) 
{
  assert(T1.n_leaves() == T2.n_leaves());

  vector<double> d1(T1.n_branches());
  vector< dynamic_bitset<> > part1(T1.n_branches(),dynamic_bitset<>(T1.n_leaves()));

  vector<double> d2(T2.n_branches());
  vector< dynamic_bitset<> > part2(T1.n_branches(),dynamic_bitset<>(T2.n_leaves()));

  // get partitions and lengths for T1
  for(int b=0;b<T1.n_branches();b++) {
    d1[b] = T1.branch(b).length();
    part1[b] = branch_partition(T1,b);
  }

  // get partitions and lengths for T2
  for(int b=0;b<T2.n_branches();b++) {
    d2[b] = T2.branch(b).length();
    part2[b] = branch_partition(T2,b);
  }

  // Accumulate distances for T1 partitions
  double total=0;
  for(int i=0;i<part1.size();i++) {
    int found = find_partition(part1[i],part2);
    if (found == -1)
      total += std::abs(d1[i]);
    else {
      total += std::abs(d1[i] - d2[found]);
    }
  }

  // Accumulate distances for T2 partitions
  for(int i=0;i<part2.size();i++) {
    int found = find_partition(part2[i],part1);
    if (found == -1)
      total += std::abs(d2[i]);
    else
      { } // this is already counted in the previous loop
  }
  return total;
}

double internal_branch_distance(const Tree& T1, const Tree& T2) 
{
  assert(T1.n_leaves() == T2.n_leaves());

  unsigned l1 = T1.n_leafbranches();
  unsigned l2 = T2.n_leafbranches();

  unsigned n1 = T1.n_branches() - l1;
  unsigned n2 = T2.n_branches() - l2;

  vector<double> d1(n1);
  vector<double> d2(n2);

  vector< dynamic_bitset<> > part1(n1,dynamic_bitset<>(T1.n_leaves()));
  vector< dynamic_bitset<> > part2(n2,dynamic_bitset<>(T2.n_leaves()));

  // get partitions and lengths for T1
  for(int i=0;i<n1;i++) {
    d1[i] = T1.branch(i+l1).length();
    part1[i] = branch_partition(T1,i+l1);
  }

  // get partitions and lengths for T2
  for(int i=0;i<n2;i++) {
    d2[i] = T2.branch(i+l2).length();
    part2[i] = branch_partition(T2,i+l2);
  }

  // Accumulate distances for T1 partitions
  vector<bool> found(n2,false);

  double total = 0;
  for(int i=0;i<part1.size();i++) {
    int j = find_partition(part1[i],part2);
    if (j == -1)
      total += d1[i];
    else {
      found[j] = true;
      total += abs(d1[i]-d2[j]);
    }
  }

  for(int i=0;i<part2.size();i++) {
    if (not found[i])
      total += d2[i];
  }

  return total;
}

unsigned topology_distance(const Tree& T1, const Tree& T2) 
{
  assert(T1.n_leaves() == T2.n_leaves());

  unsigned l1 = T1.n_leafbranches();
  unsigned l2 = T2.n_leafbranches();

  unsigned n1 = T1.n_branches() - l1;
  unsigned n2 = T2.n_branches() - l2;

  vector< dynamic_bitset<> > part1(n1,dynamic_bitset<>(T1.n_leaves()));
  vector< dynamic_bitset<> > part2(n2,dynamic_bitset<>(T2.n_leaves()));

  // get partitions and lengths for T1
  for(int i=0;i<n1;i++)
    part1[i] = branch_partition(T1,i+l1);

  // get partitions and lengths for T2
  for(int i=0;i<n2;i++)
    part2[i] = branch_partition(T2,i+l2);

  // Accumulate distances for T1 partitions
  unsigned shared=0;
  for(int i=0;i<part1.size();i++) {
    if (find_partition(part1[i],part2) != -1)
      shared++;
  }

  return (n1-shared) + (n2-shared);
}

double robinson_foulds_distance(const Tree& T1, const Tree& T2) 
{
  return 0.5*topology_distance(T1,T2);
}

template <class T>
struct array_order
{
  const vector<T>& array;
  bool operator()(int i,int j) const {return std::less<T>()(array[i],array[j]);}

  array_order(const vector<T>& n):array(n) {}
};

vector<int> compute_sorted_mapping(const vector<string>& names)
{
  vector<int> mapping(names.size());
  for(int i=0;i<names.size();i++)
    mapping[i] = i;

  std::sort(mapping.begin(),mapping.end(),array_order<string>(names));

  return invert(mapping);
}


//FIXME - return mapping of leaf nodes?  Of all nodes?
void standardize(SequenceTree& T) 
{
  vector<int> mapping = compute_sorted_mapping(T.get_sequences());

  T.standardize(mapping);
}

void standardize(RootedSequenceTree& T) {

  vector<int> mapping = compute_sorted_mapping(T.get_sequences());

  T.standardize(mapping);

  T.reroot(T.directed_branch(0).target());
}

RootedSequenceTree standardized(const string& t) 
{
  RootedSequenceTree T;
  T.parse(t);

  if (T.root().degree() == 2)
    T.remove_node_from_branch(T.root());

  if (has_sub_branches(T))
    throw myexception()<<"Tree has node of degree 2";

  standardize(T);
  return T;
}

