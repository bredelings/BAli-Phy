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
#include "tree/sequencetree.H"
#include "myexception.H"
#include "util.H"
#include "io.H"

using namespace std;

using boost::dynamic_bitset;

//-------------------------- SequenceTree methods ----------------------------//

void SequenceTree::set_label(int i, const string& s)
{
  assert(node_label_index != -1);
  (*nodes_[i]->node_attributes)[node_label_index] = s;
}

const string& SequenceTree::get_label(int i) const 
{
  // There should be no empty labels!  Only "" is allowed.
  const boost::any& label = (*nodes_[i]->node_attributes)[node_label_index];
  const string* label_p = boost::any_cast<const string>(&label);
  assert(label_p);
  return *label_p;
}

vector<string> SequenceTree::get_labels() const
{
  // There should be no empty labels!  Only "" is allowed.
  vector<string> node_labels(n_nodes());

  for(int i=0;i<node_labels.size();i++)
    node_labels[i] = get_label(i);

  return node_labels;
}

vector<string> SequenceTree::get_leaf_labels() const
{
  vector<string> leaf_labels(n_leaves());

  for(int i=0;i<leaf_labels.size();i++)
    leaf_labels[i] = get_label(i);

  return leaf_labels;
}

int SequenceTree::index(const string& l) const 
{
  for(int i=0;i<n_nodes();i++)
    if (get_label(i) == l) return i;
  return -1;
}

nodeview SequenceTree::add_leaf_node(int n)
{
  nodeview nv = Tree::add_leaf_node(n);
  set_label(nv, "");
  return nv;
}

nodeview SequenceTree::create_node_on_branch(int b)
{
  nodeview nv = Tree::create_node_on_branch(b);
  set_label(nv, "");
  return nv;
}

void SequenceTree::read(const string& filename) {
  checked_ifstream file(filename, "NEWICK tree file");
  read(file);
}

void SequenceTree::read(std::istream& file) {
  assert(file);

  string total;
  string line;
  while(portable_getline(file,line))
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

// count depth -> if we are at depth 0, and have
// one object on the stack then we quit
int SequenceTree::parse(const string& line) 
{
  int root = Tree::parse_and_discover_names(line);

  for(int i=0;i<n_nodes();i++)
    if (node(i).attribute(node_label_index).empty())
      set_label(i,"");

  return root;
}

int SequenceTree::parse_and_discover_names(const std::string& s)
{
  int root = Tree::parse_and_discover_names(s);

  for(int i=0;i<n_nodes();i++)
    if (node(i).attribute(node_label_index).empty())
      set_label(i,"");

  return root;
}

int SequenceTree::parse_with_names_or_numbers(const std::string& s, const std::vector<std::string>& names, bool allow_numbers)
{
  int root = Tree::parse_with_names_or_numbers(s,names, allow_numbers);

  for(int i=0;i<names.size();i++)
    set_label(i, names[i]);

  for(int i=0;i<n_nodes();i++)
    if (node(i).attribute(node_label_index).empty())
      set_label(i,"");

  return root;
}

int SequenceTree::parse_nexus(const string& s,const vector<string>& names) 
{
  int root = parse_with_names_or_numbers(s, names);

  for(int i=0;i<names.size();i++)
    set_label(i, names[i]);

  for(int i=names.size();i<n_nodes();i++)
    set_label(i, "");

  return root;
}

SequenceTree::SequenceTree(const std::string& s) 
{
  add_first_node();
  set_label(0, s);
}

SequenceTree::SequenceTree(const Tree& T,const vector<string>& names)
  :Tree(T)
{
  if (names.size() != n_nodes() and names.size() != n_leaves())
    throw myexception()<<"Can't label tree of "<<n_nodes()<<" nodes with "<<names.size()<<" labels!\n";

  for(int i=0;i<names.size();i++)
    set_label(i, names[i]);
  for(int i=names.size();i<n_nodes();i++)
    set_label(i, "");
}

//-------------------------- SequenceTree methods ----------------------------//
vector<int> RootedSequenceTree::prune_leaves(const vector<int>& remove) 
{
  root_ = NULL;

  // if we need to do this, virtualize unlink_subtree to complain if the subtree
  // contains the root.

  return Tree::prune_leaves(remove);
}

string write_with_bootstrap_fraction(const vector<string>& names, const_branchview b, 
				     const vector<double>& bf, bool print_lengths)
{
  string output;

  // Print the subtrees
  if (b.target().is_internal_node())
  {
    vector<const_branchview> branches = sorted_branches_after(b);
    output = "(";
    for(int i=0;i<branches.size();i++) {
      output += write_with_bootstrap_fraction(names,branches[i],bf,print_lengths);

      if (i+1<branches.size())
	output += ",";
    }
    output += ")";
  }

  // Print the name
  output += names[b.target()];

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
  string output;

  // Print the subtrees
  vector<const_branchview> branches = sorted_neighbors(T.root());
  output = "(";
  for(int i=0;i<branches.size();i++) {
    output += write_with_bootstrap_fraction(names,branches[i],bf,print_lengths);

    if (i+1 < branches.size())
      output += ',';
  }
  output += ")";

  // Print the name
  output += names[T.root()];

  // Print the terminator
  output += ";";

  return output;
}

string RootedSequenceTree::write(bool print_lengths) const 
{
  return ::write(*this, get_labels(), print_lengths);
}

string RootedSequenceTree::write_with_bootstrap_fraction(const vector<double>& bf, bool print_lengths) const 
{
  return ::write_with_bootstrap_fraction(*this, get_labels(), bf, print_lengths);
}

int RootedSequenceTree::parse_and_discover_names(const std::string& s)
{
  int r = SequenceTree::parse_and_discover_names(s);

  root_ = nodes_[r];

  return r;
}

int RootedSequenceTree::parse_with_names_or_numbers(const std::string& s, const std::vector<std::string>& names, bool allow_numbers)
{
  int r = SequenceTree::parse_with_names_or_numbers(s,names,allow_numbers);

  root_ = nodes_[r];

  return r;
}

int RootedSequenceTree::parse(const string& s) 
{
  int r = SequenceTree::parse(s);

  root_ = nodes_[r];

  return r;
}

int RootedSequenceTree::parse_nexus(const string& s, const vector<string>& names) 
{
  // Parse the tree with the specified number of names
  int r = parse_with_names_or_numbers(s, names);

  // Complain if the number of names is odd.
  if (names.size() != n_leaves())
    throw myexception()<<"Can't parsed tree of "<<n_leaves()<<" leaves using "<<names.size()<<" labels!\n";

  // Construct the leaf labels
  for(int i=0;i<n_nodes();i++) 
    set_label(i, "");

  for(int i=0;i<names.size();i++)
    set_label(i, names[i]);

  return r;
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
  set_label(0, s);
}

RootedSequenceTree::RootedSequenceTree(istream& file) {
  read(file);
}

RootedSequenceTree::RootedSequenceTree(const RootedSequenceTree& T1, const RootedSequenceTree& T2)
  :RootedTree(T1,T2)
{
  // This preserves only leaf labels.

  // We will create new names which will be the same as
  //  T1.order + T2.order
  int l=0;
  for(int i=0;i<n_nodes();i++) 
    set_label(i, "");

  for(int i=0;i<T1.n_leaves();i++) 
    set_label(l++, T1.get_label(i));

  for(int i=0;i<T2.n_leaves();i++) 
    set_label(l++, T2.get_label(i));

  // Hmmm.. n_leaves() could be odd if there is a root leaf...
}

RootedSequenceTree add_root(SequenceTree T,int b) {
  int r = T.create_node_on_branch(b);
  return RootedSequenceTree(T,r);
}
  
RootedSequenceTree operator+(const RootedSequenceTree& t1,const RootedSequenceTree& t2) 
{
  RootedSequenceTree t3(t1,t2);
  int new_root = t3.add_leaf_node(t3.root());
  t3.reroot(new_root);

  return t3;
}

std::istream& operator >>(std::istream& i,SequenceTree& T) 
{
  string line;
  while(portable_getline(i,line)) {
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
  SequenceTree T(star_tree(names.size()), names);
  assert(T.n_leaves() == names.size());
  return T;
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

  set< dynamic_bitset<> > part1;
  set< dynamic_bitset<> > part2;

  // get partitions and lengths for T1
  for(int i=0;i<n1;i++)
    part1.insert(branch_partition(T1,i+l1));

  // get partitions and lengths for T2
  for(int i=0;i<n2;i++)
    part2.insert(branch_partition(T2,i+l2));

  // Accumulate distances for T1 partitions
  unsigned shared=0;
  for(const auto& part: part1)
    if (part2.count(part) or part2.count(~part))
      shared++;

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

RootedSequenceTree& RootedSequenceTree::operator=(const RootedSequenceTree& T)
{
  RootedTree::operator=(T);

  return *this;
}

//FIXME - return mapping of leaf nodes?  Of all nodes?
void standardize(SequenceTree& T) 
{
  vector<int> mapping = compute_sorted_mapping(T.get_leaf_labels());

  T.standardize(mapping);
}

void standardize(RootedSequenceTree& T) {

  vector<int> mapping = compute_sorted_mapping(T.get_leaf_labels());

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

