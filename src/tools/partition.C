/*
   Copyright (C) 2009-2010 Benjamin Redelings

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

///
/// \file   partition.C
/// \brief  Provides routines to for handling splits: bi-partitions on trees.
///
/// In addition to implementing the class Partition, this file
/// provides numerous other routines for handling partitions.
///
/// \author Benjamin Redelings
/// 


#include "partition.H"

#include <fstream>

#include "util.H"
#include "io.H"

using std::string;
using std::vector;
using std::endl;

using boost::dynamic_bitset;

dynamic_bitset<> group_from_names(const vector<string>& names,const vector<string>& subset)
{
  assert(subset.size() <= names.size());

  dynamic_bitset<> group(names.size());

  for(int i=0; i<subset.size(); i++) {
    if (includes(names,subset[i]))
      group[find_index(names,subset[i])] = true;
    else
      throw myexception()<<"Can't find taxon '"<<subset[i]<<"' in taxa set.";
  }

  return group;
}

Partition partition_from_branch(const SequenceTree& T,int b) 
{
  dynamic_bitset<> group(T.n_leaves());
  const dynamic_bitset<>& with_internal = T.partition(b);

  for(int i=0;i<group.size();i++)
    group[i] = with_internal[i];

  return Partition(T.get_leaf_labels(), group);
}


Partition full_partition_from_names(const vector<string>& names, const vector<string>& names1) 
{
  dynamic_bitset<> group1 = group_from_names(names,names1);

  return Partition(names,group1);
}


Partition partition_from_names(const vector<string>& names, const vector<string>& names1,
			       const vector<string>& names2)
{
  dynamic_bitset<> group1 = group_from_names(names,names1);
  dynamic_bitset<> group2 = group_from_names(names,names2);

  return Partition(names, group1, group1 | group2);
}

vector<Partition> internal_partitions_from_tree(const SequenceTree& T) 
{
  vector<Partition> partitions;

  for(int b=T.n_leafbranches();b<T.n_branches();b++)
    partitions.push_back(partition_from_branch(T,b));

  return partitions;
}


vector<Partition> all_partitions_from_tree(const SequenceTree& T) 
{
  vector<Partition> partitions;

  for(int b=0;b<T.n_branches();b++)
    partitions.push_back(partition_from_branch(T,b));

  return partitions;
}


bool Partition::full() const {
  for(int i=0;i<names.size();i++)
    if (not group1[i] and not group2[i])
      return false;
  return true;
}

Partition& Partition::flip() {
  std::swap(group1,group2);
  return *this;
}

Partition Partition::reverse() const {
  Partition p2 = *this;
  p2.flip();
  return p2;
}

Partition::Partition(const Partition& p,const dynamic_bitset<>& mask)
  :names(p.names),
   group1(p.group1 & mask),
   group2(p.group2 & mask)
{
  assert(mask.size() == p.group1.size());
  assert(not group1.intersects(group2));
}

Partition::Partition(const dynamic_bitset<>& g) 
  :group1(~g),group2(g)
{ 
  assert(not group1.intersects(group2));
}

Partition::Partition(const dynamic_bitset<>& g,const dynamic_bitset<>& mask) 
  :group1((~g) & mask),group2(g & mask)
{
  assert(g.size() == mask.size());
  assert(not group1.intersects(group2));
}

Partition::Partition(const vector<string>& n,const dynamic_bitset<>& g) 
  :names(n),group1(~g),group2(g)
{
  assert(n.size() == g.size());
  assert(not group1.intersects(group2));
}

Partition::Partition(const vector<string>& n,const dynamic_bitset<>& g,const dynamic_bitset<>& mask) 
  :names(n),group1((~ g) & mask),group2(g & mask)
{
  assert(n.size() == g.size());
  assert(g.size() == mask.size());
  assert(not group1.intersects(group2));
}

vector< vector<string> > parse_partition(const string& line)
{
  vector<string> all_names = split(line,' ');

  vector< vector<string> > names(3);

  int group = 0;
  for(int i=0; i< all_names.size(); i++) 
  {
    if (all_names[i] == "|") {
      assert(group == 0);
      group++; 
    }
    else if (all_names[i] == "[") {
      assert(group == 1);
      group++;
    }
    else if (all_names[i] == "]") {
      assert(group == 2);
      group++;
    }
    else if (all_names[i].size())
      names[group].push_back(all_names[i]);
  }

  return names;
}


Partition::Partition(const string& line) 
{
  vector<string> all_names = split(line,' ');

  names.clear();
  vector< vector<string> > name_groups = parse_partition(line);
  for(int i=0;i<name_groups.size();i++)
    names.insert(names.end(), name_groups[i].begin(), name_groups[i].end());

  std::sort(names.begin(),names.end());
  group1.resize(names.size());
  group2.resize(names.size());
  
  group1 = group_from_names(names,name_groups[0]);
  group2 = group_from_names(names,name_groups[1]);
  assert(not group1.intersects(group2));
}

Partition::Partition(const vector<string>& n,const string& line) 
  :names(n),group1(n.size()),group2(n.size())
{
  vector< vector<string> > name_groups = parse_partition(line);

  group1 = group_from_names(names,name_groups[0]);
  group2 = group_from_names(names,name_groups[1]);

  assert(not group1.intersects(group2));
}

/// \brief Check if the split is informative
///
/// \param p The split
bool informative(const Partition& p) {
  return (p.group1.count() >= 2) and (p.group2.count() >= 2);
}

/// \brief Check if the split is informative
///
/// \param p The split
bool informative(const boost::dynamic_bitset<>& p) {
  int N = p.size();
  int C = p.count();
  return (C >= 2) and ((N-C) >= 2);
}

bool valid(const Partition& p) {
  return p.group1.any() and p.group2.any();
}

std::ostream& operator<<(std::ostream& o, const Partition& P) 
{
  assert(not P.group1.intersects(P.group2));

  for(int i=0;i<P.size();i++)
    if (P.group1[i]) o<<P.names[i]<<" ";
  
  o<<"| ";
  
  for(int i=0;i<P.size();i++)
    if (P.group2[i]) o<<P.names[i]<<" ";

  dynamic_bitset<> rmask = ~(P.group1 | P.group2);
  if (rmask.any()) {
    o<<" [ ";
    for(int i=0;i<P.size();i++) {
      if (rmask[i]) o<<P.names[i]<<" ";
    }
    o<<"]";
  }
  return o;
}

bool operator==(const Partition& p1, const Partition& p2) {
  return 
    (p1.names == p2.names) and 
    (
     ((p1.group1 == p2.group1) and (p1.group2 == p2.group2)) or
     ((p1.group1 == p2.group2) and (p1.group2 == p2.group1))
    );
}

bool consistent(const Partition& p1, const Partition& p2) {
  if (not p1.group1.intersects(p2.group1)) return true;
  if (not p1.group1.intersects(p2.group2)) return true;

  if (not p1.group2.intersects(p2.group1)) return true;
  if (not p1.group2.intersects(p2.group2)) return true;

  return false;
}


/// Does the grouping of all nodes bm, imply *this?
bool implies(const Partition& p1, const Partition& p2) 
{
  if (p2.group1.is_subset_of(p1.group1) and p2.group2.is_subset_of(p1.group2)) return true;

  if (p2.group2.is_subset_of(p1.group1) and p2.group1.is_subset_of(p1.group2)) return true;

  return false;
}

/// Does the grouping of all nodes bm, imply *this?
bool directed_implies(const Partition& p1, const Partition& p2) 
{
  if (p2.group1.is_subset_of(p1.group1) and p2.group2.is_subset_of(p1.group2)) return true;

  return false;
}

/// Does any branch in T imply the partition p?
bool implies(const SequenceTree& T,const Partition& p) {
  bool result = false;
  for(int b=0;b<T.n_branches() and not result;b++) {
    dynamic_bitset<> bp = branch_partition(T,b);

    if (implies(bp,p)) return true;
  }
  return false;
}

// Remove partitions we imply.  Add only if not implied. Return true if we expanded the coverage.
vector<Partition> unimplied_partitions(const vector<Partition>& partitions,const vector<Partition>& delta) 
{
  vector<Partition> unimplied;

  for(int i=0;i<delta.size();i++)
    if (not implies(partitions,delta[i]))
      unimplied.push_back(delta[i]);

  return unimplied;
}

// Remove partitions we imply.  Add only if not implied. Return true if we expanded the coverage.
bool merge_partition(vector<Partition>& partitions,const Partition& delta) 
{
  if (implies(partitions,delta)) 
    return false;

  for(int i=partitions.size()-1;i>=0;i--)
    if (implies(delta,partitions[i]))
      partitions.erase(partitions.begin()+i);

  partitions.push_back(delta);

  return true;
}

// Remove partitions we imply.  Add only if not implied. Return true if we expanded the coverage.
bool merge_partitions(vector<Partition>& partitions,const vector<Partition>& delta) 
{
  bool changed=false;

  for(int i=0;i<delta.size();i++) 
    if (merge_partition(partitions,delta[i]))
      changed = true;

  return changed;
}

int which_branch(const SequenceTree& T, const Partition& p) 
{
  for(int b=0; b<2*T.n_branches(); b++) {
    dynamic_bitset<> bp = branch_partition(T,b);
    if( directed_implies(bp,p) )
      return b;
  }
  return -1;
}

bool p_equal(const vector<Partition>& P1,const vector<Partition>& P2) 
{
  if (P1.size() != P2.size()) return false;

  for(int i=0;i<P1.size();i++)
    if (not includes(P2,P1[i]))
      return false;

  return true;
}

bool p_contains(const vector<vector<Partition> >& partitions, const vector<Partition>& P)
{
  for(int i=0;i<partitions.size();i++)
    if (p_equal(P,partitions[i]))
      return true;

  return false;
}

/// \brief Load a list of partition lists from file 'filename'
///
/// \param filename    The file from which to load the partition lists
/// \param partitions  The list of partition lists
///
/// Load a list of partition lists from file 'filename'.  Blank
/// lines separate the collections of partitions. A collection 
/// can begin with a Newick tree, which is then decomposed into all
/// of its partitions (both informative and uninformative).
///
void load_partitions(const string& filename, vector<vector<Partition> >& partitions) 
{
  checked_ifstream file(filename, "splits file");

  string line;
  while(file) {
    vector<Partition> P;

    while(getline(file,line) and line.size()) {
      if (line[0] == '(') {
	SequenceTree T = standardized(line);
	vector<Partition> TP = all_partitions_from_tree(T);
	P.insert(P.end(),TP.begin(),TP.end());
      }
      else
	P.push_back(Partition(line));
    }

    if (P.size() and not p_contains(partitions,P))
      partitions.push_back(P);
  }
}

void write_partitions(std::ostream& o,const vector<Partition>& partitions)
{
  vector<Partition> full;
  vector<Partition> sub;
  for(int i=0;i<partitions.size();i++)
    if (partitions[i].full())
      full.push_back(partitions[i]);
    else
      sub.push_back(partitions[i]);

  if (full.size()) {
    SequenceTree consensus = get_mf_tree(partitions[0].names,full);
    o<<consensus.write(false)<<endl;
  }

  for(int i=0;i<sub.size();i++)
    o<<sub[i]<<endl;
}


SequenceTree get_mf_tree(const std::vector<std::string>& names,
			 const std::vector<Partition>& partitions) 
{
  SequenceTree T = star_tree(names);

  int i=0;
  try {
    for(;i<partitions.size();i++)
      T.induce_partition(partitions[i].group1);
  }
  catch(...) {
    throw myexception()<<"Partition ("<<partitions[i]<<") conflicts with tree "<<T;
  }

  for(int i=0;i<T.n_branches();i++)
    T.branch(i).set_length(1.0);

  return T;
}

SequenceTree get_mf_tree(const std::vector<std::string>& names,
			 const std::vector<dynamic_bitset<> >& partitions) 
{
  SequenceTree T = star_tree(names);

  for(int i=0;i<partitions.size();i++)
    T.induce_partition(partitions[i]);

  for(int i=0;i<T.n_branches();i++)
    T.branch(i).set_length(1.0);

  return T;
}

SequenceTree get_mf_tree(const std::vector<std::string>& names,
			 const std::vector<dynamic_bitset<> >& partitions,
			 const std::vector<double>& branch_lengths) 
{
  SequenceTree T = star_tree(names);
  for(int b=0;b<T.n_branches();b++)
    T.branch(b).set_length(branch_lengths[b]);

  const int LB = T.n_branches();

  assert(branch_lengths.size() == LB + partitions.size());

  for(int i=0;i<partitions.size();i++)
  {
    int b = T.induce_partition(partitions[i]);
    T.branch(b).set_length(branch_lengths[LB+i]);
  }

  return T;
}

/// Remove uninformative branches, then add leaf branches in front
void add_leaf_partitions(const vector<string>& names, vector<Partition>& partitions)
{
  // remove uninformative branches
  for(int i=0;i<partitions.size();)
    if (not informative(partitions[i]))
      partitions.erase(partitions.begin()+i);
    else
      i++;

  // adds leaf branches
  for(int i=0;i<names.size();i++) {
    dynamic_bitset<> m(names.size()); m.flip();
    m[i] = false;
    partitions.insert(partitions.begin()+i,Partition(names,m));
  }
}

vector<Partition> get_star_partitions(const vector<string>& names)
{
  vector<Partition> partitions;
  add_leaf_partitions(names,partitions);
  return partitions;
}
