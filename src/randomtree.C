/*
   Copyright (C) 2004-2006 Benjamin Redelings

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

#include "sequencetree.H"
#include "rng.H"
#include "util-random.H"

using std::vector;
using std::string;

vector<int> permutation(int n) {
  vector<int> p;
  if (n==1)
    p.push_back(0);
  else {
    p = permutation(n-1);
    int i = myrandom(n);
    p.insert(p.begin()+i,n-1);
  }
  return p;
}

BranchNode* random_sub_node(BranchNode* n)
{
  unsigned d = nodeview(n).degree();
  unsigned temp = unsigned( uniform()*d );

  for(unsigned i=0;i<temp;i++)
    n = n->next;

  return n;
}

BranchNode* random_sub_node_after(BranchNode* n)
{
  unsigned d = nodeview(n).degree()-1;
  unsigned temp = unsigned( uniform()*d );

  BranchNode* n2 = n->next;
  for(unsigned i=0;i<temp;i++)
    n2 = n2->next;

  assert(n2 != n);

  return n2;
}

// Take two random parts of the node, and make a new node pointing to them.
BranchNode* randomly_split_node(Tree& T, BranchNode* n1) 
{
  int D0 = degree(n1);
  int n1_index = n1->node_attributes->name;
  assert(D0 > 3);

  // get a new node n2 
  BranchNode* n2 = T.add_leaf_node(n1->node_attributes->name);
  int n2_index = n2->node_attributes->name;

  // find the sub-node n1 in T[n1_index] that points to T[n2_index]
  n1 = n2->out;
  assert(n1->node_attributes->name == n1_index);

  // move the first random branch after b to pull out the first random node (subtree)
  for(int i=0;i<2;i++)
  {
    BranchNode* bn = random_sub_node_after( n1 );
    int n3_index = bn->out->node_attributes->name;
    assert(n3_index != n1_index);
    assert(n3_index != n2_index);
    T.reconnect_branch(n3_index, n1_index, n2_index);
  }

  assert(T[n1_index].degree() < D0);
  assert(T[n1_index].degree() >= 3);
  assert(n1->node_attributes->name == n1_index);

  assert(T[n2_index].degree() < D0);
  assert(T[n2_index].degree() >= 3);
  assert(n2->node_attributes->name == n2_index);

  return n1;
}

void RandomTree(Tree& T) 
{
  for(BN_iterator BN(T[0]);BN;BN++) 
  {
    while(nodeview(*BN).degree() > 3) {
      BranchNode * start = *BN;
      start = randomly_split_node(T,start);
      // start our search over at this new starting point.
      BN = BN_iterator(start);
    }
  }
}


void RandomTree(Tree& T, double branch_mean) 
{
  RandomTree(T);

  for(int i=0;i<T.n_branches();i++) 
    T.branch(i).set_length( exponential(branch_mean) );
}


Tree RandomTree(int n,double branch_mean) 
{
  Tree T = star_tree(n);
  RandomTree(T,branch_mean);
  return T;
}

SequenceTree RandomTree(const vector<string>& s,double branch_mean) 
{
  SequenceTree T = star_tree(s);
  RandomTree(T,branch_mean);
  return T;
}

vector<const_branchview> randomized_branches_after(const const_branchview& b)
{
  vector<const_branchview> branches;
  append(b.branches_after(), branches);
  sort(branches.begin(), branches.end());
  return randomize(branches);
}

vector<const_branchview> randomized_branches_out(const const_nodeview& n)
{
  vector<const_branchview> branches;
  append(n.branches_out(), branches);
  sort(branches.begin(), branches.end());
  return randomize(branches);
}
