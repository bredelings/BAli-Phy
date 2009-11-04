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

Tree remove_root_branch(RootedTree RT) 
{
  nodeview r1 = RT.root();
  assert(r1.degree() == 1);
  nodeview r2 = *(RT.root().neighbors());
  RT.reroot(0);

  r2 = RT.prune_subtree(RT.directed_branch(r2,r1));
  if (not r2.is_leaf_node())
    RT.remove_node_from_branch(r2);

  return Tree(RT);
}

BranchNode* random_sub_node(BranchNode* n)
{
  unsigned d = nodeview(n).degree();
  unsigned temp = unsigned( uniform()*d );

  for(unsigned i=0;i<temp;i++)
    n = n->next;

  return n;
}

BranchNode* randomly_split_node(BranchNode* n) 
{
  assert(nodeview(n).degree() > 3);

  // pull out the first random node (subtree)
  BranchNode* n1 = random_sub_node(n);
  n = TreeView::unlink_subtree(n1);

  // pull out the second random node (subtree)
  BranchNode* n2 = random_sub_node(n);
  n = TreeView::unlink_subtree(n2);
  
  // make a node containing both subtrees
  insert_after(n2,n1);

  // connect the two nodes
  connect_nodes(n,n1);

  return n;
}

void RandomTree(Tree& T) 
{
  for(BN_iterator BN(T[0]);BN;BN++) 
  {
    while(nodeview(*BN).degree() > 3) {
      BranchNode * start = *BN;
      start = randomly_split_node(start);
      BN = BN_iterator(start);
    }
  }
  T.reanalyze(T[0]);
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
