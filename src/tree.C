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

#include "tree.H"
#include "util.H"
#include <algorithm>
#include <sstream>
#include "myexception.H"

using std::vector;
using std::string;
using boost::dynamic_bitset;

void TreeView::destroy_tree(BranchNode* start) {
  assert(start);

  vector<BranchNode*> nodes;
  nodes.reserve(20);

  for(BN_iterator BN(start);BN;BN++) 
    nodes.push_back(*BN);

  for(int i=0;i<nodes.size();i++)
    delete nodes[i];
}

BranchNode* TreeView::copy_node(const BranchNode* start) {

  const BranchNode* n1 = start;

  BranchNode* start2 = new BranchNode(n1->branch,n1->node,n1->length);
  BranchNode* n2 = start2;

  if (start->out == start)
    start2->out = start2;

  do {

    n1 = n1->next;

    if (n1 == start)
      n2->next = start2;
    else
      n2->next = new BranchNode(n1->branch,n1->node,n1->length);

    n2->next->prev = n2;

    n2 = n2->next;
  } while(n1 != start);

  return n2;
}


BranchNode* TreeView::copy_tree(const BranchNode* start) {

  const BranchNode* here1 = start;
  BranchNode* start2 = copy_node(start);
  BranchNode* here2 = start2;
  
  do {
    
    // If we jump out to another node, then create it if its not there
    if (!here2->out) {
      here2->out = copy_node(here1->out);
      here2->out->out = here2;
    }

    here1 = here1->out->next;
    here2 = here2->out->next;
    
  } while(here1 != start);

  assert(here1 == start);
  assert(here2 == start2);

  return start2;
}

TreeView TreeView::copy() const {
  if (not root)
    return TreeView(0);

  return copy_tree(root);
}

void TreeView::exchange_subtrees(BranchNode* n1, BranchNode* n2) {
  // I should assert that the subtrees are disjoint, somehow...

  // Neither branches comes from a leaf node
  assert(not is_leaf_node(n1));
  assert(not is_leaf_node(n2));

  // The nodes are distinct
  assert(n1 != n2);

  // Switch the nodes that we point to
  std::swap(n1->prev,n2->prev);
  std::swap(n1->next,n2->next);

  // Switch the nodes that point to us
  n1->prev->next = n1;
  n1->next->prev = n1;

  n2->prev->next = n2;
  n2->next->prev = n2;

  // Switch the node name that we are part of
  std::swap(n1->node,n2->node);
}

void TreeView::merge_nodes(BranchNode* n1,BranchNode* n2) {
  std::swap(n1->next,n2->next);
  n1->next->prev = n1;
  n2->next->prev = n2;
}

// this preserves sub-branch directions
// the node with the smaller name maintains the name of its attatched branch

//         b1 <-----> b2
//            goes to...


BranchNode* TreeView::create_node_on_branch(BranchNode* b1, int  new_branchname) {

  BranchNode* b2 = b1->out;

  // Create a ring of size 2 - duplicate branch names and lengths
  BranchNode* n1 = new BranchNode(b2->branch,-1,b2->length);
  BranchNode* n2 = new BranchNode(b1->branch,-1,b1->length);
  n1->next = n1->prev = n2;
  n2->next = n2->prev = n1;

  // Link from ring to branch endpoints
  n1->out = b1;
  b1->out = n1;

  n2->out = b2;
  b2->out = n2;

  int delta = std::abs(b2->branch - b1->branch);

  // choose sub-branch to give the new name to
  if (b1->node > b2->node)
    std::swap(b1,b2);

  // determine sub-branch direction;
  if (b2->branch > b2->out->branch)
    b2 = b2->out;

  // set new branch name and set length to 0
  b2->branch = new_branchname;
  b2->out->branch = new_branchname + delta;
  b2->length = b2->out->length = 0;

  return n1;
}


// b1 <-----> [n1,n2] <-------> b2
//            goes to...
//         b1 <-----> b2
// The larger NODE name gets to keep its... BRANCH direction?
//
// The direction of the remaining branch stays unchanged. (b2->branch = n1->branch)
//

/// Merge sub-branches, adding their lengths, and reporting which branch name didn't survive.
/// If branch_to_move is not -1, then we force this name to be the one that did not survive.
int TreeView::remove_node_from_branch(BranchNode* n1, int branch_to_move) 
{
  BranchNode* n2 = n1->next;
  assert(n2->next == n1);

  BranchNode* b1 = n1->out;
  BranchNode* b2 = n2->out;

  int b1_name = std::min(b1->branch, b1->out->branch);
  int b2_name = std::min(b2->branch, b2->out->branch);

  // Preserve the name of the branch with the smaller name (to avoid renaming leaf branches!)
  // (The name of the b1<--->n1 branch gets preserved)

  bool swap_branches = false;
  if (branch_to_move != -1)
  {
    if (b1->branch == branch_to_move or b1->out->branch == branch_to_move)
      swap_branches = true;
  }
  else if (b1_name > b2_name)
    swap_branches = true;

  if (swap_branches)
  {
    std::swap(n1,n2);
    std::swap(b1,b2);
    std::swap(b1_name, b2_name);
  }

  if (branch_to_move != -1)
      assert(b2->branch == branch_to_move or b2->out->branch == branch_to_move);

  //---------- get delta - and check it ------------//
#ifndef NDEBUG
  int delta1 = std::abs(b1->branch - b1->out->branch);
  int delta2 = std::abs(b2->branch - b2->out->branch);
  assert(delta1 == delta2);
#endif

  //-- Connect branches, merge lengths, use new name --//
  b1->out = b2;
  b2->out = b1;

  b1->length += b2->length;
  b2->length = b1->length;

  b2->branch = n1->branch; // preserve the direction of the remaining branch.

  //-------- Remove the node, and reconnect --------//
  delete n1;
  delete n2;

  //-------- report which branch name didn't survive -------//
  assert(b2_name > b1_name);
  assert(b2_name >= ((delta1+3)/2));
  return b2_name;
}

BranchNode* TreeView::unlink_subtree(BranchNode* b) 
{
  if (not is_leaf_node(b)) {
    BranchNode* prev = b->prev;

    // disconnect from tree
    b->prev->next = b->next;
    b->next->prev = b->prev;

    // re-link this node as a leaf node
    b->prev = b->next = b;

    return prev;
  }
  else {
    BranchNode* copy = new BranchNode;
    copy->prev = copy->next = copy->out = copy;
    copy->node = b->node;
    copy->branch = b->branch;

    return copy;
  }
}

void knit_node_together(const vector<BranchNode*>& nodes) {
  nodes[0]->prev = nodes.back();
  nodes.back()->next = nodes[0];

  for(int i=0;i<nodes.size()-1;i++) {
    nodes[i]->next = nodes[i+1];
    nodes[i+1]->prev = nodes[i];
  }
  
}

vector<int> directed_names(const vector<branchview>& v)
{
  vector<int> names(v.size());
  for(int i=0;i<names.size();i++)
    names[i] = v[i].name();
  return names;
}

vector<int> directed_names(const vector<const_branchview>& v)
{
  vector<int> names(v.size());
  for(int i=0;i<names.size();i++)
    names[i] = v[i].name();
  return names;
}


string write(const vector<string>& names, const_branchview b, bool print_lengths)
{
  string output;

  // If this is an internal node, then print the subtrees
  if (b.target().is_internal_node()) 
  {
    vector<const_branchview> branches = sorted_branches_after(b);
    output = "(";
    for(int i=0;i<branches.size();i++) {
      output += write(names,branches[i],print_lengths);

      if (i+1<branches.size())
	output += ",";
    }
    output += ")";
  }

  // Print the name (it might be empty)
  output += names[b.target()];

  // print the branch length if requested
  if (print_lengths)
    output += ":" + convertToString(b.length());

  return output;
}

string write(const_nodeview root, const vector<string>& names, bool print_lengths) 
{
  string output;

  // If this is an internal node, then print the subtrees
  vector<const_branchview> branches = sorted_neighbors(root);
  output = "(";
  for(int i=0;i<branches.size();i++) {
    output += write(names,branches[i],print_lengths);
    if (i+1 < branches.size())
      output += ',';
  }
  output += ")";

  // Print the name (it might be empty)
  output += names[root];

  // Print the terminator
  output += ";";

  return output;
}

string write_no_names(const_branchview b, bool print_lengths)
{
  string output;

  // If this is a leaf node, then print the name
  if (b.target().is_leaf_node())
    output += convertToString(b.target().name()+1);
  // If this is an internal node, then print the subtrees
  else {
    vector<const_branchview> branches = sorted_branches_after(b);
    output = "(";
    for(int i=0;i<branches.size();i++) {
      output += write_no_names(branches[i],print_lengths);

      if (i+1<branches.size())
	output += ",";
    }
    output += ")";
  }

  // print the branch length if requested
  if (print_lengths)
    output += ":" + convertToString(b.length());

  return output;
}

string write_no_names(const_nodeview root, bool print_lengths) 
{
  vector<const_branchview> branches = sorted_neighbors(root);

  string output = "(";
  for(int i=0;i<branches.size();i++) {
    output += write_no_names(branches[i],print_lengths);
    if (i+1 < branches.size())
      output += ',';
  }
  output += ");";
  return output;
}

//------------------------ Begin definition of Tree::* routines ------------------------//

void name_node(BranchNode* start,int i) {
  BranchNode* n = start;

  do {
    n->node = i;
    n = n->next;
  } while (n != start);
}

vector<int> Tree::standardize() {
  vector<int> lnames(n_leaves());
  for(int i=0;i<lnames.size();i++)
    lnames[i] = i;
  return standardize(lnames);
}

vector<int> Tree::standardize(const vector<int>& lnames) {

  vector<BranchNode*> old_nodes = nodes_;

  //----------- Set the leaf names ------------//
  assert(lnames.size() == n_leaves());
  for(int i=0;i<n_leaves();i++)
    nodes_[i]->node = lnames[i];

  //---------- recompute everything -----------//
  reanalyze(nodes_[0]);

  //------------- compute mapping -------------//
  vector<int> mapping(old_nodes.size());
  for(int i=0;i<mapping.size();i++)
    mapping[i] = old_nodes[i]->node;

  return mapping;
}

/// Give leaf nodes their new names
void shift_leaves(BranchNode* start,int first,int n) {
  for(BN_iterator BN(start);BN;BN++) {
    if (not is_leaf_node(*BN))
      continue;

    if ((*BN)->node >= n)
      ((*BN)->node)--;

    (*BN)->node += first;
  }
}

/// Remove the specified leaves and their dangling ancestors
vector<int> Tree::prune_leaves(const vector<int>& remove) 
{
  // get pointers to the current leaves
  vector<BranchNode*> old_nodes = nodes_;

  // get mask of leaves to remove
  vector<int> do_remove(n_leaves(),false);
  for(int i=0;i<remove.size();i++)
    do_remove[remove[i]] = true;

  // remove some leaves and shift others down
  int new_leaves=0;
  BranchNode* node_remainder = NULL;
  for(int i=0;i<n_leaves();i++)
  {
    if (do_remove[i]) 
    {
      BranchNode* leaf = old_nodes[i];
      while(is_leaf_node(leaf) and leaf->out and leaf->out != leaf) {
	BranchNode* parent = TreeView::unlink_subtree(leaf->out);
	TreeView(leaf).destroy();
	leaf = parent;
      }

      // remove nodes of degree 2
      if (leaf->next != leaf and leaf->next->next == leaf)
	TreeView::remove_node_from_branch(leaf);
    }
    else {
      name_node(old_nodes[i],new_leaves++);
      node_remainder = old_nodes[i];
    }
  }
  assert(new_leaves == n_leaves() - remove.size());
  
  // Reconstruct everything from node names
  reanalyze(node_remainder);

  assert(new_leaves == n_leaves());

  // Construct the map from new to old node names: O(N^2)
  vector<int> mapping(n_nodes(), -1);
  for(int i=0;i<mapping.size();i++)
    mapping[i] = find_index(old_nodes, nodes_[i]);

  for(int i=0;i<mapping.size();i++) {
    assert(mapping[i] != -1);
    for(int j=0;j<i;j++)
      assert(mapping[i] != mapping[j]);
  }

  return mapping;
}


/* Guarantees for names: 
     1. Surviving leaf names will be in same order in each tree.
     2. All the leaf names in tree 1 will be before all the leaf names in tree 2.
     3. The merged node will NOT be a leaf node in the new tree.
   Requirements:
     1. Both trees have at least 1 edge: 
        This ensures that the nodes retain definition.
	If either tree has only one node and no edges then that node simply goes away,
         and guarantee #2 is lost.
    
*/

void Tree::merge_tree(int node, const Tree& T, int tnode) {
  //--- Make new tree structure, w/ correct leaf node names ---//
  BranchNode* n  = nodes_[node];
  BranchNode* tn = T.copy(tnode);

  if (not n->out)
    throw myexception()<<"Trying to merge a tree into a tree w/ only one node: not allowed.";
  if (not tn->out)
    throw myexception()<<"Trying to merge a tree w/ only one node: not allowed.";

  int nl1 = n_leaves();   if (is_leaf_node(n )) nl1--;

  shift_leaves( n, 0  ,  n->node);
  shift_leaves(tn, nl1, tn->node);

  TreeView::merge_nodes(n,tn);

  //------------------------- Setup ---------------------------//
  assert(not is_leaf_node(n));
  assert(not is_leaf_node(tn));

  reanalyze(nodes_[0]);

  assert(n->node == tn->node);
}

double Tree::distance(int i,int j) const {
  double d=0;

  BranchNode* b = nodes_[i];

  while (b->node != j) {
    if (subtree_contains(b->branch,j)) {
      d += b->length;
      b = b->out;
    }
    else 
      b = b->next;
  }
  return d;
}

int Tree::edges_distance(int i,int j) const {
  int d=0;

  BranchNode* b = nodes_[i];

  while (b->node != j) {
    if (subtree_contains(b->branch,j)) {
      d++;
      b = b->out;
    }
    else 
      b = b->next;
  }
  return d;
}

BranchNode* get_first_node() {
  BranchNode* BN = new BranchNode(0,0,-1);
  BN->prev = BN->next = BN;
  BN->out = BN;

  return BN;
}

void Tree::add_first_node() {
  if (nodes_.size())
    throw myexception()<<"Trying to add first node to tree which is not empty";

  BranchNode* BN = get_first_node();

  nodes_.push_back(BN);
  branches_.push_back(BN);

  n_leaves_ = 1;
}

BranchNode* add_leaf_node(BranchNode* n) 
{
  // The spot to which to link the new node.
  BranchNode* n_link = NULL;

  // if n is a bare node, then the new node with link to n
  if (n->out == n)
    n_link = n;
  // otherwise it links to a new BranchNode inserted next to n
  else {
    n_link = new BranchNode(-1,n->node,-1);
    n_link->prev = n; 
    n_link->next = n->next;

    n_link->prev->next = n_link;
    n_link->next->prev = n_link;
  }

  // Add a new leaf node, and an edge to node 'node'
  BranchNode* n_leaf = new BranchNode;
  n_leaf->prev = n_leaf->next = n_leaf;
  n_leaf->out = n_link;
  n_link->out = n_leaf;

  return n_leaf;
}


nodeview Tree::add_leaf_node(int node) 
{
  assert(0 <= node and node < nodes_.size());
  int n_branches_old = n_branches();

  // Update the directed branch names
  for(BN_iterator BN(nodes_[0]);BN;BN++) 
  {
    int name = std::min((*BN)->branch, (*BN)->out->branch);
    if ((*BN)->branch != name)
      (*BN)->branch++;
  }

  // Add the new leaf node to the tree
  BranchNode* n_leaf = ::add_leaf_node(nodes_[node]);
  n_leaf->node = nodes_.size();
  n_leaf->branch = n_branches_old;
  n_leaf->out->branch = 2*n_branches_old+1;

  // Update the nodes_ array
  nodes_.push_back(n_leaf);

  // Update the branches_ array
  branches_.resize(branches_.size()+2);
  for(BN_iterator BN(nodes_[0]);BN;BN++) 
  {
    branches_[ (*BN)->branch ] = *BN;
    assert( nodes_[ (*BN)->node ] = *BN);
  }

  caches_valid = false;

  return n_leaf;
}


void get_branches_before(vector<const_branchview>& branch_list) {
  for(int i=0;i<branch_list.size();i++)
    append(branch_list[i].branches_before(),branch_list);
}


void get_branches_after(vector<const_branchview>& branch_list) {
  for(int i=0;i<branch_list.size();i++)
    append(branch_list[i].branches_after(),branch_list);
}

vector<const_branchview> branches_before_inclusive(const Tree& T,int b) {
  vector<const_branchview> branch_list;
  branch_list.reserve(T.n_branches());

  branch_list.push_back(T.directed_branch(b));
  get_branches_before(branch_list);

  return branch_list;
}


vector<const_branchview> branches_after_inclusive(const Tree& T,int b) {
  vector<const_branchview> branch_list;
  branch_list.reserve(T.n_branches());

  branch_list.push_back(T.directed_branch(b));
  get_branches_after(branch_list);

  return branch_list;
}

vector<const_branchview> branches_after(const Tree& T,int b) 
{
  vector<const_branchview> branch_list = branches_after_inclusive(T,b);

  branch_list.erase(branch_list.begin());

  return branch_list;
}

vector<const_branchview> branches_from_node(const Tree& T,int n) {

  vector<const_branchview> branch_list;
  branch_list.reserve(T.n_branches());

  append(T[n].branches_out(),branch_list);

  get_branches_after(branch_list);

  std::reverse(branch_list.begin(),branch_list.end());
  return branch_list;
}  

vector<const_branchview> branches_toward_node(const Tree& T,int n) {
  vector<const_branchview> branch_list;
  branch_list.reserve(T.n_branches());

  append(T[n].branches_in(),branch_list);

  for(int i=0;i<branch_list.size();i++)
    append(branch_list[i].branches_before(), branch_list);

  std::reverse(branch_list.begin(),branch_list.end());
  return branch_list;
}  

vector<const_branchview> branches_from_leaves(const Tree& T) 
{
  vector<const_branchview> branch_list;
  branch_list.reserve(2*T.n_branches());
  vector<bool> visited(2*T.n_branches(),false);

  for(int i=0;i<T.n_leaves();i++) {
    branch_list.push_back(T.branch(i));
    visited[i] = true;
  }

  for(int i=0;i<branch_list.size();i++) 
  {
    // because we are on the list, we are 'visited'
    assert(visited[branch_list[i]]);

    // check branches-after to see if any are ready
    for(const_edges_after_iterator j = branch_list[i].branches_after();j;j++) 
    {
      // if we are already valid, then ignore
      if (visited[*j]) continue;

      // check if all branches-before are valid
      bool ready = true;
      for(const_edges_before_iterator k = (*j).branches_before();k;k++)
	if (not visited[*k]) ready = false;

      // if so, then 
      if (ready) {
	branch_list.push_back(*j);
	visited[*j] = true;
      }
    }
  }
  assert(branch_list.size() == 2*T.n_branches());

  return branch_list;
}

void Tree::compute_partitions() const 
{
  vector<const_branchview> branch_list = branches_from_node(*this,nodes_[0]->node);

  // set up cached partition masks
  cached_partitions.resize(2*n_branches());
  for(int i=0;i<cached_partitions.size();i++)
    if (cached_partitions[i].size() != n_nodes())
      cached_partitions[i].resize(n_nodes());

  // compute partition masks
  for(int i=0;i<branch_list.size();i++) 
  {
    const_branchview b = branch_list[i];

    if (b.target().is_leaf_node())
      cached_partitions[b].reset();
    else {
      const_edges_after_iterator j = b.branches_after();

      cached_partitions[b] = cached_partitions[*j];j++;
      for(;j;j++)
	cached_partitions[b] |= cached_partitions[*j];
    }

    cached_partitions[b][b.target()] = true;

    cached_partitions[b.reverse()] = ~cached_partitions[b]; 
  }

  caches_valid = true;
}

void exchange_subtrees(Tree& T, int br1, int br2) 
{
  BranchNode* n0 = (BranchNode*)T[0];

  BranchNode* b1 = (BranchNode*)T.directed_branch(br1);
  BranchNode* b2 = (BranchNode*)T.directed_branch(br2);

  assert(not T.subtree_contains(br1,b2->out->node));
  assert(not T.subtree_contains(br2,b1->out->node));

  TreeView::exchange_subtrees(b1,b2);

  // don't mess with the names
  T.recompute(n0);
}

nodeview Tree::create_node_on_branch(int br) 
{
  BranchNode* b = branches_[br];

  BranchNode* n = TreeView::create_node_on_branch(b,-1);

  //FIXME - reanalyze destroys branch names, and so destroys length information...
  //  lets put branch information in the tree structure!
  //  std::abort();

  reanalyze(nodes_[0]);

  return n;
}


void remove_sub_branches(Tree& T)
{
  while(true) 
  {
    // find first node of degree 2
    int n=0;
    while(n<T.n_nodes() and T[n].degree() != 2)
      n++;

    // if no nodes of degree 2, we are done
    if (n == T.n_nodes())
      return;

    // remove the first node of degree 2
    T.remove_node_from_branch(n);
  };
}

/// Note that names are recalculated from scratch here.
void Tree::remove_node_from_branch(int node) 
{
  BranchNode* n = nodes_[node];

  if (nodeview(n).degree() == 2) {
    TreeView::remove_node_from_branch(n);

    reanalyze(nodes_[0]);
  }
}

/// SPR: move the subtree b1 into branch b2
///
/// When two branches are merged into one as the pruned subtree is removed,
/// one branch name remains in place, and one is moved.  If branch_to_move is
/// -1, then the branch with the higher undirected name is the one that moves.
/// If branch_to_move is not -1 (default) then it specifies the one to move.
///
/// The direction of the branches that do not move remains unchanged, but
/// for the attachment branch, it may be pointing either towards or away
/// from the attachment point.
///
int SPR(Tree& T, int br1,int br2, int branch_to_move) 
{
  BranchNode* b1 = (BranchNode*)T.directed_branch(br1);
  BranchNode* b2 = (BranchNode*)T.directed_branch(br2);

  assert(T.n_leaves() > 2);

  // don't regraft to the sub-branches we are being pruned from
  assert(b2 != b1->prev and b2 != b1->next);
  assert(T.partition(b1->out->branch)[b2->node]);
  assert(T.partition(b1->out->branch)[b2->out->node]);

  //------------ Prune the subtree -----------------//
  BranchNode* newbranch = TreeView::unlink_subtree(b1)->out;
  int dead_branch = TreeView::remove_node_from_branch(newbranch->out, branch_to_move);
  assert(dead_branch >= T.n_leafbranches());
  
  //----------- Regraft the subtree ---------------//
  TreeView::create_node_on_branch(b2,dead_branch);
  TreeView::merge_nodes(b1,b2->out);
  name_node(b1,b1->node);

  T.recompute(b1);

  return dead_branch;
}

/// Return a pointer to the BN in @start which points towards the root
BranchNode* get_parent(BranchNode* start) {
  BranchNode* BN = start;
  BranchNode* first = NULL;

  assert(start->node == -1);

  // find the first non-visited neighbor
  do {
    if (BN->out->node == -1) {
      if (first)
	return NULL;  // >1 unvisited neighbors - let them do it

      first = BN;
    }
    BN = BN->next;
  } while (BN != start);

  if (first)      // point to the unnamed neighbor
    return first; 
  else            // no un-visited neighbors - somebody is already doing this one.
    return NULL;
}

//NOTE: both of these routines assume that prev,next, and old pointers are correct

/// This routine assumes only that leaf nodes have proper names
void Tree::reanalyze(BranchNode* start) {

  nodes_.clear();
  branches_.clear();

  //--------------- Count nodes ---------------//
  n_leaves_ = 0;
  int total_branch_nodes = 0;
  for(BN_iterator BN(start);BN;BN++) {
    total_branch_nodes++;
    if (is_leaf_node(*BN))
      n_leaves_++;
  }
  nodes_.resize(1+total_branch_nodes/2);
  branches_.resize(total_branch_nodes);

  if (n_leaves_ == 1) {
    recompute(start);
    return;
  }

  //----------- Set the leaf names ------------//
  vector<BranchNode*> work(n_leaves());
  for(BN_iterator BN(start);BN;BN++)
    if (is_leaf_node(*BN))
      work[(*BN)->node] = (*BN);

  //------------- Clear all names -------------//
  for(BN_iterator BN(start);BN;BN++) {
    (*BN)->node = -1;
    (*BN)->branch = -1;
  }

  //----------- recompute all names -----------//
  int n=0;
  int b=0;
  const int B = branches_.size()/2;

  while(work.size()) 
  {
    vector<BranchNode*> temp = work;
    work.clear();

    for(int i=0;i<temp.size();i++) {
      // name the node
      name_node(temp[i],n++);

      if (temp[i]->branch == -1) {
	// name the branch out of the node
	temp[i]->branch = b++;
	temp[i]->out->branch = temp[i]->branch + B;

	// add the parent to the list if we are its last child
	BranchNode* next = get_parent(temp[i]->out);
	if (next) work.push_back(next);
      }
    }
  } 
  assert(b == B);
  assert(n == nodes_.size());

  // give names to nodes and branches
  recompute(start);
}

/// Computes nodes_[] and branch_[] indices, and cached_partitions[]
void Tree::recompute(BranchNode* start,bool recompute_partitions) 
{
  if (not start) return;

  n_leaves_ = 0;
  for(BN_iterator BN(start);BN;BN++) {

    // each leaf node has only one BranchNode, so this works
    if (is_leaf_node(*BN)) n_leaves_++;

    //construct the nodes_ index
    nodes_[(*BN)->node] = *BN;

    //construct the branches_ index
    branches_[(*BN)->branch] = *BN;
  }
  
  check_structure();

  if (recompute_partitions)
    caches_valid = false;
}

void Tree::check_structure() const {
#ifndef NDEBUG

  //----- Check that our lookup tables are right ------//
  for(int i=0;i<nodes_.size();i++) {
    BranchNode* BN = nodes_[i];

    //the cache lookup tables for node names must be correct
    assert(BN->node == i);

    //leaf nodes come before internal nodes
    if (i<n_leaves())
      assert(is_leaf_node(BN));
    else
      assert(is_internal_node(BN));

    //each BranchNode in a node must have the same node name
    for(BranchNode* n = BN->next;n != BN;n=n->next)
      assert(n->node == BN->node);

    //this node and its prev and next nodes must link to each other consistently
    assert(BN->prev->next == BN);
    assert(BN->next->prev == BN);
  }

  for(int i=0;i<branches_.size();i++) {

    BranchNode* BN = branches_[i];

    //the cached lookup tables must correct refer to the lower-named branch
    assert(BN->branch == i);

    if (not n_branches()) continue; 

    //leaf branches must have the same branch name and node name
    if (i < n_leafbranches())
      assert(BN->node == i);

    //reversed branch must have a different name
    assert(std::abs(BN->branch - BN->out->branch) == n_branches());

    //this branch and its reversal must link to each other consistently
    assert(BN->out->out == BN);

    assert(BN->length == BN->out->length);
  }

#endif
}

/// Insert the partial node n2 after the partial node n1 in the ring containing n1.
void insert_after(BranchNode* n1,BranchNode* n2)
{
  n2->node = n1->node;

  n2->prev = n1;
  n2->next = n1->next;

  n2->prev->next = n2;
  n2->next->prev = n2;
}

BranchNode* connect_nodes(BranchNode* n1, BranchNode* n2)
{
  BranchNode* c1 = new BranchNode;
  insert_after(n1,c1);

  BranchNode* c2 = new BranchNode;
  insert_after(n2,c2);

  c1->length = c2->length = -1.0;

  c1->out = c2;
  c2->out = c1;

  return c1;
}

void Tree::reconnect_branch(int source_index, int target_index, int new_target_index)
{
  branchview b = directed_branch(source_index, target_index);

  if (b.target().degree() < 2)
    throw myexception()<<"Cannot move branch away from target "<<target_index<<": degree is "<<b.target().degree();

  BranchNode* target = b.target();
  
  if (nodes_[target_index] == target)
    nodes_[target_index] = target->prev;

  // remove bt from the ring of the old node
  target->prev->next = target->next;
  target->next->prev = target->prev;

  BranchNode* new_target = nodes_[new_target_index];

  insert_after(new_target, target);

  assert(target->node == new_target_index);
  assert(nodes_[target_index]->node == target_index);
  assert(nodes_[new_target_index]->node == new_target_index);
}

int Tree::induce_partition(const dynamic_bitset<>& partition) 
{
  assert(partition.size() == n_leaves());
  
  prepare_partitions();

  dynamic_bitset<> partition1(n_nodes());
  dynamic_bitset<> partition2(n_nodes());

  // copy bits from smaller bitset on leaves to larger bitset on all nodes
  for(int i=0;i<partition.size();i++) {
    if (partition[i])
      partition1.flip(i);
    else
      partition2.flip(i);
  }

  for(int i=n_leaves();i<n_nodes();i++) 
  {
    vector<BranchNode*> group1;
    vector<BranchNode*> group2;

    // divide the branches out into two groups
    BranchNode * BN = nodes_[i];
    do {
      if (not partition1.intersects(cached_partitions[BN->branch]))
	group2.push_back(BN);
      else if (not partition2.intersects(cached_partitions[BN->branch]))
	group1.push_back(BN);
      else {
	group1.clear();
	group2.clear();
	break;
      }
      
      BN = BN->next;
    } while (BN != nodes_[i]);

    // this node can't separate the groups
    if (not group1.size() and not group2.size()) continue;

    BranchNode* bn = NULL;

    // groups are already split!
    if (group1.size() == 1)
      bn = group1[0];
    // groups are already split!
    else if (group2.size() == 1)
      bn = group2[0];
    // split the node and note the name of the newly added branch
    else {
      nodeview new_node = add_leaf_node(group1[0]->node);
      int old_index = group1[0]->node;
      int new_index = new_node;

      for(int i=0;i<group2.size();i++) {
	reconnect_branch(group2[i]->out->node, group2[i]->node, new_node);
	assert(group2[i]->node == new_index);
      }
      for(int i=0;i<group1.size();i++)
	assert(group1[i]->node == old_index);

      bn = new_node;
    }

    if (not bn)
      return -1;
    else
      return std::min(bn->branch,bn->out->branch);
  }
  throw myexception()<<"induce_partition: partition conflicts with tree!";
}

Tree& Tree::operator=(const Tree& T) 
{
  assert(&T != this);

  // bail if we are copying the same thing only ourselves
  if (&T == this)
    return *this;

  // destroy old tree structure
  if (nodes_.size()) TreeView(nodes_[0]).destroy();

  n_leaves_ = T.n_leaves_;
  caches_valid = T.caches_valid;
  cached_partitions.clear();
  if (caches_valid)
    cached_partitions = T.cached_partitions;
  nodes_ = std::vector<BranchNode*>(T.nodes_.size(),(BranchNode*)NULL);
  branches_ = std::vector<BranchNode*>(T.branches_.size(),(BranchNode*)NULL);
  
  // recalculate pointer indices
  BranchNode* start = T.copy();
  recompute(start,false);
  
  return *this;
}

// count depth -> if we are at depth 0, and have
// one object on the stack then we quit

int append_empty_node(vector< vector<BranchNode*> >& tree_stack, vector<string>& labels)
{
  // determine the level in the tree stack
  int level = tree_stack.size() - 1;

  // determine the node index
  int new_index = labels.size();

  // add an empty label for the node
  labels.push_back(string());

  // determine the parent node of the new leaf node
  BranchNode* parent = 0;
  if (level == 0)
  {
    parent = new BranchNode;
    parent->out = parent->next = parent->prev = parent;
  }
  else
    parent = tree_stack[level-1].back()->out;

  // Connect this to a new leaf node of the appropriate index (with that index)
  BranchNode* child = ::add_leaf_node(parent);

  // make sure that non-root leaf nodes keep the lowest numbers
  if (parent->node != -1)
  {
    child->node = parent->node;
    child->branch = child->out->branch = new_index - 1;
    name_node(parent, new_index);
    std::swap(labels[child->node], labels[parent->node]);
  }
  else
    child->node = new_index;
    

  // Set the branch lengths for the new child to -1
  child->out->length = child->length = -1;

  // put the partial node on the tree stack
  tree_stack.back().push_back(child->out);

  return new_index;
}

int push_empty_node(vector< vector<BranchNode*> >& tree_stack, vector<string>& labels)
{
  // increase the depth
  tree_stack.push_back( vector<BranchNode*>() );

  // append the empty node
  return append_empty_node(tree_stack, labels);
}

#include <iostream>

/*
 * Tree -> Branch ;
 * Branch -> [Node] [string] [: double]
 * Node -> (Branch [, Branch]* )
 *
 * pos == index where we are in the Branch rule, and runs from 0 (before start) to 4 (after end).
 */

// FIXME - don't we need to destroy the current tree?
int Tree::parse_and_discover_names(const string& line,vector<string>& labels)
{
  labels.clear();

  const string delimiters = "(),:;";
  const string whitespace = "\t\n ";

  string prev;
  string word;

  vector< vector<BranchNode*> > tree_stack;
  push_empty_node(tree_stack, labels);
  int pos = 0;
    
  for(int i=0;get_word(word,i,line,delimiters,whitespace);prev=word) 
  {
    //std::cerr<<"word = '"<<word<<"'    depth = "<<tree_stack.size()<<"   stack size = "<<tree_stack.back().size()<<std::endl;

    if (word == ";") break;

    //------ Process the data given the current state ------//
    if (word == "(") 
    {
      if (pos != 0)
	throw myexception()<<"In tree file, found '(' in the middle of word \""<<prev<<"\"";

      push_empty_node(tree_stack, labels);
      pos = 0;
    }
    else if (word == ",")
    {
      append_empty_node(tree_stack, labels);
      pos = 0;
    }
    else if (word == ")") 
    {
      // We need at least 2 levels of trees
      if (tree_stack.size() < 2)
	throw myexception()<<"In tree file, too many end parenthesis.";

      // destroy the top level
      tree_stack.pop_back();
      pos = 1;
    }
    else if (word == ":")
    {
      if (pos > 2)
	throw myexception()<<"Cannot have a ':' here! (pos == "<<pos<<")";
      pos = 3;
    }
    else
    {
      BranchNode* BN = tree_stack.back().back()->out;

      if (pos == 0 or pos == 1) {
	labels[BN->node] = word;
	pos = 2;
      }
      else if (pos == 3)
      {
	BN->out->length = BN->length = convertTo<double>(word);	
	pos = 4;
      }
    }
  }


  if (tree_stack.size() != 1)
    throw myexception()<<"Attempted to read w/o enough left parenthesis";
  if (tree_stack.back().size() != 1)
    throw myexception()<<"Multiple trees on the same line";

  BranchNode* remainder = tree_stack.back()[0];
  BranchNode* root_ = TreeView::unlink_subtree(remainder->out);

  // Handle root_ being a leaf
  if (::is_leaf_node(root_)) {
    root_->node = labels.size();
    throw myexception()<<"Tree has an unnamed leaf node at the root.  Please remove the useless branch to the root.";
  }

  TreeView(remainder).destroy();

  // destroy old tree structure
  if (nodes_.size()) TreeView(nodes_[0]).destroy();

  // determine nodes_[]
  nodes_.resize(labels.size());
  for(BN_iterator BN(root_);BN;BN++) 
    nodes_[(*BN)->node] = *BN;

  vector<BranchNode*> old_nodes = nodes_;

  // switch to new tree structure
  n_leaves_ = 0;
  for(BN_iterator BN(root_);BN;BN++)
    if (is_leaf_node(*BN))
      (*BN)->node = n_leaves_++;
    else
      (*BN)->node = -1;

  reanalyze(root_);

  vector<string> new_labels(labels.size());
  for(int i=0;i<old_nodes.size();i++)
    new_labels[old_nodes[i]->node] = labels[i];

  labels = new_labels;

  return root_->node;
}

int Tree::parse_with_names(const string& line,const vector<string>& names)
{
  return parse_with_names_or_numbers(line,names,false);
}

int Tree::parse_with_names_or_numbers(const string& line,const vector<string>& names,bool allow_numbers)
{
  if (names.size() == 0 and not allow_numbers)
    throw myexception()<<"Tree::parse_with_names_or_numbers( ): must supply leaf names if integers are not allowed.";

  vector< vector<BranchNode*> > tree_stack(1);

  const string delimiters = "(),:;";
  const string whitespace = "\t\n ";

  string prev;
  string word;
  for(int i=0;get_word(word,i,line,delimiters,whitespace);prev=word) 
  {
    //std::cerr<<"word = '"<<word<<"'    depth = "<<tree_stack.size()<<"   stack size = "<<tree_stack.back().size()<<std::endl;

    if (word == ";") break;

    //------ Process the data given the current state ------//
    if (word == "(") {
      tree_stack.push_back(vector<BranchNode*>());
      if (not (prev == "(" or prev == "," or prev == ""))
	throw myexception()<<"In tree file, found '(' in the middle of word \""<<prev<<"\"";
    }
    else if (word == ")") {
      // We need at least 2 levels of trees
      if (tree_stack.size() < 2)
	throw myexception()<<"In tree file, too many end parenthesis.";

      // merge the trees in the top level
      BranchNode* BN = tree_stack.back()[0];
      for(int i=1;i<tree_stack.back().size();i++)
	TreeView::merge_nodes(BN,tree_stack.back()[i]);

      // destroy the top level
      tree_stack.pop_back();

      // insert merged trees into the next level down
      BN = ::add_leaf_node(BN);
      BN->out->length = BN->length = -1;
      tree_stack.back().push_back(BN);
    }
    else if (prev == "(" or prev == "," or prev == "") 
    {
      int leaf_index = -1;
      if (allow_numbers and can_be_converted_to<int>(word,leaf_index)) {
	leaf_index = convertTo<int>(word)-1;
	if (leaf_index < 0)
	  throw myexception()<<"Leaf index '"<<word<<"' is negative: not allowed!";
	if (leaf_index >= names.size())
	  throw myexception()<<"Leaf index '"<<word<<"' is too high: the taxon set contains only "<<names.size()<<" taxa.";
      }
      else if (names.size() == 0)
	  throw myexception()<<"Leaf name '"<<word<<"' is not an integer!";
      else 
      {
	leaf_index = find_index(names,word);
	if (leaf_index == -1)
	  throw myexception()<<"Leaf name '"<<word<<"' is not in the specified taxon set!";
      }

      BranchNode* BN = new BranchNode(-1,leaf_index,-1);
      BN->out = BN->next = BN->prev = BN;

      BN = ::add_leaf_node(BN);
      BN->out->length = BN->length = -1;
      tree_stack.back().push_back(BN);
    }
    else if (prev == ":") {
      BranchNode* BN = tree_stack.back().back();
      BN->out->length = BN->length = convertTo<double>(word);
    }
  }


  if (tree_stack.size() != 1)
    throw myexception()<<"Attempted to read w/o enough left parenthesis";
  if (tree_stack.back().size() != 1)
    throw myexception()<<"Multiple trees on the same line";

  BranchNode* remainder = tree_stack.back()[0];
  BranchNode* root_ = TreeView::unlink_subtree(remainder->out);
  TreeView(remainder).destroy();

  // Handle root_ being a leaf
  if (::is_leaf_node(root_)) {
    root_->node = names.size();
    throw myexception()<<"Tree has an unnamed leaf node at the root.  Please remove the useless branch to the root.";
  }

  // destroy old tree structure
  if (nodes_.size()) TreeView(nodes_[0]).destroy();

  // switch to new tree structure
  reanalyze(root_);

  return root_->node;
}

Tree::Tree()
  :caches_valid(false),
   n_leaves_(0) 
{}

Tree::Tree(const BranchNode* BN) 
  :caches_valid(false)
{
  reanalyze(TreeView::copy_tree(BN));
}

Tree::Tree(const Tree& T) 
    :caches_valid(T.caches_valid),
     cached_partitions(T.cached_partitions),
     n_leaves_(T.n_leaves_),
     nodes_(T.nodes_.size(),(BranchNode*)NULL),
     branches_(T.branches_.size(),(BranchNode*)NULL)
{
    // recalculate pointer indices
    BranchNode* start = T.copy();
    recompute(start,false);
}

Tree::~Tree() 
{
  for(int i=0;i<branches_.size();i++)
    delete branches_[i];
}

void add_left_right(BranchNode*& top, BranchNode* left,BranchNode* right) {
  BranchNode* n = new BranchNode;

  n->prev = left;
  n->prev->next = n;

  n->next = right;
  n->next->prev = n;

  n->prev->prev = n->next;
  n->next->next = n->prev;

  top->out = n;
  n->out = top;
}


// IDEA - always include root and root->out, but if we don't want a stem
// just make the ring which WOULD contain root->out skip it.  We still have
// pointers INTO the ring from root->out->{prev,next}.

void swap_children(BranchNode* b) {
  // this assumes BINARY trees, where every node has exactly two children
  assert(b->next->next->next == b);

  std::swap(b->prev,b->next);

  b->prev->prev = b->next;
  b->prev->next = b;

  b->next->next = b->prev;
  b->next->prev = b;
} 

void RootedTree::recompute(BranchNode* start,bool recompute_partitions) {
  Tree::recompute(start,recompute_partitions);

  //  asdf;
  //   std::abort();
  // now make sure that nodes_[i] points to the TOP node in the ring
}

void RootedTree::check_structure() const {
#ifndef NDEBUG
  Tree::check_structure();
  /*
  bool found = false;
  for(int i=0;i<branches_.size() and not found;i++)
    if (root_ == branches_[i])
      found = true;

  if (not nodes_.size())
    assert(root_ == (BranchNode*)NULL);
  else if (not found) {
    throw myexception()<<"RootedTree: root node is none of our nodes!";
    assert(found);
  }
  */
#endif
}


/// Note that names are recalculated from scratch here.
void RootedTree::remove_node_from_branch(int node) 
{
  if (root_->node == node)
    root_ = nodes_[0];

  Tree::remove_node_from_branch(node);
}

vector<int> RootedTree::prune_leaves(const vector<int>& remove) 
{
  root_ = NULL;

  // if we need to do this, virtualize unlink_subtree to complain if the subtree
  // contains the root.

  return Tree::prune_leaves(remove);
}


BranchNode* gen_root() {
  BranchNode* root = new BranchNode;
  root->prev = root->next = root;

  root->out = new BranchNode;
  root->out->prev = root->out->next = root->out;
  root->out->out = root;
  
  return root;
}

void RootedTree::reroot(int n) {
  assert(0 <= n and n < n_nodes());
  root_ = nodes_[n];
}

int RootedTree::common_ancestor(int i,int j) const {
  assert(0 <= i and i < n_nodes()); 
  assert(0 <= j and j < n_nodes()); 

  BranchNode* BN = root_;
  
  do {
    BN = BN->out;
    while(not subtree_contains(BN->branch,i))
      BN = BN->next;

    assert(subtree_contains(BN->branch,j));
  } while(subtree_contains(BN->branch,j));

  return BN->node;
}

void RootedTree::add_first_node() {
  Tree::add_first_node();
  root_ = nodes_[0];
}

RootedTree& RootedTree::operator=(const RootedTree& RT) {

  Tree::operator=(RT);

  root_ = nodes_[RT.root_->node];

  return *this;
}

int RootedTree::parse_and_discover_names(const string& s,vector<string>& names)
{
  int r = Tree::parse_and_discover_names(s, names);

  root_ = nodes_[r];

  return r;
}

int RootedTree::parse_with_names_or_numbers(const string& s,const vector<string>& names, bool allow_numbers)
{
  int r = Tree::parse_with_names_or_numbers(s, names, allow_numbers);

  root_ = nodes_[r];

  return r;
}

string write(const RootedTree& T, const vector<string>& names, bool print_lengths) 
{
  return write(T.root(), names, print_lengths);
}

string write(const Tree& T, const vector<string>& names, bool print_lengths) 
{
  return write(T.directed_branch(0).target(), names, print_lengths);
}

string write_no_names(const RootedTree& T, bool print_lengths) 
{
  return write_no_names(T.root(), print_lengths);
}

string write_no_names(const Tree& T, bool print_lengths) 
{
  return write_no_names(T.directed_branch(0).target(), print_lengths);
}

RootedTree::RootedTree(const BranchNode* BN)
  :root_(TreeView::copy_tree(BN)) 
{
  reanalyze(root_);
}

RootedTree::RootedTree(const Tree& T,int r)
  :Tree(T),root_(nodes_[r])
{ }

RootedTree::RootedTree(const RootedTree& RT)
  :Tree(RT),root_(nodes_[RT.root_->node])
{ }

RootedTree::RootedTree(const RootedTree& t1, const RootedTree& t2) 
  :Tree(t1),root_(nodes_[t1.root_->node])
{
  merge_tree(root(), t2, t2.root());
}

RootedTree add_root(Tree T,int b) {
  T.create_node_on_branch(b);
  return RootedTree(T,T.n_nodes()-1);
}

bool branch_order(const const_branchview& b1,const const_branchview& b2) {
    return b1.target().name() < b2.target().name();
}

vector<const_branchview> sorted_neighbors(const_nodeview n) {
  vector<const_branchview> branches;
  append(n.branches_out(),branches);

  std::sort(branches.begin(),branches.end(),branch_order);

  return branches;
}


vector<const_branchview> sorted_branches_after(const_branchview b) {
  vector<const_branchview> branches;
  append(b.branches_after(),branches);

  std::sort(branches.begin(),branches.end(),branch_order);

  return branches;
}

Tree star_tree(int n) 
{
  BranchNode* center = get_first_node();

  if (n > 1)
    for(int i=0;i<n;i++)
      add_leaf_node(center)->node = i;

  return Tree(center);
}

boost::dynamic_bitset<> branch_partition(const Tree& T,int b) 
{
  const dynamic_bitset<>& temp = T.partition(b);
  dynamic_bitset<> p(T.n_leaves());
  for(int i=0;i<p.size();i++)
    if (temp[i])
      p[i].flip();

  return p;
}


double length(const Tree& T) {
  double total = 0;
  for(int i=0;i<T.n_branches();i++)
    total += T.branch(i).length();
  return total;
}

int subtree_height(const Tree& T,int b) 
{
  int node = T.directed_branch(b).target();

  int depth = 0;
  for(int i=0;i<T.n_leaves();i++) 
    if (T.partition(b)[i])
      depth = std::max(depth, T.edges_distance(node,i) );

  return depth;
}

int node_depth(const Tree& T,int node) 
{
  // re-write using branches_from_node

  int depth = T.n_branches();
  for(int i=0;i<T.n_leaves();i++) 
    depth = std::min(depth, T.edges_distance(node,i) );

  return depth;
}


vector< vector<int> > partition_sets(const Tree& T)
{
  // set up cached partition sets
  vector< vector<int> > sets(2*T.n_branches());

  for(int b=0;b<2*T.n_branches();b++)
    for(int i=0;i<T.n_leaves();i++) 
      if (T.partition(b)[i]) 
	sets[b].push_back(i);

  return sets;
}


bool is_Cayley(const Tree& T)
{
  for(int i=0;i<T.n_nodes();i++)
  {
    int d = T[i].degree();
    if (d != 1 and d != 3)
      return false;
  }
  return true;
}

bool has_sub_branches(const Tree& T)
{
  for(int i=0;i<T.n_nodes();i++)
  {
    if (T[i].degree() == 2)
      return true;
  }
  return false;
}

bool has_polytomy(const Tree& T)
{
  for(int i=0;i<T.n_nodes();i++)
  {
    if (T[i].degree() > 3)
      return true;
  }
  return false;
}

