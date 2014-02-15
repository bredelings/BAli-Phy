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

#include "tree/tree.H"
#include "util.H"
#include <algorithm>
#include <sstream>
#include "myexception.H"

using std::vector;
using std::string;
using std::pair;
using boost::any;
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

  BranchNode* start2 = new BranchNode;
  {
    start2->node_attributes = start->node_attributes->unused_copy();
  }
  BranchNode* n2 = start2;

  if (start->out == start)
    start2->out = start2;

  do {

    n1 = n1->next;

    if (n1 == start)
      n2->next = start2;
    else
    {
      n2->next = new BranchNode;
      n2->next->node_attributes = start2->node_attributes;
    }

    n2->next->prev = n2;

    n2 = n2->next;
  } while(n1 != start);

  return n2;
}

// This routine walks the entire original tree using the update "n = n->out->next".
// It visits each directed branch (e.g. BranchNode) exactly once.
// - on visiting a new BranchNode, it copies the branch attributes.
// - before it moves to a new BranchNode, it makes sure that the node exists and copies the attributes.
BranchNode* TreeView::copy_tree(const BranchNode* start) {

  const BranchNode* here1 = start;
  BranchNode* start2 = copy_node(start);
  BranchNode* here2 = start2;

  // If this is a bare node (e.g. a single-noe tree) then we don't need to do anything else.
  if (start2->out == start2)
    return start2;
  
  do {
    
    // 1. If we jump out to another node, then create it if its not there (and copy node attributes)
    if (!here2->out) {
      here2->out = copy_node(here1->out);
      here2->out->out = here2;
    }

    // Now that our destination node is sure to exist, we can refer to the reverse branch

    // 2. Share undirected branch attributes if already present, otherwise copy them from original tree.
    if (here2->out->undirected_branch_attributes)
      here2->undirected_branch_attributes = here2->out->undirected_branch_attributes;
    else
      here2->undirected_branch_attributes = here1->undirected_branch_attributes->unused_copy();
      
    // 3. Copy directed branch attributes from original tree.
    here2->directed_branch_attributes = here1->directed_branch_attributes->unused_copy();

    // 4. Move to next directed branch
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

/// Insert the partial node n2 after the partial node n1 in the ring containing n1.
void insert_after(BranchNode* n1,BranchNode* n2)
{
  n2->node_attributes = n1->node_attributes;
  
  n2->prev = n1;
  n2->next = n1->next;
  
  n2->prev->next = n2;
  n2->next->prev = n2;
}

void insert_branch_into_node(BranchNode* n1, BranchNode* n3)
{
  if (n3->out == n3)
  {
    BranchNode* n2 = n1->out;
    n1->out = n3;
    n3->out = n1;

    n3->directed_branch_attributes = n2->directed_branch_attributes;
    n3->undirected_branch_attributes = n2->undirected_branch_attributes;

    delete n2;
  }
  else
    insert_after(n3, n1->out);
}


/// Unlink the branch (n1,n2=n1->out) from n2
BranchNode* unlink_branch_from_node(BranchNode* n1)
{
  assert(n1->out != n1);
  BranchNode* n2 = n1->out;

  // we need to insert a new BranchNode here so that we don't remove the node, just the edge to it.
  if (is_leaf_node(n2)) 
  {
    // the node will become a degree 0 node
    BranchNode* degree_0_node = n2;
    degree_0_node->out = degree_0_node;

    // link n1 to a new node *fragment*, and call the fragment n2
    n1->out = new BranchNode;
    n2 = n1->out;
    n2->out = n1;

    insert_after(n2, degree_0_node);
  }

  // disconnect node n2 from its node
  n2->prev->next = n2->next;
  n2->next->prev = n2->prev;

  // but disconnect it from its former node
  n2->node_attributes.reset();

  // n1->out is returned as a node *fragment*, and therefore has no node attributes, though it does have branch attributes.
  BranchNode* remainder = n2->prev;

  // relink n2 as a leaf node so that we can easily walk the resulting tree to clean it up.
  n2->prev = n2->next = n2;

  return remainder;
}

// unlink the branch (n1,n2=n1->out) from n2 and relink it to n3
// all properties are preserved
BranchNode* reconnect_branch(BranchNode* n1, BranchNode* n3)
{
  // if we're already attached to n3, don't do all this stuff.
  if (n1->out->node_attributes->name == n3->node_attributes->name) return n1->out;

  // unlink the n1->out from its node, possibly create a new BranchNode if n1->out is 
  BranchNode* remainder = unlink_branch_from_node(n1);

  // insert n1->out into node n3, possibly deleting it if n3 is a leaf node.
  insert_branch_into_node(n1,n3);

  // return a pointer to a part of the node that used to contain n2.
  return remainder;
}

// rewrite use a new reconnect_branch( ) operation.
void TreeView::exchange_subtrees(BranchNode* n1, BranchNode* n2) 
{
  // I should assert that the subtrees are disjoint, somehow...

  // Neither branches comes from a leaf node
  assert(not is_leaf_node(n1));
  assert(not is_leaf_node(n2));

  // The nodes are distinct
  assert(n1 != n2);

  BranchNode* b1 = n1->out;
  n1 = n1->prev;
  BranchNode* b2 = n2->out;
  n2 = n2->prev;
    
  reconnect_branch(b1,n2);
  reconnect_branch(b2,n1);
}

// the node attributes of the second node will be lost!
void TreeView::merge_nodes(BranchNode* n1,BranchNode* n2) 
{
  // merge the two cycles
  std::swap(n1->next,n2->next);
  n1->next->prev = n1;
  n2->next->prev = n2;

  // make sure the node fragments point to the same node attribute structure!
  BranchNode* n = n1;

  do {
    n->node_attributes = n1->node_attributes;
    n = n->next;
  } while (n != n1);
  
}

// this preserves sub-branch directions
// the node with the smaller name maintains the name of its attatched branch

//         b1 <-----> b2
//            goes to...


BranchNode* TreeView::create_node_on_branch(BranchNode* b1, int  new_branchname) {

  BranchNode* b2 = b1->out;

  // Create a ring of size 2 - duplicate branch names and lengths
  BranchNode* n1 = new BranchNode;
  n1->node_attributes = new tree_attributes(b1->node_attributes->size());
  n1->directed_branch_attributes = new tree_attributes(b1->directed_branch_attributes->size());

  BranchNode* n2 = new BranchNode;
  n2->node_attributes = n1->node_attributes;
  n2->directed_branch_attributes = new tree_attributes(b1->directed_branch_attributes->size());

  n1->next = n1->prev = n2;
  n2->next = n2->prev = n1;

  // Link from ring to branch endpoints
  n1->out = b1;
  b1->out = n1;

  n2->out = b2;
  b2->out = n2;

  int delta = std::abs(b2->directed_branch_attributes->name - b1->directed_branch_attributes->name);

  // choose sub-branch to give the new name to. (It will go to the one pointed to by b2)
  if (b1->node_attributes->name > b2->node_attributes->name)
    std::swap(b1,b2);

  b1->out->undirected_branch_attributes.swap( b2->undirected_branch_attributes );
  b1->out->directed_branch_attributes.swap( b2->directed_branch_attributes );
  assert(b1->undirected_branch_attributes == b1->out->undirected_branch_attributes );


  // determine sub-branch direction;
  if (b2->directed_branch_attributes->name > b2->out->directed_branch_attributes->name)
    b2 = b2->out;

  // set new branch name and set length to 0
  b2->undirected_branch_attributes = new tree_attributes(b1->undirected_branch_attributes->size());
  b2->out->undirected_branch_attributes = b2->undirected_branch_attributes;
  if (not (*b2->out->undirected_branch_attributes)[0].empty())
    (*b2->undirected_branch_attributes)[0] = double(0.0);

  b2->undirected_branch_attributes->name = new_branchname;
  b2->directed_branch_attributes->name = new_branchname;
  b2->out->directed_branch_attributes->name = new_branchname + delta;

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

  int b1_name = b1->undirected_branch_attributes->name;
  int b2_name = b2->undirected_branch_attributes->name;

  // Preserve the name of the branch with the smaller name (to avoid renaming leaf branches!)
  // (The name of the b1<--->n1 branch gets preserved)

  bool swap_branches = false;
  if (branch_to_move != -1)
  {
    if (b1->directed_branch_attributes->name == branch_to_move or 
	b1->out->directed_branch_attributes->name == branch_to_move)
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
      assert(b2->directed_branch_attributes->name == branch_to_move or 
	     b2->out->directed_branch_attributes->name == branch_to_move);

  //---------- get delta - and check it ------------//
#ifndef NDEBUG
  int delta1 = std::abs(b1->directed_branch_attributes->name - b1->out->directed_branch_attributes->name);
  int delta2 = std::abs(b2->directed_branch_attributes->name - b2->out->directed_branch_attributes->name);
  assert(delta1 == delta2);
#endif

  //-- Connect branches, merge lengths, use new name --//
  b1->out = b2;
  b2->out = b1;

  // Issue! If we remove a root node, we arbitrarily the attributes (such as they are
  // from one child, while ignoring the other.

  if (not (*b1->undirected_branch_attributes)[0].empty())
    (*b1->undirected_branch_attributes)[0] = 
      boost::any_cast<double>((*b1->undirected_branch_attributes)[0]) +
      boost::any_cast<double>((*b2->undirected_branch_attributes)[0]);

  // Summary: here we are deleting the n2<-->b2 branch.

  // preserve the direction of the remaining branch.
  b2->undirected_branch_attributes = n1->undirected_branch_attributes; 
  b2->directed_branch_attributes = n1->directed_branch_attributes; 

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
    // we're trying to remove a subtree from the node, not destroy the node.
    BranchNode* copy = new BranchNode;
    copy->prev = copy->next = copy->out = copy;
    copy->node_attributes = b->node_attributes->unused_copy();

    // we return what remains, which is the copy, NOT b.
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


string get_attribute_string(const tree_attributes& attributes, const vector<string>& attribute_names, const string& prefix, const string& delim, int ignore_index = -1)
{
  string output = "";

  for(int i=0;i<attributes.size() and i<attribute_names.size();i++)
  {
    // The label should be (will be) printed above.  Don't print it here.
    if (i == ignore_index) continue;

    if (attribute_names[i].size() and boost::any_cast<string>(&attributes[i]))
    {
      if (output.size()) output += delim;
      output += attribute_names[i] + "=" + boost::any_cast<string>(attributes[i]);
    }
  }

  if (output.size())
    output = "[" + prefix + output + "]";

  return output;
}

string write(const vector<string>& names, 
	     const vector<string>& node_attribute_names, const vector<string>& undirected_branch_attribute_names,
	     const_branchview b, bool print_lengths)
{
  const int node_label_index = 0;
  const int branch_length_index = 0;

  string output;

  // If this is an internal node, then print the subtrees
  if (b.target().is_internal_node()) 
  {
    vector<const_branchview> branches = sorted_branches_after(b);
    output = "(";
    for(int i=0;i<branches.size();i++) {
      output += write(names,node_attribute_names,undirected_branch_attribute_names,branches[i],print_lengths);

      if (i+1<branches.size())
	output += ",";
    }
    output += ")";
  }

  const_nodeview n = b.target();

  // Print the name (it might be empty)
  if (names[n].size())
    output += "'" + names[n] + "'";

  // Print the node attributes, if any
  output += get_attribute_string(n.attributes(), node_attribute_names, "&&NHX:", ":", node_label_index);

  string branch_output = ":";

  // print the branch length if requested
  if (print_lengths and b.has_length())
    branch_output += convertToString(b.length());
  branch_output += get_attribute_string(b.undirected_attributes(), undirected_branch_attribute_names, "&&NHX:", ":", branch_length_index);

  if (branch_output.size() > 1)
    output += branch_output;

  return output;
}

string write(const_nodeview root, const vector<string>& names,
	     const vector<string>& node_attribute_names, const vector<string>& undirected_branch_attribute_names,
	     bool print_lengths) 
{
  const int node_label_index = 0;
  //  const int branch_length_index = 0;

  string output;

  // If this is an internal node, then print the subtrees
  vector<const_branchview> branches = sorted_neighbors(root);
  if (branches.size())
  {
    output = "(";
    for(int i=0;i<branches.size();i++) {
      output += write(names,node_attribute_names,undirected_branch_attribute_names,branches[i],print_lengths);
      if (i+1 < branches.size())
	output += ',';
    }
    output += ")";
  }

  // Print the name (it might be empty)
  if (names[root].size())
    output += "'" + names[root] + "'";

  // Print the node attributes, if any
  output += get_attribute_string(root.attributes(), node_attribute_names, "&&NHX:", ":", node_label_index);

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

void name_node(BranchNode* start,int i) 
{
  BranchNode* n = start;

  n->node_attributes->name = i;
  do {
    assert(n->node_attributes == start->node_attributes);
    n = n->next;
  } while (n != start);
}

const vector<int>& Tree::leaf_nodes() const
{
  // Require that leaf node ORDER is determined by the node_ order,
  // and in increasing order of node names.
  if (not leaf_nodes_.is_valid())
  {
    leaf_nodes_.modify_value().clear();
    for(int i=0;i<nodes_.size();i++)
      if (is_leaf_node(nodes_[i]))
	leaf_nodes_.modify_value().push_back(i);
    leaf_nodes_.validate();
  }

  assert(n_leaves() == leaf_nodes_.access_value().size());

  return leaf_nodes_;
}

const vector<int>& Tree::internal_nodes() const
{
  // Require that internal node ORDER is determined by the node_ order,
  // and in increasing order of node names.
  if (not internal_nodes_.is_valid())
  {
    internal_nodes_.modify_value().clear();
    for(int i=0;i<nodes_.size();i++)
      if (is_internal_node(nodes_[i]))
	internal_nodes_.modify_value().push_back(i);
    internal_nodes_.validate();
  }

  assert(n_nodes() - n_leaves() == internal_nodes_.access_value().size());

  return internal_nodes_;
}

const vector<tree_edge>& Tree::leaf_branches() const
{
  // Require that leaf branch ORDER is determined by the branches_ order,
  // and in increasing order of node names.
  if (not leaf_branches_.is_valid())
  {
    leaf_branches_.modify_value().clear();
    if (nodes_.size() == 2)
      leaf_branches_.modify_value().push_back(nodes_[0]);
    else
    {
      for(int i=0;i<branches_.size();i++)
	// orient branches away from leaf nodes
	if (is_branch(branches_[i]) and is_leaf_node(branches_[i]))
	  leaf_branches_.modify_value().push_back( branches_[i] );
    }
    leaf_branches_.validate();
  }

  assert(leaf_branches_.value().size() == n_leafbranches());

  return leaf_branches_;
}

const vector<tree_edge>& Tree::internal_branches() const
{
  // Require that internal branch ORDER is determined by the branches_ order,
  // and in increasing order of node names.
  if (not internal_branches_.is_valid())
  {
    internal_branches_.modify_value().clear();
    for(int i=0;i<branches_.size();i++)
      // Choose the orientation with the smaller name
      if (is_internal_branch(branches_[i]) and branches_[i]->directed_branch_attributes->name < branches_[i]->out->directed_branch_attributes->name)
	internal_branches_.modify_value().push_back( branches_[i] );
    internal_branches_.validate();
  }

  assert(internal_branches_.value().size() == n_branches() - n_leafbranches());

  return internal_branches_;
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
    nodes_[i]->node_attributes->name = lnames[i];

  //---------- recompute everything -----------//
  reanalyze(nodes_[0]);

  //------------- compute mapping -------------//
  vector<int> mapping(old_nodes.size());
  for(int i=0;i<mapping.size();i++)
    mapping[i] = old_nodes[i]->node_attributes->name;

  return mapping;
}

/// Give leaf nodes their new names
void shift_leaves(BranchNode* start,int first,int n) {
  for(BN_iterator BN(start);BN;BN++) {
    if (not is_leaf_node(*BN))
      continue;

    if ((*BN)->node_attributes->name >= n)
      ((*BN)->node_attributes->name)--;

    (*BN)->node_attributes->name += first;
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
      while(is_leaf_node(leaf) and leaf->out and leaf->out != leaf) 
      {
	old_nodes[leaf->node_attributes->name] = 0;
	BranchNode* parent = TreeView::unlink_subtree(leaf->out);
	TreeView(leaf).destroy();
	leaf = parent;
      }

      // remove nodes of degree 2
      if (leaf->next != leaf and leaf->next->next == leaf)
      {
	old_nodes[leaf->node_attributes->name] = 0;
	TreeView::remove_node_from_branch(leaf);
      }
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

  // Construct the map from new to old node names
  vector<int> mapping(n_nodes(), -1);
  for(int i=0;i<old_nodes.size();i++)
    if (old_nodes[i])
      mapping[old_nodes[i]->node_attributes->name] = i;

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

  shift_leaves( n, 0  ,  n->node_attributes->name);
  shift_leaves(tn, nl1, tn->node_attributes->name);

  TreeView::merge_nodes(n,tn);

  //------------------------- Setup ---------------------------//
  assert(not is_leaf_node(n));
  assert(not is_leaf_node(tn));

  reanalyze(nodes_[0]);

  assert(n->node_attributes->name == tn->node_attributes->name);
}

double Tree::distance(int i,int j) const {
  double d=0;

  BranchNode* b = nodes_[i];

  while (b->node_attributes->name != j) {
    if (subtree_contains(b->directed_branch_attributes->name,j)) {
      d += boost::any_cast<double>((*b->undirected_branch_attributes)[0]);
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

  while (b->node_attributes->name != j) {
    if (subtree_contains(b->directed_branch_attributes->name,j)) {
      d++;
      b = b->out;
    }
    else 
      b = b->next;
  }
  return d;
}

BranchNode* get_first_node(int n_node_attributes) {
  BranchNode* BN = new BranchNode;
  BN->node_attributes = new tree_attributes(n_node_attributes);
  BN->prev = BN->next = BN;
  BN->out = BN;

  return BN;
}

void Tree::add_first_node() {
  if (nodes_.size())
    throw myexception()<<"Trying to add first node to tree which is not empty";

  BranchNode* BN = get_first_node(n_node_attributes());

  nodes_.push_back(BN);
  branches_.push_back(BN);

  n_leaves_ = 1;

  leaf_nodes_.invalidate();
  internal_nodes_.invalidate();

  leaf_branches_.invalidate();
  internal_branches_.invalidate();
}

BranchNode* add_leaf_node(BranchNode* n, int n_u_a, int n_d_a) 
{
  // The spot to which to link the new node.
  BranchNode* n_link = NULL;

  // if n is a bare node, then the new node with link to n
  if (n->out == n)
    n_link = n;
  // otherwise it links to a new BranchNode inserted next to n
  else {
    assert(n_u_a == n->undirected_branch_attributes->size());
    assert(n_d_a == n->directed_branch_attributes->size());

    n_link = new BranchNode;
    n_link->node_attributes = n->node_attributes;
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

  n_leaf->node_attributes = new tree_attributes(n->node_attributes->size());
  n_leaf->undirected_branch_attributes = new tree_attributes(n_u_a);
  n_leaf->out->undirected_branch_attributes = n_leaf->undirected_branch_attributes;
  n_leaf->directed_branch_attributes = new tree_attributes(n_d_a);
  n_leaf->out->directed_branch_attributes = new tree_attributes(n_d_a);

  return n_leaf;
}


nodeview Tree::add_leaf_node(int node) 
{
  assert(0 <= node and node < nodes_.size());
  int n_branches_old = n_branches();

  // Update the directed branch names
  for(BN_iterator BN(nodes_[0]);BN;BN++) 
  {
    if ((*BN)->directed_branch_attributes->name != (*BN)->undirected_branch_attributes->name)
      (*BN)->directed_branch_attributes->name++;
  }

  // Add the new leaf node to the tree
  BranchNode* n_leaf = ::add_leaf_node(nodes_[node], n_undirected_branch_attributes(), n_directed_branch_attributes());

  n_leaf->node_attributes->name = nodes_.size();

  n_leaf->undirected_branch_attributes->name = n_branches_old;

  n_leaf->directed_branch_attributes->name = n_branches_old;
  n_leaf->out->directed_branch_attributes->name = 2*n_branches_old+1;

  // Update the nodes_ array
  nodes_.push_back(n_leaf);

  leaf_nodes_.invalidate();
  internal_nodes_.invalidate();

  leaf_branches_.invalidate();
  internal_branches_.invalidate();

  // Update the branches_ array
  branches_.resize(branches_.size()+2);
  for(BN_iterator BN(nodes_[0]);BN;BN++) 
  {
    branches_[ (*BN)->directed_branch_attributes->name ] = *BN;
    assert( nodes_[ (*BN)->node_attributes->name ] = *BN);
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

  append(T.node(n).branches_out(),branch_list);

  get_branches_after(branch_list);

  return branch_list;
}  

vector<const_branchview> branches_toward_node(const Tree& T,int n) {
  vector<const_branchview> branch_list;
  branch_list.reserve(T.n_branches());

  append(T.node(n).branches_in(),branch_list);

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
  vector<const_branchview> branch_list = branches_from_node(*this,nodes_[0]->node_attributes->name);
  std::reverse(branch_list.begin(), branch_list.end());

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


// This could create loops it we don't check that the subtrees are disjoint.
// br{1,2} point into the subtrees.  b{1,2} point out of the subtrees, towards the other subtree.
void exchange_subtrees(Tree& T, int br1, int br2) 
{
  branchview b1 = T.directed_branch(br1).reverse();
  branchview b2 = T.directed_branch(br2).reverse();

  int s1 = b1.source();
  int t1 = b1.target();

  int s2 = b2.source();
  int t2 = b2.target();

  assert(not T.subtree_contains(br1,s2));
  assert(not T.subtree_contains(br2,s1));

  T.reconnect_branch(s1,t1,t2);
  T.reconnect_branch(s2,t2,t1);
}

// Currently, we HAVE to assign new branch names, but only to backwards branches
nodeview Tree::create_node_on_branch(int br) 
{
  BranchNode* b = branches_[br];

  int B = branches_.size()/2;
  BranchNode* n = TreeView::create_node_on_branch(b,B);

  // Name the new node
  name_node(n, nodes_.size());
  nodes_.push_back(n);

  // Reserve space for more branches
  branches_.resize(branches_.size()+2);

  // Rename only backwards branches.
  // This does NOT destroy length information.
  for(BN_iterator BN(nodes_[0]);BN;BN++) {
    if ((*BN)->directed_branch_attributes->name > (*BN)->out->directed_branch_attributes->name)
      (*BN)->directed_branch_attributes->name = (*BN)->out->directed_branch_attributes->name + B+1;
  }

  // Recompute the nodes_ and branches_ array and check our invariants.
  recompute(nodes_[0], true);

  return n;
}


void remove_sub_branches(Tree& T)
{
  while(true) 
  {
    // find first node of degree 2
    int n=0;
    while(n<T.n_nodes() and T.node(n).degree() != 2)
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
/// Got m1<--->x<--->m2 and n1<--->n2, and trying to move x onto (n1,n2)
int SPR(Tree& T, int br1,int br2, int branch_to_move) 
{
  int x1 = T.directed_branch(br1).source();
  int x2 = T.directed_branch(br1).target();

  std::vector<const_branchview> m_branches;
  append(T.directed_branch(x2,x1).branches_after(), m_branches);
  assert(m_branches.size() == 2);
  int m1 = m_branches[0].target();
  int m2 = m_branches[1].target();

  int n1 = T.directed_branch(br2).source();
  int n2 = T.directed_branch(br2).target();

  //-------------------- Correctly order m1 and m2 ----------------------//
  // Preserve the name of the branch with the smaller name (to avoid renaming leaf branches!)
  // (The name of the x<--->n1 branch gets preserved)
  if (branch_to_move == -1)
  {
    if (T.directed_branch(m1,x1).undirected_name() > T.directed_branch(m2,x1).undirected_name() )
      std::swap(m1,m2);
  }
  // ensure that (x,m2) is the branch to move
  else
  {
    if (T.directed_branch(m1,x1).name() == branch_to_move or T.directed_branch(x1,m1).name() == branch_to_move)
      std::swap(m1,m2);
    else if (T.directed_branch(m2,x1).name() == branch_to_move or T.directed_branch(x1,m2).name() == branch_to_move)
      ;
    else
      std::abort(); // we couldn't find the branch to move!
  }

  //-------------------- Correctly order n1 and n2 ----------------------//
  // choose sub-branch to give the new name to. (It will go to the one pointed to by b2)
  if (n1 > n2)
    std::swap(n1,n2);

  //------ Merge the branches (m1,x1) and (x1,m2) -------//
  int dead_branch = T.directed_branch(m2,x1).undirected_name();

  T.directed_branch(m1,x1).set_length( T.directed_branch(m1,x1).length() + T.directed_branch(m2,x1).length() );

  T.directed_branch(m2,x1).set_length( 0.0 );

  //------------ Reconnect the branches ---------------//

  // Reconnect (m1,x) to m2, making x a degree-2 node
  // This leaves m1 connected to its branch, so m1 can be a leaf.
  assert(not T.node(m2).is_leaf_node());
  T.reconnect_branch(tree_edge(m1,x1), m2);

  // Reconnect (x,m2) to n2, leaving x a degree-2 node
  T.reconnect_branch(tree_edge(x1,m2), n2);

  // Reconnect (n1,n2) to x, making x a degree-3 node again.
  // This leaves n1 connected to its branch, so n1 can be a leaf.
  assert(not T.node(n2).is_leaf_node());
  T.reconnect_branch(tree_edge(n1,n2), x1);

  return dead_branch;
}

/// Return a pointer to the BN in @start which points towards the root
BranchNode* get_parent(BranchNode* start) {
  BranchNode* BN = start;
  BranchNode* first = NULL;

  assert(start->node_attributes->name == -1);

  // find the first non-visited neighbor
  do {
    if (BN->out->node_attributes->name == -1) {
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

//  This routine should be independent of the circular order at nodes.
/// This routine assumes only that leaf nodes have proper names
void Tree::reanalyze(BranchNode* start) 
{
  nodes_.clear();
  branches_.clear();

  leaf_nodes_.invalidate();
  internal_nodes_.invalidate();

  //--------------- Count nodes ---------------//
  n_leaves_ = 0;
  int total_branch_nodes = 0;
  for(BN_iterator BN(start);BN;BN++) {
    total_branch_nodes++;
    if (is_leaf_node(*BN))
      n_leaves_++;
  }
  nodes_.resize(1+total_branch_nodes/2);

  if (total_branch_nodes > 1) // handle 1-node tree
    branches_.resize(total_branch_nodes);

  if (n_leaves_ == 1) {
    recompute(start);
    return;
  }

  //----------- Set the leaf names ------------//
  vector<BranchNode*> work(n_leaves());
  for(BN_iterator BN(start);BN;BN++)
    if (is_leaf_node(*BN))
      work[(*BN)->node_attributes->name] = (*BN);

  //------------- Clear all names -------------//
  for(BN_iterator BN(start);BN;BN++) {
    (*BN)->node_attributes->name = -1;
    (*BN)->undirected_branch_attributes->name = -1;
    (*BN)->directed_branch_attributes->name = -1;
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

      if (temp[i]->undirected_branch_attributes->name == -1) {
	// name the branch out of the node
	temp[i]->undirected_branch_attributes->name = b;
	temp[i]->directed_branch_attributes->name = b;
	temp[i]->out->directed_branch_attributes->name = b + B;
	b++;

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
  leaf_nodes_.invalidate();
  internal_nodes_.invalidate();

  leaf_branches_.invalidate();
  internal_branches_.invalidate();

  if (not start) return;

  n_leaves_ = 0;
  for(BN_iterator BN(start);BN;BN++) {

    // each leaf node has only one BranchNode, so this works
    if (is_leaf_node(*BN)) n_leaves_++;

    //construct the nodes_ index
    nodes_[(*BN)->node_attributes->name] = *BN;

    // handle the case of a 1-leaf tree.
    if ((*BN)->directed_branch_attributes)
      //construct the branches_ index 
      branches_[(*BN)->directed_branch_attributes->name] = *BN;
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
    assert(BN->node_attributes->name == i);

    //leaf nodes come before internal nodes
    if (i<n_leaves())
      assert(is_leaf_node(BN));
    else
      assert(is_internal_node(BN));

    //each BranchNode in a node must have the same node name
    for(BranchNode* n = BN->next;n != BN;n=n->next)
      assert(n->node_attributes->name == BN->node_attributes->name);

    //this node and its prev and next nodes must link to each other consistently
    assert(BN->prev->next == BN);
    assert(BN->next->prev == BN);
  }

  //------ Check that BranchNode in a node has the same node attributes -------//
  for(BN_iterator BN(nodes_[0]);BN;BN++) 
  {
    assert((*BN)->prev->node_attributes == (*BN)->node_attributes);
    assert((*BN)->next->node_attributes == (*BN)->node_attributes);
  }

  for(int i=0;i<branches_.size();i++) {

    BranchNode* BN = branches_[i];

    //the cached lookup tables must correct refer to the lower-named branch
    assert(BN->directed_branch_attributes->name == i);

    if (not n_branches()) continue; 

    //leaf branches must have the same branch name and node name
    if (i < n_leafbranches())
      assert(BN->node_attributes->name == i);

    //reversed branch must have a different name
    assert(std::abs(BN->directed_branch_attributes->name - BN->out->directed_branch_attributes->name) == n_branches());

    //reversed branch must have a different name
    assert(BN->undirected_branch_attributes->name == std::min(BN->directed_branch_attributes->name,BN->out->directed_branch_attributes->name));

    //reversed branch must have the same undirected attributes
    assert(BN->undirected_branch_attributes == BN->out->undirected_branch_attributes);

    //reversed branch must have the same undirected attributes
    assert(BN->undirected_branch_attributes == BN->out->undirected_branch_attributes);

    //this branch and its reversal must link to each other consistently
    assert(BN->out->out == BN);
  }

#endif
}

BranchNode* connect_nodes(BranchNode* n1, BranchNode* n2, int n_u_a, int n_d_a)
{
  BranchNode* c1 = new BranchNode;
  insert_after(n1,c1);

  BranchNode* c2 = new BranchNode;
  insert_after(n2,c2);

  c1->directed_branch_attributes = new tree_attributes(n_d_a);
  c2->directed_branch_attributes = new tree_attributes(n_d_a);
  
  c1->undirected_branch_attributes = new tree_attributes(n_u_a);
  c2->undirected_branch_attributes = c1->undirected_branch_attributes;

  c1->out = c2;
  c2->out = c1;

  return c1;
}


// There are problems with now, because this could mess up 
// - leaf nodes having low indices
// - leaf branches having low indices
void Tree::reconnect_branch(int source_index, int target_index, int new_target_index)
{
  leaf_nodes_.invalidate();
  internal_nodes_.invalidate();

  branchview b = directed_branch(source_index, target_index);

  if (b.target().degree() < 2)
    throw myexception()<<"Cannot move branch away from target "<<target_index<<": degree is "<<b.target().degree();

  BranchNode* branch = b;

  BranchNode* target = b.target();
  
  if (nodes_[target_index] == target)
    nodes_[target_index] = target->prev;
    
  // NOTE: This makes the circular order at the target node dependent on nodes_.
  // The result is thus alterable by inc_node_pointers()
  BranchNode* new_target = nodes_[new_target_index];

  ::reconnect_branch(branch, new_target);

  check_structure();

  caches_valid = false;
}

void Tree::reconnect_branch(const tree_edge& e, int new_target_index)
{
  reconnect_branch(e.node1, e.node2, new_target_index);
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
      if (not partition1.intersects(cached_partitions[BN->directed_branch_attributes->name]))
	group2.push_back(BN);
      else if (not partition2.intersects(cached_partitions[BN->directed_branch_attributes->name]))
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
      nodeview new_node = add_leaf_node(group1[0]->node_attributes->name);
#ifndef NDEBUG
      int old_index = group1[0]->node_attributes->name;
      int new_index = new_node;
#endif
      for(int i=0;i<group2.size();i++) {
	reconnect_branch(group2[i]->out->node_attributes->name, group2[i]->node_attributes->name, new_node);
	assert(group2[i]->node_attributes->name == new_index);
      }
      for(int i=0;i<group1.size();i++)
	assert(group1[i]->node_attributes->name == old_index);

      bn = new_node;
    }

    if (not bn)
      return -1;
    else
      return bn->undirected_branch_attributes->name;
  }
  throw myexception()<<"induce_partition: partition conflicts with tree!";
}

/// This routines assumes that branches_ is a valid index for all BranchNode's in the tree.
/// It then sets nodes_ to select the same BranchNode (by index) within each node as old_nodes.
/// This ensures that list of branches or nodes connected to a specific node have the same order.
void Tree::set_equivalent_node_pointers(const vector<BranchNode*>& old_nodes)
{
  // Assign the nodes to the same branch they were on the other tree!
  assert(nodes_.size() == old_nodes.size());
  for(int n=0;n<nodes_.size();n++)
  {
    int b = old_nodes[n]->directed_branch_attributes->name;
    nodes_[n] = branches_[b];
    assert(nodes_[n]->node_attributes->name == n);
    assert(nodes_[n]->out->node_attributes->name == old_nodes[n]->out->node_attributes->name);
  }
}

void Tree::inc_node_pointers()
{
  for(int n=0;n<nodes_.size();n++)
    nodes_[n] = nodes_[n]->next;
}

Tree& Tree::operator=(const Tree& T) 
{
  // bail if we are copying the same thing only ourselves
  if (&T == this)
    return *this;

  // destroy old tree structure
  if (nodes_.size()) TreeView(nodes_[0]).destroy();

  n_leaves_ = T.n_leaves_;
  node_label_index = T.node_label_index;
  branch_length_index = T.branch_length_index;

  node_attribute_names = T.node_attribute_names;
  undirected_branch_attribute_names = T.undirected_branch_attribute_names;
  directed_branch_attribute_names = T.directed_branch_attribute_names;

  caches_valid = T.caches_valid;
  cached_partitions.clear();
  if (caches_valid)
    cached_partitions = T.cached_partitions;
  nodes_ = std::vector<BranchNode*>(T.nodes_.size(),(BranchNode*)NULL);
  branches_ = std::vector<BranchNode*>(T.branches_.size(),(BranchNode*)NULL);

  leaf_nodes_.invalidate();
  internal_nodes_.invalidate();
  
  leaf_branches_.invalidate();
  internal_branches_.invalidate();

  // recalculate pointer indices
  BranchNode* start = T.copy();
  recompute(start,false);

  // Assign the nodes to the same branch they were on the other tree!
  set_equivalent_node_pointers(T.nodes_);
  
  return *this;
}

// count depth -> if we are at depth 0, and have
// one object on the stack then we quit

int Tree::add_node_attribute()
{
  int new_index = node_attribute_names.size();

  for(int i=0;i<n_nodes();i++)
  {
    assert( new_index == nodes_[i]->node_attributes->size() );

    nodes_[i]->node_attributes->push_back( boost::any() );
  }

  node_attribute_names.push_back("");

  return new_index;
}

int Tree::add_node_attribute(const string& name)
{
  int index = add_node_attribute();

  node_attribute_names[index] = name;

  return index;
}

int Tree::find_node_attribute_index_by_name(const string& name)  const
{
  return find_index(node_attribute_names,name);
}

void Tree::set_n_node_attributes(int n)
{
  node_attribute_names.resize(n);

  if (nodes_.size())
    for(BN_iterator BN(nodes_[0]);BN;BN++) 
      (*BN)->node_attributes->resize(n);
}

int Tree::find_undirected_branch_attribute_index_by_name(const string& name)  const
{
  return find_index(undirected_branch_attribute_names,name);
}

void Tree::set_n_undirected_branch_attributes(int n)
{
  undirected_branch_attribute_names.resize(n);

  if (nodes_.size()) 
    for(BN_iterator BN(nodes_[0]);BN;BN++) 
      (*BN)->undirected_branch_attributes->resize(n);
}

int Tree::find_directed_branch_attribute_index_by_name(const string& name)  const
{
  return find_index(directed_branch_attribute_names,name);
}

void Tree::set_n_directed_branch_attributes(int n)
{
  directed_branch_attribute_names.resize(n);

  if (nodes_.size())
    for(BN_iterator BN(nodes_[0]);BN;BN++) 
      (*BN)->directed_branch_attributes->resize(n);
}

int append_empty_node(vector< vector<BranchNode*> >& tree_stack, int& n_nodes, int n_n_a, int n_u_a, int n_d_a)
{
  // determine the level in the tree stack
  int level = tree_stack.size() - 1;

  // determine the node index
  int new_index = n_nodes;
  n_nodes++;

  // determine the parent node of the new leaf node
  BranchNode* parent = 0;
  if (level == 0)
  {
    parent = new BranchNode;
    parent->node_attributes = new tree_attributes(n_n_a);
    parent->out = parent->next = parent->prev = parent;
  }
  else {
    parent = tree_stack[level-1].back()->out;

    // Fix the parent to avoid complaints in ::add_leaf_node( )
    parent->node_attributes->resize(n_n_a);
    parent->undirected_branch_attributes->resize(n_u_a);
    parent->directed_branch_attributes->resize(n_d_a);
  }

  // Connect this to a new leaf node of the appropriate index (with that index)
  BranchNode* child = ::add_leaf_node(parent, n_u_a, n_d_a);

  // make sure that non-root leaf nodes keep the lowest numbers
  if (parent->node_attributes->name != -1)
  {
    child->node_attributes->name = parent->node_attributes->name;
    child->undirected_branch_attributes->name = new_index - 1;
    child->directed_branch_attributes->name = child->out->directed_branch_attributes->name = new_index - 1;
    name_node(parent, new_index);
  }
  else
    child->node_attributes->name = new_index;
    
  // put the partial node on the tree stack
  tree_stack.back().push_back(child->out);

  return new_index;
}

int push_empty_node(vector< vector<BranchNode*> >& tree_stack, int& n_nodes, int n_node_attributes, int n_undirected_attributes, int n_directed_attributes)
{
  // increase the depth
  tree_stack.push_back( vector<BranchNode*>() );

  // append the empty node
  return append_empty_node(tree_stack, n_nodes, n_node_attributes, n_undirected_attributes, n_directed_attributes);
}

void set_attributes(const vector<pair<string,any> >& tags, vector<string>& attribute_names, tree_attributes& attributes)
{
  for(int k=0;k<tags.size();k++)
  {
    int index = find_index(attribute_names, tags[k].first );
    
    if (index == -1) {
      index = attribute_names.size();
      attribute_names.push_back( tags[k].first );
    }
    
    if (attributes.size() < attribute_names.size())
      attributes.resize( attribute_names.size() );
    
    attributes[index] = tags[k].second;
  }
}

void add_comments(vector< pair<string,any> >& tags, const vector<string>& comments, const string& prefix, const string& delim)
{
  for(int j=0;j<comments.size();j++)
  {
    // skip this comment if it doesn't have the prefix
    if (comments[j].size() < prefix.size()) continue;
    if (comments[j].substr(0, prefix.size()) != prefix) continue;
    
    // remove the prefix;
    string comment = comments[j].substr(prefix.size(), comments[j].size() - prefix.size());
    
    // split the comment on ','
    vector<string> pieces = split(comment, delim);
    
    for(int k=0;k<pieces.size();k++)
    {
      int L = pieces[k].size();
      
      int sep = pieces[k].find('=',0);
      
      string attribute;
      any value;
      
      if (sep == -1 or sep == 0)
      {
	attribute = pieces[k];
	value = string();
      }
      else
      {
	attribute = pieces[k].substr(0,sep);
	value     = pieces[k].substr(sep+1,L-sep-1);
      }
      
      tags.push_back(pair<string,any>(attribute, value) );
    }
  }
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
int Tree::parse_and_discover_names(const string& line)
{
  node_attribute_names.clear();
  if (node_label_index != -1)
    node_attribute_names.resize(node_label_index+1);

  directed_branch_attribute_names.clear();

  undirected_branch_attribute_names.clear();
  if (branch_length_index != -1)
    undirected_branch_attribute_names.resize(branch_length_index+1);

  const string delimiters = "(),:;";
  const string whitespace = "\t\n ";

  string prev;
  string word;

  if (line.empty()) 
    throw myexception()<<"Trying to parse tree from empty string";

  vector< vector<BranchNode*> > tree_stack;
  vector< string > comments;
  int N = 0;
  push_empty_node(tree_stack, N, n_node_attributes(), n_undirected_branch_attributes(), n_directed_branch_attributes());
  int pos = 0;
    
  for(int i=0;get_word(word,i,comments,line,delimiters,whitespace);prev=word) 
  {
    vector< pair<string, any> > tags;
    add_comments(tags, comments, "&&NHX:", ":");
    add_comments(tags, comments, "&!", ",!");

    //std::cerr<<"word = '"<<word<<"'    depth = "<<tree_stack.size()<<"   stack size = "<<tree_stack.back().size()<<std::endl;

    if (word == ";")
    {
      BranchNode* BN = tree_stack.back().back()->out;
      if (pos == 0 or pos == 1 or pos == 2)
	set_attributes(tags, node_attribute_names, *BN->node_attributes);
      else if (pos == 3 or pos == 4)
	assert( tags.empty() );

      break;
    }

    //------ Process the data given the current state ------//
    if (word == "(") 
    {
      assert( tags.empty() );

      if (pos != 0)
	throw myexception()<<"In tree file, found '(' in the middle of word \""<<prev<<"\"";

      push_empty_node(tree_stack, N, n_node_attributes(), n_undirected_branch_attributes(), n_directed_branch_attributes());
      pos = 0;
    }
    else if (word == ",")
    {
      BranchNode* BN = tree_stack.back().back()->out;

      if (pos == 0 or pos == 1 or pos == 2)
	set_attributes(tags, node_attribute_names, *BN->node_attributes);
      else if (pos == 3 or pos == 4)
	set_attributes(tags, undirected_branch_attribute_names, *BN->undirected_branch_attributes);

      append_empty_node(tree_stack, N, n_node_attributes(), n_undirected_branch_attributes(), n_directed_branch_attributes());
      pos = 0;
    }
    else if (word == ")") 
    {
      BranchNode* BN = tree_stack.back().back()->out;

      if (pos == 0 or pos == 1 or pos == 2)
	set_attributes(tags, node_attribute_names, *BN->node_attributes);
      else if (pos == 3 or pos == 4)
	set_attributes(tags, undirected_branch_attribute_names, *BN->undirected_branch_attributes);

      // We need at least 2 levels of trees
      if (tree_stack.size() < 2)
	throw myexception()<<"In tree file, too many end parenthesis.";

      // destroy the top level
      tree_stack.pop_back();
      pos = 1;
    }
    else if (word == ":")
    {
      BranchNode* BN = tree_stack.back().back()->out;

      set_attributes(tags, node_attribute_names, *BN->node_attributes);

      if (pos > 2)
	throw myexception()<<"Cannot have a ':' here! (pos == "<<pos<<")";
      pos = 3;
    }
    else
    {
      BranchNode* BN = tree_stack.back().back()->out;

      if (pos == 0 or pos == 1) 
      {
	set_attributes(tags, node_attribute_names, *BN->node_attributes);

	if (node_label_index != -1)
	  (*BN->node_attributes)[node_label_index] = word;

	pos = 2;
      }
      else if (pos == 2)
	throw myexception()<<"Node name '"<<word<<"' comes directly after '"<<prev<<"'";
      else if (pos == 3)
      {
	set_attributes(tags, undirected_branch_attribute_names, *BN->undirected_branch_attributes);

	if (branch_length_index != -1)
	  (*BN->undirected_branch_attributes)[branch_length_index] = convertTo<double>(word);	
	pos = 4;
      }
      else if (pos == 4)
	throw myexception()<<"Word name '"<<word<<"' comes directly after branch length '"<<prev<<"'";
    }
  }

  if (tree_stack.size() != 1)
    throw myexception()<<"Attempted to read w/o enough left parenthesis";
  if (tree_stack.back().size() != 1)
    throw myexception()<<"Multiple trees on the same line";

  BranchNode* remainder = tree_stack.back()[0];
  BranchNode* root_ = TreeView::unlink_subtree(remainder->out);

  // Handle root_ being a leaf
  //  if (::is_leaf_node(root_) and )
  //    throw myexception()<<"Tree has an unnamed leaf node at the root.  Please remove the useless branch to the root.";

  TreeView(remainder).destroy();

  // destroy old tree structure
  if (nodes_.size()) TreeView(nodes_[0]).destroy();

  // determine nodes_[]
  nodes_.resize(N);
  for(BN_iterator BN(root_);BN;BN++) 
    nodes_[(*BN)->node_attributes->name] = *BN;

  leaf_nodes_.invalidate();
  internal_nodes_.invalidate();

  // switch to new tree structure
  n_leaves_ = 0;
  for(BN_iterator BN(root_);BN;BN++)
  {
    if (is_leaf_node(*BN))
      (*BN)->node_attributes->name = n_leaves_++;
    else
      (*BN)->node_attributes->name = -1;

    // set the number of attributes to the correct number
    (*BN)->node_attributes->resize(n_node_attributes());
    if ((*BN)->undirected_branch_attributes)
      (*BN)->undirected_branch_attributes->resize(n_undirected_branch_attributes());
    if ((*BN)->directed_branch_attributes)
      (*BN)->directed_branch_attributes->resize(n_directed_branch_attributes());
  }

  reanalyze(root_);

  return root_->node_attributes->name;
}

int get_leaf_index(const string& word, bool allow_numbers, const vector<string>& names)
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
  return leaf_index;
}

int Tree::parse_with_names(const string& line,const vector<string>& names)
{
  return parse_with_names_or_numbers(line,names,false);
}

// FIXME - don't we need to destroy the current tree?
int Tree::parse_with_names_or_numbers(const string& line,const vector<string>& names,bool allow_numbers)
{
  if (names.size() == 0 and not allow_numbers)
    throw myexception()<<"Tree::parse_with_names_or_numbers( ): must supply leaf names if integers are not allowed.";

  node_attribute_names.clear();
  if (node_label_index != -1)
    node_attribute_names.resize(node_label_index+1);

  directed_branch_attribute_names.clear();

  undirected_branch_attribute_names.clear();
  if (branch_length_index != -1)
    undirected_branch_attribute_names.resize(branch_length_index+1);

  const string delimiters = "(),:;";
  const string whitespace = "\t\n ";

  string prev;
  string word;

  if (line.empty()) 
    throw myexception()<<"Trying to parse tree from empty string";

  vector< vector<BranchNode*> > tree_stack;
  vector< string > comments;
  int N = 0;
  push_empty_node(tree_stack, N, n_node_attributes(), n_undirected_branch_attributes(), n_directed_branch_attributes());
  int pos = 0;
    
  for(int i=0;get_word(word,i,comments,line,delimiters,whitespace);prev=word) 
  {
    vector< pair<string, any> > tags;
    add_comments(tags, comments, "&&NHX:", ":");
    add_comments(tags, comments, "&!", ",!");

    //std::cerr<<"word = '"<<word<<"'    depth = "<<tree_stack.size()<<"   stack size = "<<tree_stack.back().size()<<std::endl;

    if (word == ";")
    {
      BranchNode* BN = tree_stack.back().back()->out;
      if (pos == 0 or pos == 1 or pos == 2)
	set_attributes(tags, node_attribute_names, *BN->node_attributes);
      else if (pos == 3 or pos == 4)
	assert( tags.empty() );

      break;
    }

    //------ Process the data given the current state ------//
    if (word == "(") 
    {
      assert( tags.empty() );

      if (pos != 0)
	throw myexception()<<"In tree file, found '(' in the middle of word \""<<prev<<"\"";

      push_empty_node(tree_stack, N, n_node_attributes(), n_undirected_branch_attributes(), n_directed_branch_attributes());
      pos = 0;
    }
    else if (word == ",")
    {
      BranchNode* BN = tree_stack.back().back()->out;

      if (pos == 0 or pos == 1 or pos == 2)
	set_attributes(tags, node_attribute_names, *BN->node_attributes);
      else if (pos == 3 or pos == 4)
	set_attributes(tags, undirected_branch_attribute_names, *BN->undirected_branch_attributes);

      append_empty_node(tree_stack, N, n_node_attributes(), n_undirected_branch_attributes(), n_directed_branch_attributes());
      pos = 0;
    }
    else if (word == ")") 
    {
      BranchNode* BN = tree_stack.back().back()->out;

      if (pos == 0 or pos == 1 or pos == 2)
	set_attributes(tags, node_attribute_names, *BN->node_attributes);
      else if (pos == 3 or pos == 4)
	set_attributes(tags, undirected_branch_attribute_names, *BN->undirected_branch_attributes);

      // We need at least 2 levels of trees
      if (tree_stack.size() < 2)
	throw myexception()<<"In tree file, too many end parenthesis.";

      // destroy the top level
      tree_stack.pop_back();
      pos = 1;
    }
    else if (word == ":")
    {
      BranchNode* BN = tree_stack.back().back()->out;

      set_attributes(tags, node_attribute_names, *BN->node_attributes);

      if (pos > 2)
	throw myexception()<<"Cannot have a ':' here! (pos == "<<pos<<")";
      pos = 3;
    }
    else
    {
      BranchNode* BN = tree_stack.back().back()->out;

      if (pos == 0 or pos == 1) 
      {
	set_attributes(tags, node_attribute_names, *BN->node_attributes);

	// Set the index the node name (an integer) -- possibly to -1.
	if (is_leaf_node(BN))
	  BN->node_attributes->name = get_leaf_index(word, allow_numbers, names);
	else
	  BN->node_attributes->name = -1;

	// Set the node label, if we are in the names list and this is a leaf node.
	if (node_label_index != -1 and BN->node_attributes->name != -1)
	  (*BN->node_attributes)[node_label_index] = names[BN->node_attributes->name];

	pos = 2;
      }
      else if (pos == 2)
	throw myexception()<<"Node name '"<<word<<"' comes directly after '"<<prev<<"'";
      else if (pos == 3)
      {
	set_attributes(tags, undirected_branch_attribute_names, *BN->undirected_branch_attributes);

	if (branch_length_index != -1)
	  (*BN->undirected_branch_attributes)[branch_length_index] = convertTo<double>(word);	
	pos = 4;
      }
      else if (pos == 4)
	throw myexception()<<"Word name '"<<word<<"' comes directly after branch length '"<<prev<<"'";
    }
  }

  if (tree_stack.size() != 1)
    throw myexception()<<"Attempted to read w/o enough left parenthesis";
  if (tree_stack.back().size() != 1)
    throw myexception()<<"Multiple trees on the same line";

  BranchNode* remainder = tree_stack.back()[0];
  BranchNode* root_ = TreeView::unlink_subtree(remainder->out);

  // Handle root_ being a leaf
  //  if (::is_leaf_node(root_) and )
  //    throw myexception()<<"Tree has an unnamed leaf node at the root.  Please remove the useless branch to the root.";

  TreeView(remainder).destroy();

  // destroy old tree structure
  if (nodes_.size()) TreeView(nodes_[0]).destroy();

  // determine nodes_[]
  nodes_.clear();
  leaf_nodes_.invalidate();
  internal_nodes_.invalidate();

  // switch to new tree structure
  for(BN_iterator BN(root_);BN;BN++)
  {
    if (is_leaf_node(*BN))
      //      (*BN)->node_attributes->name = n_leaves_++;
      ;
    else
      (*BN)->node_attributes->name = -1;

    // set the number of attributes to the correct number
    (*BN)->node_attributes->resize(n_node_attributes());
    if ((*BN)->undirected_branch_attributes)
      (*BN)->undirected_branch_attributes->resize(n_undirected_branch_attributes());
    if ((*BN)->directed_branch_attributes)
      (*BN)->directed_branch_attributes->resize(n_directed_branch_attributes());
  }

  reanalyze(root_);

  return root_->node_attributes->name;
}

Tree::Tree()
  :caches_valid(false),
   n_leaves_(0),
   node_label_index(0),
   branch_length_index(0)
{
  if (node_label_index != -1 and node_label_index >= n_node_attributes())
    set_n_node_attributes(node_label_index+1);
  if (branch_length_index != -1 and branch_length_index >= n_undirected_branch_attributes())
    set_n_undirected_branch_attributes(branch_length_index+1);
}

Tree::Tree(BranchNode* BN) 
  :caches_valid(false),
   node_label_index(0),
   branch_length_index(0)
{
  if (BN->undirected_branch_attributes)
    undirected_branch_attribute_names.resize( BN->undirected_branch_attributes->size() );

  if (BN->directed_branch_attributes)
    directed_branch_attribute_names.resize( BN->directed_branch_attributes->size() );

  if (node_label_index != -1 and node_label_index >= n_node_attributes())
    set_n_node_attributes(node_label_index+1);
  if (branch_length_index != -1 and branch_length_index >= n_undirected_branch_attributes())
    set_n_undirected_branch_attributes(branch_length_index+1);

  reanalyze(BN);
}

Tree::Tree(const Tree& T) 
    :caches_valid(T.caches_valid),
     cached_partitions(T.cached_partitions),
     n_leaves_(T.n_leaves_),
     node_label_index(T.node_label_index),
     branch_length_index(T.branch_length_index),
     node_attribute_names(T.node_attribute_names),
     undirected_branch_attribute_names(T.undirected_branch_attribute_names),
     directed_branch_attribute_names(T.directed_branch_attribute_names),
     nodes_(T.nodes_.size(),(BranchNode*)NULL),
     branches_(T.branches_.size(),(BranchNode*)NULL)
{
    // recalculate pointer indices
    BranchNode* start = T.copy();
    recompute(start,false);
    set_equivalent_node_pointers(T.nodes_);
}

Tree::~Tree() 
{
  for(int i=0;i<branches_.size();i++)
    delete branches_[i];
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
  if (root_->node_attributes->name == node)
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
    while(not subtree_contains(BN->directed_branch_attributes->name,i))
      BN = BN->next;

    assert(subtree_contains(BN->directed_branch_attributes->name,j));
  } while(subtree_contains(BN->directed_branch_attributes->name,j));

  return BN->node_attributes->name;
}

void RootedTree::add_first_node() {
  Tree::add_first_node();
  root_ = nodes_[0];
}

RootedTree& RootedTree::operator=(const RootedTree& RT) {

  Tree::operator=(RT);

  root_ = nodes_[RT.root_->node_attributes->name];

  return *this;
}

int RootedTree::parse_and_discover_names(const string& s)
{
  int r = Tree::parse_and_discover_names(s);

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
  return write(T.root(), names, T.node_attribute_names, T.undirected_branch_attribute_names, print_lengths);
}

string write(const Tree& T, const vector<string>& names, bool print_lengths) 
{
  return write(T.directed_branch(0).target(), names, T.node_attribute_names, T.undirected_branch_attribute_names, print_lengths);
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
  :Tree(RT),root_(nodes_[RT.root_->node_attributes->name])
{ }

RootedTree::RootedTree(const RootedTree& t1, const RootedTree& t2) 
  :Tree(t1),root_(nodes_[t1.root_->node_attributes->name])
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
  BranchNode* center = get_first_node(1);

  if (n == 1)
    ;
  else if (n == 2)
  {
    center->node_attributes->name = 0;
    add_leaf_node(center,1,0)->node_attributes->name = 1;
  }
  else
    for(int i=0;i<n;i++)
      add_leaf_node(center,1,0)->node_attributes->name = i;

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


double tree_length(const Tree& T) {
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
    int d = T.node(i).degree();
    if (d != 1 and d != 3)
      return false;
  }
  return true;
}

bool has_sub_branches(const Tree& T)
{
  for(int i=0;i<T.n_nodes();i++)
  {
    if (T.node(i).degree() == 2)
      return true;
  }
  return false;
}

bool has_polytomy(const Tree& T)
{
  for(int i=0;i<T.n_nodes();i++)
  {
    if (T.node(i).degree() > 3)
      return true;
  }
  return false;
}

bool same_topology_and_node_and_branch_numbers(const Tree& T1, const Tree& T2)
{
  if (T1.n_nodes() != T2.n_nodes()) return false;

  if (T1.n_branches() != T2.n_branches()) std::abort();

  for(int i=0;i<T1.n_branches();i++)
  {
    if (T1.branch(i).source().name() != T2.branch(i).source().name()) return false;

    if (T1.branch(i).target().name() != T2.branch(i).target().name()) return false;
  }

  return true;
}
